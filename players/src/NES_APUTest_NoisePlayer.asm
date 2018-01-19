    .inesprg 1 ;1x 16kb PRG code
    .ineschr 1 ;1x 8kb CHR data
    .inesmap 0 ; mapper 0 = NROM, no bank swapping
    .inesmir 1 ;background mirroring (vertical mirroring = horizontal scrolling)

    .rsset $0000
joypad1 .rs 1           ;button states for the current frame
joypad1_old .rs 1       ;last frame's button states
joypad1_pressed .rs 1   ;current frame's off_to_on transitions
current_noise .rs 1     ;used to index into our note_table
noi_volumereg .rs 1		;used to update volume register of noise channel
noi_lencountbit .rs 1	;state of bit 5 of the noise volume register ($400C)
noi_envdecaybit .rs 1	;state of bit 4 of the noise volume register ($400C)
sleeping .rs 1          ;main program sets this and waits for the NMI to clear it.  Ensures the main program is run only once per frame.  
                        ;   for more information, see Disch's document: URL HERE
ptr1 .rs 2              ;a pointer
y_mod .rs 1				;used for mod function
;----- first 8k bank of PRG-ROM    
    .bank 0
    .org $C000
    
irq:
NMI:
    pha ;backup registers
    txa
    pha
    tya
    pha
    
	; draw text to screen
    jsr draw_rngtext
	jsr draw_freqtext
	jsr draw_bitstext
    
    lda #$00        ;set scroll
    sta $2005
    sta $2005
    
    lda #$00
    sta sleeping    ;wake up the main program
    
    pla ;restore registers
    tay
    pla
    tax
    pla
    rti

RESET:
    sei
    cld
    ldx #$FF
    txs
    inx
    
vblankwait1:
    bit $2002
    bpl vblankwait1
    
clearmem:
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$FE
    sta $0200, x
    inx
    bne clearmem
    
 vblankwait2:
    bit $2002
    bpl vblankwait2
    
;set a couple palette colors.  This demo only uses two
    lda $2002   ;reset PPU HI/LO latch
    
    lda #$3F
    sta $2006
    lda #$00
    sta $2006   ;palette data starts at $3F00
    
    lda #$0F    ;black
    sta $2007
    lda #$30    ;white
    sta $2007
    
;Enable sound channels
    lda #%00001000
    sta $4015 ;enable Noise
    
    lda #$00
    sta current_noise ;start with first noise
	
	lda #$01
    sta noi_lencountbit ; start with LC disabled
	sta noi_envdecaybit ; start with ED disabled
	
    lda #$88
    sta $2000   ;enable NMIs
    lda #$18
    sta $2001   ;turn PPU on
    
;main program starts here   
forever:
    inc sleeping ;go to sleep (wait for NMI).
.loop:
    lda sleeping
    bne .loop ;wait for NMI to clear the sleeping flag and wake us up
    
    ;when NMI wakes us up, handle input and go back to sleep
    jsr read_joypad
    jsr handle_input
    jmp forever ;go back to sleep
    
;----------------------------
; read_joypad will capture the current button state and store it in joypad1.  
;       Off-to-on transitions will be stored in joypad1_pressed
read_joypad:
    lda joypad1
    sta joypad1_old ;save last frame's joypad button states
    
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    
    ldx #$08
.loop:    
    lda $4016
    lsr a
    rol joypad1  ;A, B, select, start, up, down, left, right
    dex
    bne .loop
    
    lda joypad1_old 	;what was pressed last frame.
    eor #%11111111 		;EOR to flip all the bits to find what was not pressed last frame
	;;;eor #%11110111
	; we ignore up button here. we want to let up button continue to retrigger noise sample, otherwise it will only play once per btn press.
    and joypad1 		;what is pressed this frame
    sta joypad1_pressed ;stores off-to-on transitions
	
    
    rts
    
;---------------------
; handle_input will perform actions based on input:
;   up - play current note
;   down - stop playing the note
;   left - cycle down a note
;   right - cycle up a note
handle_input:
    lda joypad1_pressed
    and #%11001111 ;check d-pad and a, b only
    beq .done
.check_up:
    and #$08 ;up
    beq .check_down
    jsr play_note
.check_down:
    lda joypad1_pressed
    and #$04 ;down
    beq .check_left
    jsr silence_note
.check_left:
    lda joypad1_pressed
    and #$02 ;left
    beq .check_right
    jsr note_down
.check_right:
    lda joypad1_pressed
    and #$01 ;right
    beq .check_a
    jsr note_up    
.check_a:
    lda joypad1_pressed
    and #$80 ;a
    beq .check_b
	
    lda noi_lencountbit  ; flip bit
	eor #$01
	sta noi_lencountbit
	
.check_b:
    lda joypad1_pressed
    and #$40 ;b
    beq .done
	
    lda noi_envdecaybit  ; flip bit
	eor #$01
	sta noi_envdecaybit
	
.done:
    rts
    
;----------------------
; play_note plays the noise stored in current_noise
play_note:
    lda #%00001111    	; full volume
	sta noi_volumereg 	; reset volume register
	
	lda noi_lencountbit ; check if we should set length counter
	asl a
	asl a
	asl a
	asl a
	asl a  ; shift value in variable left 5 times to match it to bit 5
	ora noi_volumereg  ; OR it with our volume to add the 1 if it's there
	sta noi_volumereg
	
	lda noi_envdecaybit ; check if we should set envelope decay
	asl a
	asl a
	asl a
	asl a  ; shift value in variable left 4 times to match it to bit 4
	ora noi_volumereg  ; OR it with our volume to add the 1 if it's there
	sta noi_volumereg
	
    sta $400C  ; write final volume byte to register
	
    
    lda current_noise
    asl a               ;multiply by 2 because we are indexing into a table of words
    tay
    lda noise_values, y   ;read the low byte of the "period"
    sta $400E             ;write to "NOI_LO"
    lda noise_values+1, y ;read the high byte of the "period"
    sta $400F             ;write to "NOI_HI" (notes in table are made to fit noise registers)
    rts
    
;--------------------
; silence_note silences the noise channel
silence_note:
    lda #$30
    sta $400C   ;silence Noise by setting the volume to 0.
    rts
    
;--------------------
; note_down will move current_noise down.  Lowest noise will wrap to highest noise
note_down:
    dec current_noise
    lda current_noise
    cmp #$FF
    bne .done
    lda #$1F    ;highest noise.  We wrapped from 0
    sta current_noise
.done:
    rts
    
;----------------------
; note_up will move current_noise up.  Highest noise will wrap to lowest noise   
note_up:
    inc current_noise
    lda current_noise
    cmp #$20            ;did we move past the highest noise value?
    bne .done           ;if not, no problem
    lda #$00            ;but if we did, wrap around to 0 (the lowest noise)
    sta current_noise
.done:
    rts
    

;-------------
; draw_rngtext will draw the rng mode
;   this subroutine writes to the PPU registers, so it should only be run during vblank (ie, in NMI)
draw_rngtext:
    lda $2002
    lda #$21
    sta $2006
    lda #$4A
    sta $2006   ;$214D is a nice place in the middle of the screen to draw
    
	
    lda current_noise  ;use current_noise (0-31) as an index into our pointer table
	cmp #$10
	bcc .rng0
	lda #$01  ; if current_noise >= 16, set rng mode to 1
	jmp .rng1
.rng0:
    lda #$00  ; set rng mode to 0 as default
.rng1:
	
    asl a           ;multiply by 2 because we are indexing into a table of pointers (which are words)
	tay
	
    lda text_rng_pointers, y    ;setup pointer to the text data
    sta ptr1
    lda text_rng_pointers+1, y
    sta ptr1+1
    ldy #$00
.loop:
    lda [ptr1], y   ;read a byte from the string
    bmi .end    ;if negative, we are finished (our strings are terminated by $FF, a negative number)
    sta $2007   ;else draw on the screen
    iny
    jmp .loop
.end:
    rts
    
	
;-------------
; draw_freqtext will draw the frequency of the noise
;   this subroutine writes to the PPU registers, so it should only be run during vblank (ie, in NMI)
draw_freqtext:
    lda $2002
    lda #$21
    sta $2006
    lda #$8A
    sta $2006   ;$214D is a nice place in the middle of the screen to draw
    

    lda current_noise   ;use current_noise (0-31) as an index to freq. text
	ldy #$10
    jsr mod				; mod by 16 to get a number in 0-15 (in case we're in rng mode 1)
	
    asl a           ;multiply by 2 because we are indexing into a table of pointers (which are words)
	tay
	
    lda text_freq_pointers, y    ;setup pointer to the text data
    sta ptr1
    lda text_freq_pointers+1, y
    sta ptr1+1
    ldy #$00
.loop:
    lda [ptr1], y   ;read a byte from the string
    bmi .end    ;if negative, we are finished (our strings are terminated by $FF, a negative number)
    sta $2007   ;else draw on the screen
    iny
    jmp .loop
.end:
    rts
	
	
;-------------
; draw_bitstext will draw the states of the length counter and env decay bits (4 and 5)
;   this subroutine writes to the PPU registers, so it should only be run during vblank (ie, in NMI)
draw_bitstext:
; Draw bit 5 text
    lda $2002
    lda #$22
    sta $2006
    lda #$0A
    sta $2006   ;$214D is a nice place in the middle of the screen to draw
    
    lda noi_lencountbit  ;use noi_lencountbit (0/1) as an index into our pointer table
	
    asl a           ;multiply by 2 because we are indexing into a table of pointers (which are words)
	tay
    lda text_bit5_pointers, y    ;setup pointer to the text data
    sta ptr1
    lda text_bit5_pointers+1, y
    sta ptr1+1
    ldy #$00
.loopA:
    lda [ptr1], y   ;read a byte from the string
    bmi .endA    ;if negative, we are finished (our strings are terminated by $FF, a negative number)
    sta $2007   ;else draw on the screen
    iny
    jmp .loopA
.endA:

; Draw bit 4 text
    lda $2002
    lda #$22
    sta $2006
    lda #$4A
    sta $2006   ;$214D is a nice place in the middle of the screen to draw
	
    lda noi_envdecaybit  ;use noi_envdecaybit (0/1) as an index into our pointer table
	
    asl a           ;multiply by 2 because we are indexing into a table of pointers (which are words)
	tay
    lda text_bit4_pointers, y    ;setup pointer to the text data
    sta ptr1
    lda text_bit4_pointers+1, y
    sta ptr1+1
    ldy #$00
.loopB:
    lda [ptr1], y   ;read a byte from the string
    bmi .endB    ;if negative, we are finished (our strings are terminated by $FF, a negative number)
    sta $2007   ;else draw on the screen
    iny
    jmp .loopB
.endB:



    rts
	
	
; Modulo operator.
; Load A and Y prior to calling. Returns A%Y in A.
mod:
    SEC				; set carry (C=1) to clear borrow
    STY y_mod 		; store Y in memory address y_mod
modloop:
    SBC y_mod		; subtract A - Y
    BCS modloop		; loops if subtraction DID NOT produce a borrow (C=1)
    ADC y_mod		; add Y back to A to get last positive modulus
    RTS
    
;----- second 8k bank of PRG-ROM    
    .bank 1
    .org $E000
    
    .include "se_note_table.i" ;our NTSC note lookup table

noise_values:
    ; Noise frequencies, RNG Mode 0 ($5F - $6E)
    .dw $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F
    ; Noise frequencies, RNG Mode 1 ($6F - $7E)
    .dw $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F
  
	
;this is a table of pointers.  These pointers point to the beginning of text strings.
text_rng_pointers:
    .word text_RNGMode0, text_RNGMode1
text_freq_pointers:
    .word (text_Freq+(9*0)), (text_Freq+(9*1)), (text_Freq+(9*2)), (text_Freq+(9*3))
	.word (text_Freq+(9*4)), (text_Freq+(9*5)), (text_Freq+(9*6)), (text_Freq+(9*7))
	.word (text_Freq+(9*8)), (text_Freq+(9*9)), (text_Freq+(9*10)), (text_Freq+(9*11))
	.word (text_Freq+(9*12)), (text_Freq+(9*13)), (text_Freq+(9*14)), (text_Freq+(9*15))
	
text_bit5_pointers:
    .word text_LC_0, text_LC_1
text_bit4_pointers:
    .word text_ED_0, text_ED_1
	
;CHR     
;   $00 = blank
;   $0A = "#"
;   $10-$16 = "A"- "G"
text_Freq:  ; start of frequency text strings (Freq. 0-15)
    .byte $15, $21, $14, $20, $0F, $00, $00, $1E, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $01, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $02, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $03, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $04, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $05, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $06, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $07, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $08, $FF
    .byte $15, $21, $14, $20, $0F, $00, $00, $09, $FF
    .byte $15, $21, $14, $20, $0F, $00, $01, $1E, $FF
    .byte $15, $21, $14, $20, $0F, $00, $01, $01, $FF
    .byte $15, $21, $14, $20, $0F, $00, $01, $02, $FF
    .byte $15, $21, $14, $20, $0F, $00, $01, $03, $FF
    .byte $15, $21, $14, $20, $0F, $00, $01, $04, $FF
    .byte $15, $21, $14, $20, $0F, $00, $01, $05, $FF


text_RNGMode0:
    .byte $21, $1D, $16, $00, $1C, $1E, $13, $14, $0D, $00, $1E, $FF
text_RNGMode1:
    .byte $21, $1D, $16, $00, $1C, $1E, $13, $14, $0D, $00, $01, $FF
    
text_LC_0:
    .byte $1B, $12, $0D, $00, $1E, $FF
text_LC_1:
    .byte $1B, $12, $0D, $00, $01, $FF
text_ED_0:
    .byte $14, $13, $0D, $00, $1E, $FF
text_ED_1:
	.byte $14, $13, $0D, $00, $01, $FF
	
	
;---- vectors
    .org $FFFA     ;first of the three vectors starts here
    .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
    .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
    .dw irq        ;external interrupt IRQ is not used in this tutorial
    
;------ 8k chr bank
    .bank 2
    .org $0000
    .incbin "NES_APUTest.chr"