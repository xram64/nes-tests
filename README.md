# Players
Interactive ROMs that can play notes and change settings for each channel. Modified from the [Nerdy Nights Sound: Part 3 tutorial code](http://nintendoage.com/forum/messageview.cfm?catid=22&threadid=22776).

## NES_APUTest_NoisePlayer.nes
- Left/Right: Select noise frequency. RNG mode will switch when frequency wraps around from 0 or 15.
- Up: Play noise.
- Down: Stop noise.
- A: Toggle length counter (LC) bit. This is bit 5 of the volume register. 0 = Enabled, 1 = Disabled.
- B: Toggle envelope decay (ED) bit. This is bit 4 of the volume register. 0 = Enabled, 1 = Disabled.

## NES_APUTest_SquarePlayer.nes
- Left/Right: Select note.
- Up: Play note.
- Down: Stop note.
- A: Toggle length counter (LC) bit. This is bit 5 of the volume register. 0 = Enabled, 1 = Disabled.
- B: Toggle envelope decay (ED) bit. This is bit 4 of the volume register. 0 = Enabled, 1 = Disabled.

*Note: Only the first square channel (SQ1) is used in this test.*

# Loops
Simple ROMs that loop through sounds on one channel.

## NES_APUTest_Square1.nes
Plays a melody (CEGAFD) on the square 1 channel (SQ1) with 50% duty cycle (10). The melody plays twice at each octave, starting at C2 and ending at C7, repeating infinitely.

## NES_APUTest_Square2.nes
Plays a melody (CEGAFD) on the square 2 channel (SQ2) with 25% duty cycle (01). The melody plays twice at each octave, starting at C2 and ending at C7, repeating infinitely.

## NES_APUTest_Triangle.nes
Plays a melody (CEGAFD) on the triangle channel (TRI). The melody plays twice at each octave, starting at C3 and ending at C7, repeating infinitely.

## NES_APUTest_Noise.nes
Generates each noise frequency (0-15) one at a time, first using RNG mode 0, then again using RNG mode 1.
