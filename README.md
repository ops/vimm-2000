

Copyright © 2000 Pasi Ojala, Anders Carlsson


The fastloader used is based on the fastloader © 1998 Marko Mäkelä.
(As found in the "Veni Vidi Vic!" demo in ftp.funet.fi:/pub/cbm/)

All music composed and/or arranged by Anders Carlsson. The music
routine written by Anders Carlsson, based on Asger Alstrup's routine
from "Veni Vidi VIC!" and Adam Bergström's in VICPLAY (unreleased).

All other code by Pasi Ojala. http://www.cs.tut.fi/~albert/


    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.



Vic(i) Iterum MM     - Victory Again 2000
================	Autodetecting PAL/NTSC version for 16k mem expansion
			Supports 1540/41,1570/71,1581

Serious note: run this demo on a real VIC20.

Writing this VIC20 demo started a long, long, long time ago, in a galax..
Ahem. A friend gave me a VIC20 around 1989, and I played with it a bit,
converting some C64 fonts to VIC's 16x8 font system. Nothing really
became of it though, because I didn't have time or great motivation
to really study the operation of the chips inside the VIC20. I just
continued to write C64 demos.

Quite a many years later, working with Marko Mäkelä rekindled my interest
in VIC20, and I started to convert some of my C64 effects to VIC20.
Meanwhile, Marko spread the VIC20 'awareness' and programming information
and created the "Veni Vidi Vic!" demo.  Although impressive on its own
right, I already had a VIC20 version of my "trademark", the copper
scroller, and were in the process of creating more parts to surpass that
first and only VIC20 megademo.

As so many times before and since, other interests (programming or
otherwise) also required their slices of my free time, but finally I just
decided that this year is it, and started to think about a name.  The latin
theme Marko (and the Commodore marketing) started seemed a good idea, and
also including the year created the name:  "Vici Iterum MM", freely
translated as "Won again, 2000", or maybe "VIC again, 2000", or combined
"VIC won again, 2000".

The four or so parts I had sort of ready when the year 2000 started still
required tuning, linking, music and additional parts. All other I could
handle, but music was the greatest catch-22.  Fortunately Anders Carlsson
answered my call for help (twice even:-) and I could count on him to
provide me with a tune or two.  The rest was just a matter of thinking up
more demo ideas (of course some of them are left for future productions),
figuring out the algorithms, programming the code, creating and converting
the graphics, writing scrolltexts, adding fadein and fadeout effects where
appropriate, compiling, compressing with pucrunch (see
http://www.cs.tut.fi/~albert/Dev/ -> pucrunch) and testing..  :-)

The following goes through the parts in the order they are shown in the
demo.  Note that the explanations concern the PAL versions of the parts.
In NTSC versions part of the effects may be left out, or downsized, like
some of the stuff on screen (PAL has a larger viewable screen size).
Still, all of the parts have their essence there in both PAL and NTSC
versions, mainly because I did not NTSC-fix them, but simply concurrently
developed both PAL and NTSC versions: both are compiled from the same source.

Some of the parts would have totally different implementations, if they
were written only for NTSC machines, but I didn't have time for that.  Btw,
did you know that the video matrix can be as large as 1024 bytes?  Although
that doesn't always help, because then you can't double-buffer the color
memory if you use it all.

Okay, enough of introduction, just load vimm.prg, run it and watch the
demo and read the part-by-part descriptions.

Other stuff:
- the first part checks for 16k expansion
- runstop skips to the next part in both manual and automatic mode
- PAL/NTSC is detected automatically and the right version loaded
- you can also load and run the parts separately, just load the right
  version (.p for PAL, .n for NTSC). You don't get music in this case.
- source code will be available (someday)
- I think emulator writers will have their hands full of work after
  this demo is released :-) Horizontal screen positioning at least
  needs to be implemented..


1. VIMM - simple upscroller
---------------------------

I always wanted to create an upscroller part when I was writing demos for
C64, but it took until 1998 before I finally implemented one for the
"Roots" megademo for Zoo'98.  In that demo the upscroller was the last part
giving the demo credits.

VIMM (Vici Iterum MM) needed a loader part, which would also give some
facts about the system and the operating instructions (pressing X or
joystick button selects manual mode). To be as clear as possible,
the part would need quite a large font, and an upscroller was the
perfect choice. I also had a font idea, and that font was originally
used for this starting part and later also for the last part.

However, using the same font for two parts seemed lame, and I tried another
font idea to see how it would work. I spent a couple of hours beside my
A3000 drawing, which showed that the idea would work fantastically.  A lot
of tinkering later you see the result.  A little more resolution would've
helped, but the end result isn't half bad.

This starting part also detects the video system and installs the first
music and the 154x/157x/1581 fastloader, which is based on Marko Mäkelä's
fastloader.

Other stuff:
- checks that you have at least 16k memory expansion
- installs fastloader, 154x/157x and 1581 are supported
- detects the video mode PAL/NTSC
- the same code runs in both PAL and NTSC
- proportional font and automatic text centering code



2. VICPIC - FLI-type graphics mode
----------------------------------

VIC-I (the video chip in VIC20) does not have a bitmapped mode, thus all
graphics must be created with user-defined character sets.  To allow more
screen area to be covered, VIC-I has a 16x8-character mode, in which the
normal character height is doubled from 8 bytes to 16 bytes.  Because there
is no bitmap mode, in multicolor mode three colors are common to the whole
screen area and the fourth is read from the color memory.

I have played with C64 graphics converters in the past and decided to
create a raster-assisted graphics mode (not quite FLI) for VIC20.  Using a
graphics converter would not require a drawing program (and I'm a poor
graphician anyway), and the converter can perform dithering and as clever
color selection as I can manage to program.

I tried different aspect ratios, but 100x160 (width x height) pixels seemed
the best for several reasons:
 o 25x10 = 250 characters, which only requires one page for video matrix
   (actually two identical, because the color memory is "interlaced" to
    increase color resolution)
 o both PAL and NTSC viewers can be implemented
 o larger picture doesn't fit in unexpanded VIC20 anyway
 o and it simply is a nice aspect ratio :-)

The color memory resolution was increased from 16-line cells to 8-line cells
by duplicating the video matrix and multiplexing the two color memories.

Some of the pictures I had converted seemed quite cool, thus creating a
slideshow part for this "ultimate" VIC20 megademo was a must.

Other stuff:
- files named vimmgfx0..9a.. are loaded in order until loading fails
- both uncompressed and pucrunch-compressed (-c0) picture files are handled
- if the loaded file is music data instead of gfx, the tune is changed
- delay was added between pictures or with 1581 they are shown too fast
- see http://www.cs.tut.fi/~albert/Dev/ -> VicPic for more info



3. TECH^2 - Tech-tech and DYCP in one
-------------------------------------

Two simple effects, mainly to see how they are done in VIC20.

VIC-I doesn't have a smooth-scroll register, but the screen can be shifted
in half-character (one clock cycle) increments.  Because of this the
tech-tech effect fine-adjust must be done using character sets and the
coarse-adjust with screen positioning, just the opposite way than in C64.

DYCP can be implemented just like in C64, the 16x8-character mode does not
help in this case.  Double-buffering is needed and is used.  There was just
enough free time to add the color cycling, although double-buffering the
color memory was not possible (sorry). You can notice the lack of color
memory double-buffering when you set the sin(up-down) speed high enough.

I had some of the character codes and part of the character set memory
free, so I decided to add a little pacman-bonus to this screen.



4. PLASMA - a multicolor plasma w/ 13 colors
--------------------------------------------

The plasma part was one of the latest additions to this demo.
"Veni Vidi Vic!" had a black'n'white plasma effect, and I had created a
sprite plasma effect for "Roots" (just one of my crazy ideas), so why not
make a better one for VIMM?

VIC-I color restrictions really are restrictions.  Two of the screen colors
can only be colors 0-7, while the other two can be any of the sixteen
colors.  Not using half of the colors seemed so uncool, especially because
orange and light orange create one more color scale.  With careful planning
the plasma effect actually uses 13 of the 16 possible colors, and the
reason why not all of them are used is simply that the remaining 3 colors
just didn't fit in the color scheme..

The more or less smooth scrolling needs some raster trickery to make it
appear that the whole screen area is covered.  Another sleigh of a hand
tries to convince you that we are in 8x8-character mode, although for that
you would need a 1008-byte video matrix (28 columns, 288 raster lines in
PAL).


Other stuff:
- Press joystick button to stop the color cycling
- Very little CPU is actually used, the next part is loaded very quickly



5. COPPER - Copper scroller, "curtain" display, "sprite" plotter
----------------------------------------------------------------

One of my specialties in C64 demos besides sprite stretching is copper
effects, where the screen could be blanked and all that you could see was
created by changing just the border color synchronized to the screen
refresh.  Knowing this you should not be surprised when you see this part.
The top scroller is created by changing the background color register with
the processor while the VIC-I chip is drawing it.  For VIC20 version I also
added a character overlay to make the font clearer.

The middle of the screen implements my idea of a curtain scroller, where
the text is hidden and/or reappears with differend 'fade' effects. If you
are familiar with VIC-I and/or raster bars, you may notice that in fact
creating that effect is not so easy after all.. If you don't know why,
the effect may still look cool :-)

Using double-buffered copper code left some raster time still unused so
I decided to try implementing my first sprite plotter for VIC20. The
result may not be so impressive, but it fills the bottom of the screen
nicely.. The NTSC version only has two sprites, because clearing the
plot area takes so much time.



6. SHEKKI - Double checkerboard and scroller
--------------------------------------------

"Veni Vidi Vic!" had a chessboard zoomer and Marko Mäkelä suggested that
I make a version with two chessboards zooming independently. The result
was quite good, but a good idea can sometimes be made better.

Anyone can think of how the boards are implemented (multicolor mode,
character set plotter, and raster routine), but some tricks were still
unused. The letters PU239 are flashed on top of the zoomer and like that
weren't enough, the Pu-239 logo appears and disappears after a while.

Still I had the feeling that something else can be added to make this part
even better. When I saw a checkered flag waved on TV, I knew what I had to
try: wave it. After hard and long code optimization I managed to make one
of the boards wave both horizontally and vertically, while the other board
waves only vertically. (And not even that in the NTSC version.)

Taking into account that the horizontal resolution is one multicolor pixel,
and the vertical resolution is three raster-lines, the wave effect seems
even cooler. Integrators/accumulators rule -- real-time dithering!


Other stuff:
- you can control the boards with joystick by keeping the button pressed
  and wiggling it :-)
- the bottom scroller uses my own font -- it is mine, mine, mine!:-)



7. CREDIZ - Overlapping scrolls with a twist
--------------------------------------------

Again an idea which originated from "Veni Vidi Vic!", but only indirectly.
One of the first parts in that demo has a flag zoomer, which shows the
nationalities of the demo programmers/artists.  My original idea was to
have two scrolls in the cross formation and the border and backgrounds
could be flashed to reveal the Finnish and Swedish flags (for me and
Anders).

This required that the scrolls overlap and I first had to implement that.
The flag color idea was dismissed because it turned out to be impossible
because of the way I implemented the overlapping scrolls (and I wanted to
use three colors for the font), but other ideas replaced that.

This part performs a lot of graphics plotting, but only as much as needed.
On the other hand, plotting the overlap is slower than necessary because
what you see as the black background is actually the border color, which
has the bit pattern 01 instead of 00.  Fiddling the bits around to produce
the correct overlapping takes almost 40% of the plotting time.  This was
necessary because the screen is in fact shaped as a plus sign.

Because the vertical scroller is implemented by moving the screen (both
vertically and horizontally), the horizontal scroller must always be
plotted to compensate.  This allows the effects you see in the horizontal
scroller for little extra effort.  Some of the effects remind us of sprite
stretching. Two source images are used to implement half-cycle (one
multicolor pixel) scrolling speed for the horizontal scroller.

Two fonts, the whole character set being used for graphics, two buffers
for the horizontal scroller data, some tables, and the actual code makes
it a tight fit. But there was some space left and I put in another pacman
bonus..


Other stuff:
- in NTSC the rasterline changes in the middle of the screen, that is
  why the vertical scroller does not cross the middle part of the screen
  like in the PAL version. (It took a long time to find this 'bug'.)
- run/stop, left shift, 'x', 'v', 'n', ',', '/' can trigger the effects
- the horizontal scroller only moves 1 multicolor pixel per frame,
  a faster scroller would be easier and would also take less memory
- two fonts are needed (separate for horizontal and vertical),
  although the design is the same




Source Code
-----------
The source code is provided for the interested parties to encourage
the development of new and innovative software for VIC20, whether it
is demos, games, or application software.

The source code is also provided in hope that more complete VIC20
emulators will be developed. When all of the demo parts are perfectly
emulated, it will be pretty close.

