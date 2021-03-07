
; Copyright © 2000 Pasi Ojala

  processor 6502

; $1000-1200	video matrix
; $1200-1400	*code
; $1400-2000	*charfont
; $2000-	*code


TOPPOS = 14

PAL_LINES = 312
PAL_CYCLES_PER_LINE = 71
PAL_SCRCENTER = 34

NTSC_LINES = 261
NTSC_CYCLES_PER_LINE = 65
NTSC_SCRCENTER = 26

PAL_TIMER_VALUE = PAL_LINES * PAL_CYCLES_PER_LINE - 2
NTSC_TIMER_VALUE = NTSC_LINES * NTSC_CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

PAL_RASTER = PAL_LINES/2-6
NTSC_RASTER = NTSC_LINES/2-6


; The BASIC line - Note: it is overwritten by the video matrix quite quickly

	.org $1201	; for the expanded Vic-20
basic:
	.word 0$	; link to next line
	.word 1997	; line number
	.byte $9E	; SYS token

; SYS digits

	.if (start) / 10000
	.byte $30 + (start) / 10000
	.endif
	.if (start) / 1000
	.byte $30 + (start) % 10000 / 1000
	.endif
	.if (start) / 100
	.byte $30 + (start) % 1000 / 100
	.endif
	.if (start) / 10
	.byte $30 + (start) % 100 / 10
	.endif
	.byte $30 + (start) % 10
0$	.byte 0,0,0	; end of BASIC program

	.org $1210

COLUMNS = 3*8	; 24 of 29/26
PAL_ROWS = 20
NTSC_ROWS = 17

tPtr  = $fb


COL0 = 0	; 0  0  0  0
COL1 = 5	; 2  6  8  4
COL2 = 13	;10  3  9 12
COL3 = 1	; 1  1  1  1

COLO1:
	dc.b 8*16, 4*16, 2*16, 5*16, 6*16
COLO2:
	dc.b 9*16+ 8,12*16+ 8,10*16+ 8,13*16+ 8, 3*16+ 8

inittext:
	lda #<text
	sta tPtr+0
	lda #>text
	sta tPtr+1
chtextcol:
0$	ldy #0
	lda $900e
	and #$0f
	ora COLO1,y
	sta $900e
	lda COLO2,y
	sta $900f
	iny
	cpy #5
	bne 1$
	ldy #0
1$	sty 0$+1
	rts


puttextline:
	ldy #0		; calculate line length
	ldx #0
0$	lda (tPtr),y
	iny
	cmp #13
	beq 2$
	cmp #"I"
	beq 1$
	cmp #" "
	beq 1$
	inx
1$	inx
	inx
	bne 0$		; jump always
2$	stx 3$+1
	lda #COLUMNS
	sec
3$	sbc #0
	lsr
	clc
paladd	adc #0
	tax		; start column

4$	ldy #0		; plot line
	lda (tPtr),y
	inc tPtr+0
	bne 5$
	inc tPtr+1
5$	cmp #13
	bne 6$

	lda (tPtr),y
	beq inittext
	rts
6$	cmp #" "
	beq 7$		; space, no need to update anything
	and #31
	tay
	lda sixtimes,y
	clc
	sta $1000+NTSC_ROWS*COLUMNS-2*COLUMNS+0,x
	adc #1
	sta $1000+NTSC_ROWS*COLUMNS-1*COLUMNS+0,x
	adc #1
	sta $1000+NTSC_ROWS*COLUMNS-2*COLUMNS+1,x
	adc #1
	sta $1000+NTSC_ROWS*COLUMNS-1*COLUMNS+1,x
	cpy #9	; "I"
	beq 7$
	cpy #0	; " "
	beq 7$
	clc
	adc #1
	sta $1000+NTSC_ROWS*COLUMNS-2*COLUMNS+2,x
	adc #1
	sta $1000+NTSC_ROWS*COLUMNS-1*COLUMNS+2,x
	inx
7$	inx
	inx
	bne 4$		; jump always

sixtimes:
	dc.b $00,$06,$0c,$12,$18,$1e,$24,$2a, $30,$36,$3c,$42,$48,$4e,$54,$5a
	dc.b $60,$66,$6c,$72,$78,$7e,$84,$8a, $90,$96,$9c,$a2,$a8,$ae,$b4,$ba

setcolmem:
	ldx #0
1$	lda #0
	sta $1000,x
	sta $1100,x
	lda #8+COL3
	sta $9400,x
	sta $9500,x
	dex
	bne 1$

	lda #TOPPOS
	sta $9001	; vertical centering

	ldx #NTSC_SCRCENTER-COLUMNS	; centered
	lda SYSTEMSEL
	beq 0$
	ldx #PAL_SCRCENTER-COLUMNS	; centered
0$	stx $9000	; horizontal centering
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory
	ldx #NTSC_ROWS*2+1	; rows & 16x8 chars
	lda SYSTEMSEL
	beq 2$
	ldx #PAL_ROWS*2+1	; rows & 16x8 chars
2$	stx $9003
	lda #$cd	; matrix at $1000, chars at $1400..
	sta $9005
	lda $900e
	and #$0f
	ora #COL1*16	; brown auxcol
	sta $900e
	lda #COL2*16+8+COL0	; light brown background, black border
	sta $900f
	rts


	org $1400

	incbin "bigfont.bin"

	org $2000

start	lda #$aa
	sta $5fff
	cmp $5fff
	beq ok0$
	jmp noMem
ok0$	lda #$55
	sta $5fff
	cmp $5fff
	beq ok1$
	jmp noMem
ok1$
	jsr init	; 'install' fastloader

	lda #0
	sta KEYWAIT+1
	lda #1
	sta AUTO

	sei
	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

	; detect the system
	lda #0
	sta SYSTEMSEL
	sta paladd+1
0$	lda $9004
	cmp #1
	bne 0$		; wait for line 1
1$	lda $9004
	beq 2$		; line 0 reached -> NTSC
	cmp #NTSC_LINES/2+2
	bne 1$
	inc SYSTEMSEL
	lda #PAL_ROWS*COLUMNS-NTSC_ROWS*COLUMNS
	sta paladd+1
2$	; system detected: 0 for NTSC, 1 for PAL

	jsr setcolmem
	jsr inittext
	jsr player_init
	lda #<$5400	;player_update
	sta MUSPTR+1
	lda #>$5400	;player_update
	sta MUSPTR+2

;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #0		; disable Timer A free run
	sta $912b

	ldx #NTSC_RASTER+1	; wait for this raster line (times 2)
	lda SYSTEMSEL
	beq wait$
	ldx #PAL_RASTER+1	; wait for this raster line (times 2)
wait$:	cpx $9004
	bne wait$


sync:	ldx #NTSC_RASTER	; wait for this raster line (times 2)
	lda SYSTEMSEL
	beq 0$
	ldx #PAL_RASTER		; wait for this raster line (times 2)
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed

	; No cycle-exact needed for this part..

	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	ldy #<NTSC_TIMER_VALUE
	ldx #>NTSC_TIMER_VALUE
	lda SYSTEMSEL
	beq 1$
	ldy #<PAL_TIMER_VALUE
	ldx #>PAL_TIMER_VALUE
1$	sty $9126
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	;lda #$82
	;sta $911e	; enable Restore key

	cli

	lda #<nextpart
	sta NEXTPART+0
	lda #>nextpart
	sta NEXTPART+1

	lda SYSTEMSEL
	beq ntsc$
	lda #"P"
	sta nextFile+7
ntsc$	ldx #<nextFile
	ldy #>nextFile
	jsr loader

2$	bcs 2$

KEYWAIT	lda #0
	beq KEYWAIT

	lda #0
	sta $900f	; black border
	sta $9002	; 0 columns
	sta $9003	; 0 rows

	sei

	lda #<COMMONIRQ	; set the raster IRQ routine pointer
	sta $314
	lda #>COMMONIRQ
	sta $315

	cli
	jmp copy

nextFile:
	;dc.b "SHEKKI.N"
	;dc.b "TECH^2.N"
	 dc.b "VICPIC.N"

noMemText:
	dc.b "16K EXPANSION REQUIRED", 13, 0

noMem:	ldy #0
0$	lda noMemText,y
	beq 1$
	jsr $ffd2
	iny
	bne 0$
1$	rts



	align 256,0
irq:
	; No stable raster needed in this part!
	lda $9124	; ack?

swait	lda #0
	beq 0$

	dec swait+1
	jmp reti
0$	dec $9001
	lda $9001
	cmp #TOPPOS-8
	bne reti

	lda #TOPPOS
	sta $9001
	jsr scroll

reti	;jmp $eb18	; return from IRQ


	lda AUTO
	beq noauto$
	lda $9111	; VIA#1 port A
	and #$20	; fire?
	bne noauto$
	dec AUTO	; disable automatic control
	jsr chtextcol
noauto$


	; Check keyboard	run/stop lshift x v n , / up/down
	lda $9121
	bmi 90$
	; up/down
90$	asl
	bmi 91$
	; /
91$	asl
	bmi 92$
	; ,
92$	asl
	bmi 93$
	; n
93$	asl
	bmi 94$
	; v
94$	asl
	bmi 95$
	; x
	ldx AUTO
	beq 95$
	dec AUTO	; disable automatic control
	jsr chtextcol
	jmp 97$

95$	asl
	bmi 96$
	; lshift
96$	asl
	bmi 97$
	; run/stop
	inc KEYWAIT+1
97$

	jmp COMMONIRQ


scroll:
	ldx #COLUMNS-1
1$	lda $1000+1*COLUMNS,x
	sta $1000+0*COLUMNS,x
	lda $1000+2*COLUMNS,x
	sta $1000+1*COLUMNS,x
	lda $1000+3*COLUMNS,x
	sta $1000+2*COLUMNS,x
	lda $1000+4*COLUMNS,x
	sta $1000+3*COLUMNS,x
	dex
	bpl 1$

	ldx #COLUMNS-1
2$	lda $1000+5*COLUMNS,x
	sta $1000+4*COLUMNS,x
	lda $1000+6*COLUMNS,x
	sta $1000+5*COLUMNS,x
	lda $1000+7*COLUMNS,x
	sta $1000+6*COLUMNS,x
	lda $1000+8*COLUMNS,x
	sta $1000+7*COLUMNS,x
	lda $1000+9*COLUMNS,x
	sta $1000+8*COLUMNS,x
	dex
	bpl 2$

	ldx #COLUMNS-1
3$	lda $1000+10*COLUMNS,x
	sta $1000+9*COLUMNS,x
	lda $1000+11*COLUMNS,x
	sta $1000+10*COLUMNS,x
	lda $1000+12*COLUMNS,x
	sta $1000+11*COLUMNS,x
	lda $1000+13*COLUMNS,x
	sta $1000+12*COLUMNS,x
	lda $1000+14*COLUMNS,x
	sta $1000+13*COLUMNS,x
	dex
	bpl 3$

	ldx #COLUMNS-1
4$
	lda $1000+15*COLUMNS,x
	sta $1000+14*COLUMNS,x
	lda $1000+16*COLUMNS,x
	sta $1000+15*COLUMNS,x
	lda SYSTEMSEL
	beq 11$

	lda $1000+17*COLUMNS,x
	sta $1000+16*COLUMNS,x
	lda $1000+18*COLUMNS,x
	sta $1000+17*COLUMNS,x
	lda $1000+19*COLUMNS,x
	sta $1000+18*COLUMNS,x
	lda #0
	sta $1000+PAL_ROWS*COLUMNS-COLUMNS,x	; clear the last line
	beq 12$

11$	lda #0
	sta $1000+NTSC_ROWS*COLUMNS-COLUMNS,x	; clear the last line
12$	dex
	bpl 4$

5$	lda #0
	and #1
	beq 0$

	ldy #0
	lda (tPtr),y
	bne 8$
	jsr inittext
	lda AUTO
	beq 13$
	sta KEYWAIT+1	; automatic control -- skip to next part
13$	rts
8$	bpl 9$
	sta swait+1
	bmi 10$
9$	cmp #13
	bne 6$
	; if an empty line, just one empty line
10$	inc tPtr+0
	bne 7$
	inc tPtr+1
7$	rts

6$	jsr puttextline
0$	inc 5$+1
	rts


text:
	dc.b "ALBERT",13
	dc.b "OF PU[<]",13
	dc.b "PRESENTS",13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "VICI",13
	dc.b "ITERUM",13
	dc.b "MM",13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "THE",13
	dc.b "VIC DEMO",13
	dc.b "AD[OOO",13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "REMEMBER",13
	dc.b "THESE",13
	dc.b "FACTS",13
	dc.b "ABOUT",13
	dc.b "VIC [O",13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "VIC [O",13
	dc.b "HAS",13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13
	dc.b "FIVE KB",13
	dc.b "OF",13
	dc.b "GRAPHICS",13
	dc.b "MEMORY",13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b "NO",13
	dc.b "SPRITES",13
	dc.b 13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b "NO",13
	dc.b "SMOOTH",13
	dc.b "SCROLL",13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b "AND NO",13
	dc.b "BITMAP",13
	dc.b "MODES",13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "THREE",13
	dc.b "PULSE",13
	dc.b "VOICES",13
	dc.b 13
	dc.b "PLUS",13
	dc.b "NOISE",13
	dc.b 13
	dc.b "NO ADSR",13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "FIRE OR",13
	dc.b "KEY X",13
	dc.b "SELECTS",13
	dc.b 13
	dc.b "MANUAL",13
	dc.b "MODE",13
	dc.b 13,13
	dc.b 13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b "RUNSTOP",13
	dc.b 13
	dc.b "TO SKIP",13
	dc.b "PARTS IN",13
	dc.b 13
	dc.b "MANUAL",13
	dc.b "MODE",13
	dc.b 13,13
	dc.b 128
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13

	dc.b "EMULATOR",13
	dc.b "WARNING",13
	dc.b 13,13,13
	dc.b "EXPECT",13
	dc.b "CRASHES",13
	dc.b "AND",13
	dc.b "MESSED",13
	dc.b "DISPLAY",13

	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 13,13
	dc.b 0

#include "loader.asm"

nextpart:

	.org $5400

#include "/songs/acplay.a65"

#include "/songs/jack.sng"
