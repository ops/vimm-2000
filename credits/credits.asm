
; Copyright © 2000 Pasi Ojala

  processor 6502


loader = $5f00
NEXTENDHI = $5fb3
NEXTPART = NEXTENDHI+1
SYSTEMSEL = NEXTPART+2
AUTO = SYSTEMSEL+1
COMMONIRQ = AUTO+1
;copy = $5fc5


zpTmp = 1


; 8k memory expansion needed


; $1000-1200	Video matrix
; $1200-1400
; $1400-1c00	chars (256 8-byte chars)


; $5f00-$5fff	fastloader -- can be overwritten if this is the last part


NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34

COLUMNS1 = 3			; 8-byte chars:
COLUMNS2 = 29			; (4+1)*29	= 145 chars
ROWS = 36			; 3*36		= 108 chars
TOPROWS = 15			; total		= 253 chars
MIDROWS = 5	; must be 4 or 5!

#else

LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER	EQU	26

COLUMNS1 = 3			; 8-byte chars:
COLUMNS2 = 25			; (4+1)*26	= 130
ROWS = 32			; 3*30		=  90
TOPROWS = 12			; total		= 220
MIDROWS = 4	; must be 4 or 5!
#endif

RASTER	= 14+TOPROWS*4-4

TIMER_VALUE = LINES * CYCLES_PER_LINE - 2	; P $5685 N $4245


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm



	ORG $1201
	DC.B $0b,$12,$ef,0	; '239 SYS4621'
	DC.B $9e,$34,$36,$32
	DC.B $31,0,0,0

	jmp start

inithoriz:
	ldx #COLUMNS2-1
	stx h_code
	clc
0$	lda fivex,x
	sta $1000 + TOPROWS*COLUMNS1 + 0*COLUMNS2,x
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 1*COLUMNS2,x
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 2*COLUMNS2,x
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 3*COLUMNS2,x
#if MIDROWS > 4
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 4*COLUMNS2,x
#endif
	dex
	bpl 0$
	lda #SCRCENTER-COLUMNS2+1
	sta xhoriz+1
	rts


scrollhoriz:
	lda xsel+1
	bne cont2$
	lda #40
	sta xsel+1

	lda xhoriz+1
	cmp #SCRCENTER-COLUMNS2+1
	beq copy2$
	rts

cont2$	lda #0
	sta xsel+1

	dec xhoriz+1
	lda xhoriz+1
	cmp #SCRCENTER-COLUMNS2-1	; 4? -> 6
	bne nope$
	jmp copy$
nope$	cmp #SCRCENTER-COLUMNS2-0	; 5?
	beq setup$
	rts

copy2$
#if MIDROWS > 4
	ldx #31
#else
	ldx #23
#endif
src2$	ldy font_h+32,x
src1$	lda font_h,x
	asl
	asl
	ora shup2,y
trg2$	sta hor_store+32,x
	dex
	bpl src2$
	; 32*26 = 12 lines
	rts

setup$	ldx h_code	; target is hor_store+64*column
	lda h_mod+1,x
	lsr
	lsr
	clc
	adc #>hor_store
	sta trg$+2
	lda h_mod+1,x
	asl
	asl
	asl
	asl
	asl
	asl
	clc
	adc #<hor_store
	sta trg$+1
	bcc 10$
	inc trg$+2
	clc
10$	adc #32
	sta trg2$+1
	lda trg$+2
	adc #0
	sta trg2$+2

	dec columncnt_h
	bmi ptr$

	lda src1$+1	; next char column...
	clc
	adc #32
	sta src1$+1
	sta src$+1
	bcc 7$
	inc src1$+2
	inc src$+2
7$	lda src2$+1	; next char column...
	clc
	adc #32
	sta src2$+1
	bcc 8$
	inc src2$+2
8$	rts

	; counter negative -- get next character
ptr$	lda text_h
	inc ptr$+1
	bne 4$
	inc ptr$+2
4$	tay
	bpl 5$
	cmp #255
	beq reset$
	jsr seteffect
	jmp 9$
reset$	ldy #<text_h
	sty ptr$+1
	ldy #>text_h
	sty ptr$+2
	;lda AUTO
	;beq 9$
	dec FADEOUT+1
9$	lda #0
5$	ldy #1
	cmp #"I"
	beq 6$
	cmp #" "
	beq 6$
	cmp #"."
	bne nodot$
	lda #30
	dey
	beq 6$
nodot$	ldy #2
6$	sty columncnt_h
	and #31
	tay
	lda src_h_hi+0,y
	sta src$+2
	sta src1$+2
	lda src_h_lo+0,y
	sta src$+1
	sta src1$+1
	clc
	adc #32
	sta src2$+1
	lda src1$+2
	adc #0
	sta src2$+2
	rts


copy$
#if MIDROWS > 4
	ldx #31
#else
	ldx #23
#endif
src$	lda font_h,x
trg$	sta hor_store,x
	dex
	bpl src$	; 32*14 = 448 = 6.3 lines

	ldy #0
	clc
copy2	lda $1001 + TOPROWS*COLUMNS1 + 0*COLUMNS2,y
	sta $1000 + TOPROWS*COLUMNS1 + 0*COLUMNS2,y
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 1*COLUMNS2,y
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 2*COLUMNS2,y
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 3*COLUMNS2,y
#if MIDROWS > 4
	adc #1
	sta $1000 + TOPROWS*COLUMNS1 + 4*COLUMNS2,y
#endif
	iny
	cpy #COLUMNS2-1
	bne copy2		; 28*44 = 1232 = 17.4 lines
	;samepage copy2

	ldy h_code
	ldx h_mod+1,y
	stx h_code
	ldy fivex,x
	sty $1000 + TOPROWS*COLUMNS1 + 1*COLUMNS2-1
	iny
	sty $1000 + TOPROWS*COLUMNS1 + 2*COLUMNS2-1
	iny
	sty $1000 + TOPROWS*COLUMNS1 + 3*COLUMNS2-1
	iny
	sty $1000 + TOPROWS*COLUMNS1 + 4*COLUMNS2-1
#if MIDROWS > 4
	iny
	sty $1000 + TOPROWS*COLUMNS1 + 5*COLUMNS2-1
#endif

	lda #SCRCENTER-COLUMNS2+1	; 6
	sta xhoriz+1
	rts

	; color luminance order
	; 062485371

chtextcol:
0$	lda #0
	and #7
	tay
	lda cnt$
	asl
	;clc
	adc acc$
	sta acc$
	bcc 1$
	iny
1$	lda $900e
	and #15
	ora COLO1$,y
	sta $900e
	lda COLO2$,y
	sta $900f

	inc cnt$
	bpl 2$
	lda #0
	sta cnt$
	inc 0$+1
2$	rts
cnt$	dc.b 0
acc$	dc.b 0

COLO1$	dc.b $80,$40,$20,$60,$20,$40,$80,$50, $80
COLO2$	dc.b $98,$c8,$a8,$38,$a8,$c8,$98,$d8, $98



scrollvert2:
	lda v_code
	sec
	sbc #145-(1+TOPROWS+MIDROWS)
	sta a$+1
	asl
a$	adc #0	; *3
	clc
	adc #<code_v
	sta ss2$+1
	lda #>code_v
	adc #0
	sta ss2$+2

	ldx #(ROWS-TOPROWS-MIDROWS)*COLUMNS1 -1
	sec
ss2$	lda code_v,x	; 63..170 + 0..47 = 63..217
	sta $1000+TOPROWS*COLUMNS1+MIDROWS*COLUMNS2,x
	sbc #ROWS
	dex
	sta $1000+TOPROWS*COLUMNS1+MIDROWS*COLUMNS2,x
	sbc #ROWS
	dex
	sta $1000+TOPROWS*COLUMNS1+MIDROWS*COLUMNS2,x
	dex
	bpl ss2$	; 546 cycles = 7.7 lines
	rts




	.org $1400
#repeat 256/2
	dc.b $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
#repend

	.org $1c00
start
	lda #0
	sta $900f
	sta $9002	; 0 columns
	sta $9003	; 0 rows
	lda $900e
	and #15
	sta $900e
	lda #$cd	; video matrix $1000, chars $1400
	sta $9005

	lda #14-3
	sta $9001
	lda #SCRCENTER-COLUMNS1
	sta $9000

	ldx #0
1$	lda #255
	sta $1000,x
	lda #9
	sta $9400,x

	txa
	lsr
	lsr
	lsr
	lsr
	eor #$05
	sta shright,x
	txa
	asl
	asl
	asl
	asl
	eor #$50
	sta shleft,x

	txa
	eor #$55
	sta shnone,x

	txa
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #$14
	sta chrmaph,x

	txa
	asl
	asl
	asl
	sta chrmapl,x

	txa
	asl
	rol
	rol
	and #3
shup2 = $1100
	sta shup2,x

	dex
	bne 1$

	jsr inithoriz

	sei
	lda #$7f
	sta $913e	; disable and acknowledge interrupts / NMIs
	sta $912d
	;sta $911e	; disable NMIs (Restore key)

nmiwait
	lda #<nmi
	sta $318
	lda #>nmi
	sta $319

0$	lda $9004
	bne 0$

	lda #$40	; enable Timer A free run on irq-VIA
	sta $911b
	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9116	; load the timer low byte latch
	stx $9115	; start Timer A on VIA 2 (NMI)


;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #0		; disable Timer A free run
	sta $912b


sync	ldx #RASTER-10	; wait for this raster line (times 2)
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$	ldx $9004
	txa
	bit $24
#if SYSTEM & PAL
	ldx #24
#else
	bit $24
	ldx #21
#endif
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne 1$


	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126
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
	sta $911e	; enable Restore key

	cli

	;rts
	jmp *


; No double-buffering needed!
;           |   |
;           |   |
;           |   |
;           |   |
;           |   |
;           |   |
;-----------     --------------		<- IRQ
;
;-----------     --------------		<- 'wait' until here
;           |   |
;           |   |
;           |   |
;           |   |
;	<- then vertical scroll code
;		new row plot, upper matrix part scroll
;	<- horizontal scroll code + plot (x + 160 lines)
;	<- then the crossover point plot
;		plus lower matrix part scroll




	; 00 - background, 01 - border, 11 - aux, 10 - character

	nop
	nop
	nop

irq	lda #<(TIMER_VALUE-46+2)	; 2 for reload time
	sec
	sbc $9124	; 46 to 53 cycles delay at this stage
			; 90..83/23..16 in $9124 for PAL/NTSC
	; A = 0..7	0=wait 7 cycles .. 7=wait 0 cycles
	sta *+4
	bne *+2
	nop
	lda #$a9
	lda #$a9
	lda #$a9
	bit $ea

#if SYSTEM & PAL
	ldx #16				; -->
#else
	ldx #16+3
#endif
wait0	dex
	bpl wait0
	samepage wait0

	lda $9001	;14..11
	sec
	sbc #11		;3..0
	asl		;6..0
	tax
wait1					; wait for right line
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
#if SYSTEM & PAL
	pha
	pla
	bit $ea
#else
	nop
	nop
#endif
	dex
	bpl wait1	; 71/65 cycles/loop
	;samepage wait1


#if SYSTEM & PAL
	nop
	nop
xhoriz	lda #SCRCENTER-COLUMNS2+1	; 34-29+1 = 6..5
	ldx #COLUMNS2
	sta $9000
	stx $9002
#else
xhoriz	lda #SCRCENTER-COLUMNS2+1
	ldx #COLUMNS2
	sta $9000	; phuck! NTSC raster lines change in the middle of the screen
	stx $9002	; can't change horizontal reg through the middle part
#endif

#if SYSTEM & PAL
	ldx #7
w2$	dex
	bpl w2$
#else
#endif
	jsr chtextcol	; wait until border to hide the change bug...

	jsr effects

	lda $9001
	clc
	adc #TOPROWS*4+MIDROWS*4-1
w0$	cmp $9004
	bne w0$
#if SYSTEM & PAL
	ldx #21
#else
	ldx #21+6
#endif
w1$	dex
	bpl w1$


	ldx #COLUMNS1
xvert	lda #SCRCENTER-COLUMNS1		; --> can be from 1..49
	sta $9000
	stx $9002

	lda #2*ROWS
	sta $9003


	lda loader
	cmp #$8e
	bne 100$
	jsr $5400	; music
100$


	jsr scrollhoriz

	;lda #$44
	;sta $900f

#if 1
#if SYSTEM & PAL
width = 87
#else
width = 54
#endif
xcnt	ldx #0
	inx
	cpx #width
	bne xok0$
	ldx #0
xok0$	stx xcnt+1
	txa
	cmp #width/2
	bcc xok1$
	lda #width-1
	;sec
	sbc xcnt+1
	clc
#if SYSTEM & PAL
xok1$	adc #9
#else
xok1$	adc #2
#endif
	sta xvert+1
#else
	lda $9111	; VIA#1 port A
	tay
	and #4
	bne 21$
	inc xvert+1
21$	tya
	and #8
	bne 22$
	dec xvert+1
22$
#endif
	jsr scrollvert1	; writes $9001
	jsr copyhoriz	; reads  $9001	; 142 / 140 lines (PAL / NTSC)
	jsr scrollvert2

	jsr copyoverlap		; over 43/59 NTSC lines
	jsr pacman

#if 0	; debug color..
	inc $900f
	dec $900f
#endif


FADEOUT	lda #128+1	; fade out music, continue the scroll..
	bmi 0$

	lsr
	lsr
	lsr
	lsr
	sta ora$+1
	lda $900e
	and #$f0
ora$	ora #$00
	sta $900e
1$	dec FADEOUT+1

0$	;jmp $eabf	; return to normal IRQ
	;jmp $eb18	; return from IRQ
	pla
	tay
	pla
	tax
	pla
	rti


nmi	pha
	lda xvert+1
	sta $9000
	lda $9114	; acknowledge the NMI
	pla
	rti


scrollvert1:
	dec $9001
	lda $9001
	cmp #14-4
	bne no0$

	lda #14-0
	sta $9001
#if SYSTEM & PAL
	jmp copy$	; 272+630
#else
	jmp matrix$	; 504
#endif

no0$	cmp #14-2
	beq setup$	; 80/180 cycles
#if SYSTEM & NTSC
	cmp #14-3
	bne no1$
	jmp copy$	; 272 cycles
no1$
#endif
	rts

setup$	ldx v_code
#if SYSTEM & PAL	; NTSC first plots, then moves into view
	inx
	cpx #145+ROWS
	bne 1$
	ldx #145
1$
#endif
	lda chrmaph,x
	sta trg0$+2
	lda chrmapl,x
	sta trg0$+1


	;lda trg0$+1
	clc
	adc #<(ROWS*8)
	sta trg1$+1
	lda trg0$+2
	adc #>(ROWS*8)
	sta trg1$+2
	lda trg1$+1
	clc
	adc #<(ROWS*8)
	sta trg2$+1
	lda trg1$+2
	adc #>(ROWS*8)
	sta trg2$+2

	dec columncnt_v
	bmi ptr$	;66

	; counter positive or zero -- advance source pointers
	lda y0$+1
	clc
	adc #8
	sta y0$+1
	rts	; setup 80

	; counter negative -- get next character
ptr$	lda text_v
	inc ptr$+1
	bne 4$
	inc ptr$+2
4$	cmp #255
	bne 5$
	ldy #<text_v
	sty ptr$+1
	ldy #>text_v
	sty ptr$+2
	lda #0
5$
	ldy #1		; 16 pixels high
	cmp #"."
	bne nodot$

	sty columncnt_v
	lda #>(font_v+9*96+16)
	sta src0$+2
	lda #<(font_v+9*96+16)
	sta src0$+1
	bne cont$

nodot$	cmp #"I"
	beq 6$
	cmp #" "
	beq 6$
	ldy #3		; 32 pixels high
6$	sty columncnt_v
	and #31
	tay

	lda src_v_hi,y
	sta src0$+2
	lda src_v_lo,y
	sta src0$+1

cont$	;lda src0$+1
	clc
	adc #32
	sta src1$+1
	lda src0$+2
	adc #0
	sta src1$+2
	lda src1$+1
	clc
	adc #32
	sta src2$+1
	lda src1$+2
	adc #0
	sta src2$+2
	lda #7
	sta y0$+1
	rts	; setup 187

	align 32,0
copy$
y0$	ldy #7
	ldx #7
src0$	lda font_v,y
trg0$	sta $1400+0*ROWS*8+145*8,x
src1$	lda font_v,y
trg1$	sta $1400+1*ROWS*8+145*8,x
src2$	lda font_v,y
trg2$	sta $1400+2*ROWS*8+145*8,x
	dey
	dex
	bpl src0$		; 272 cycles = 3.8 lines

#if SYSTEM & NTSC
	rts

matrix$
#endif
	ldx v_code
	inx
	cpx #145+ROWS
	bne v1$
	ldx #145
v1$	stx v_code
	txa
	sec
	sbc #145-1
	sta a$+1
	asl
	clc
a$	adc #0	; *3
	clc
	adc #<code_v
	sta ss$+1
	lda #>code_v
	adc #0
	sta ss$+2

	ldx #TOPROWS*COLUMNS1 -1	; 15..12*3 = 45..36
	;49
	sec
ss$	lda code_v,x	; 3..110 + 0..44 = 3..156
	sta $1000,x
	sbc #ROWS
	dex
	sta $1000,x
	sbc #ROWS
	dex
	sta $1000,x
	dex
	bpl ss$		; 630 / 504 cycles (PAL / NTSC)

	rts



copyhoriz:
	; $9001*2 -21
	lda $9001
	asl
	sec
	sbc #21
	sta zpTmp	; 0..7
	clc
xsel	adc #0		; 0 / 40	MUST START AT 0!!
	clc
	adc #<hor_map
	sta 0$+1
	lda #>hor_map
	adc #0
	sta 0$+2

	lda #8
	sec
	sbc zpTmp
	sta start$+1
	sta pacoffset
	clc
#if MIDROWS > 4
	adc #32		;32..40
#else
	adc #24
#endif
	sta end$+1
	tax
	ldy #7
	lda #$55		; first clear 8 lines -- saves 12 lines
clr$	sta $1400+0*8*MIDROWS,x
	sta $1400+1*8*MIDROWS,x
	sta $1400+2*8*MIDROWS,x
	sta $1400+3*8*MIDROWS,x
	sta $1400+4*8*MIDROWS,x
	sta $1400+5*8*MIDROWS,x
	sta $1400+6*8*MIDROWS,x
	sta $1400+7*8*MIDROWS,x
	sta $1400+8*8*MIDROWS,x
	sta $1400+9*8*MIDROWS,x
	sta $1400+10*8*MIDROWS,x
	sta $1400+11*8*MIDROWS,x
	sta $1400+12*8*MIDROWS,x
	sta $1400+13*8*MIDROWS,x
	sta $1400+14*8*MIDROWS,x
	sta $1400+15*8*MIDROWS,x
	sta $1400+16*8*MIDROWS,x
	sta $1400+17*8*MIDROWS,x
	sta $1400+18*8*MIDROWS,x
	sta $1400+19*8*MIDROWS,x
	sta $1400+20*8*MIDROWS,x
	sta $1400+21*8*MIDROWS,x
	sta $1400+22*8*MIDROWS,x
	sta $1400+23*8*MIDROWS,x
#if COLUMNS2 > 25
	sta $1400+24*8*MIDROWS,x
#endif
#if COLUMNS2 > 26
	sta $1400+25*8*MIDROWS,x
#endif
#if COLUMNS2 > 27
	sta $1400+26*8*MIDROWS,x
#endif
#if COLUMNS2 > 28
	sta $1400+27*8*MIDROWS,x
#endif
	inx
	dey
	bpl clr$	; 8 * 147|132 = 1176|1056 = 16.6|16.2 lines (PAL|NTSC)

	sta $1400+COLUMNS2*8*MIDROWS-1
	sta $1400+COLUMNS2*8*MIDROWS-2
	sta $1400+COLUMNS2*8*MIDROWS-3
	sta $1400+COLUMNS2*8*MIDROWS-4
	sta $1400+COLUMNS2*8*MIDROWS-5
	sta $1400+COLUMNS2*8*MIDROWS-6
	sta $1400+COLUMNS2*8*MIDROWS-7
	sta $1400+COLUMNS2*8*MIDROWS-8

	sta $1400+0*8*MIDROWS+0
	sta $1400+0*8*MIDROWS+1
	sta $1400+0*8*MIDROWS+2
	sta $1400+0*8*MIDROWS+3
	sta $1400+0*8*MIDROWS+4
	sta $1400+0*8*MIDROWS+5
	sta $1400+0*8*MIDROWS+6
	sta $1400+0*8*MIDROWS+7	; +1 line


start$	ldx #8
0$	ldy hor_map+8,x

	lda hor_store+0*2*32,y		; 4
	sta $1400+0*8*MIDROWS,x		; 5
	lda hor_store+1*2*32,y
	sta $1400+1*8*MIDROWS,x
	lda hor_store+2*2*32,y
	sta $1400+2*8*MIDROWS,x
	lda hor_store+3*2*32,y
	sta $1400+3*8*MIDROWS,x
	lda hor_store+4*2*32,y
	sta $1400+4*8*MIDROWS,x
	lda hor_store+5*2*32,y
	sta $1400+5*8*MIDROWS,x
	lda hor_store+6*2*32,y
	sta $1400+6*8*MIDROWS,x
	lda hor_store+7*2*32,y
	sta $1400+7*8*MIDROWS,x
	lda hor_store+8*2*32,y
	sta $1400+8*8*MIDROWS,x
	lda hor_store+9*2*32,y
	sta $1400+9*8*MIDROWS,x
	lda hor_store+10*2*32,y
	sta $1400+10*8*MIDROWS,x
	lda hor_store+11*2*32,y
	sta $1400+11*8*MIDROWS,x
	lda hor_store+12*2*32,y
	sta $1400+12*8*MIDROWS,x
	lda hor_store+13*2*32,y
	sta $1400+13*8*MIDROWS,x
	lda hor_store+14*2*32,y
	sta $1400+14*8*MIDROWS,x
	lda hor_store+15*2*32,y
	sta $1400+15*8*MIDROWS,x
	lda hor_store+16*2*32,y
	sta $1400+16*8*MIDROWS,x
	lda hor_store+17*2*32,y
	sta $1400+17*8*MIDROWS,x
	lda hor_store+18*2*32,y
	sta $1400+18*8*MIDROWS,x
	lda hor_store+19*2*32,y
	sta $1400+19*8*MIDROWS,x
	lda hor_store+20*2*32,y
	sta $1400+20*8*MIDROWS,x
	lda hor_store+21*2*32,y
	sta $1400+21*8*MIDROWS,x
	lda hor_store+22*2*32,y
	sta $1400+22*8*MIDROWS,x
	lda hor_store+23*2*32,y
	sta $1400+23*8*MIDROWS,x
#if COLUMNS2 > 24
	lda hor_store+24*2*32,y
	sta $1400+24*8*MIDROWS,x
#endif
#if COLUMNS2 > 25
	lda hor_store+25*2*32,y
	sta $1400+25*8*MIDROWS,x		; NTSC 26 columns: 247*32 = 7904 = 122 lines
#endif
#if COLUMNS2 > 26
	lda hor_store+26*2*32,y
	sta $1400+26*8*MIDROWS,x
#endif
#if COLUMNS2 > 27
	lda hor_store+27*2*32,y
	sta $1400+27*8*MIDROWS,x
#endif
#if COLUMNS2 > 28
	lda hor_store+28*2*32,y
	sta $1400+28*8*MIDROWS,x
#endif
	inx
end$	cpx #32
	beq 1$
	jmp 0$				; PAL 29 columns: 274*32 = 8768 = 123.5 lines
1$
	rts



	.align 256,0
font_h:
#if MIDROWS > 4
#incbin "bigfont_h.bin"
#else
#incbin "bigfont_hn.bin"
#endif

	.align 256,0
font_v:
#incbin "bigfont_v1.bin"	; 27 chars
#incbin "bigfont_v2.bin"	; 5 chars more

	align 256,0
hor_store:
#repeat 8*COLUMNS2
	dc.b $55,$55,$55,$55,$55,$55,$55,$55
#repend
	dc.b $55,$55,$55,$55,$55,$55,$55,$55
	dc.b $55,$55,$55,$55,$55,$55,$55,$55
	dc.b $55,$55,$55,$55,$55,$55,$55,$55
	dc.b $55,$55,$55,$55,$55,$55,$55,$55
	dc.b $55,$55,$55,$55,$55,$55,$55,$55

	.align 256,0
shleft
	ds.b 256
shright
	ds.b 256
shnone
	ds.b 256
chrmaph
	ds.b 256
chrmapl
	ds.b 256

src_v_lo
	dc.b <(font_v+0*96),<(font_v+1*96),<(font_v+2*96),<(font_v+3*96)
	dc.b <(font_v+4*96),<(font_v+5*96),<(font_v+6*96),<(font_v+7*96)
	dc.b <(font_v+8*96),<(font_v+9*96),<(font_v+10*96),<(font_v+11*96)
	dc.b <(font_v+12*96),<(font_v+13*96),<(font_v+14*96),<(font_v+15*96)
	dc.b <(font_v+16*96),<(font_v+17*96),<(font_v+18*96),<(font_v+19*96)
	dc.b <(font_v+20*96),<(font_v+21*96),<(font_v+22*96),<(font_v+23*96)
	dc.b <(font_v+24*96),<(font_v+25*96),<(font_v+26*96),<(font_v+27*96)
	dc.b <(font_v+28*96),<(font_v+29*96),<(font_v+30*96),<(font_v+31*96)
src_v_hi
	dc.b >(font_v+0*96),>(font_v+1*96),>(font_v+2*96),>(font_v+3*96)
	dc.b >(font_v+4*96),>(font_v+5*96),>(font_v+6*96),>(font_v+7*96)
	dc.b >(font_v+8*96),>(font_v+9*96),>(font_v+10*96),>(font_v+11*96)
	dc.b >(font_v+12*96),>(font_v+13*96),>(font_v+14*96),>(font_v+15*96)
	dc.b >(font_v+16*96),>(font_v+17*96),>(font_v+18*96),>(font_v+19*96)
	dc.b >(font_v+20*96),>(font_v+21*96),>(font_v+22*96),>(font_v+23*96)
	dc.b >(font_v+24*96),>(font_v+25*96),>(font_v+26*96),>(font_v+27*96)
	dc.b >(font_v+28*96),>(font_v+29*96),>(font_v+30*96),>(font_v+31*96)
src_h_lo
	dc.b <(font_h+0*96),<(font_h+1*96),<(font_h+2*96),<(font_h+3*96)
	dc.b <(font_h+4*96),<(font_h+5*96),<(font_h+6*96),<(font_h+7*96)
	dc.b <(font_h+8*96),<(font_h+9*96),<(font_h+10*96),<(font_h+11*96)
	dc.b <(font_h+12*96),<(font_h+13*96),<(font_h+14*96),<(font_h+15*96)
	dc.b <(font_h+16*96),<(font_h+17*96),<(font_h+18*96),<(font_h+19*96)
	dc.b <(font_h+20*96),<(font_h+21*96),<(font_h+22*96),<(font_h+23*96)
	dc.b <(font_h+24*96),<(font_h+25*96),<(font_h+26*96),<(font_h+27*96)
	dc.b <(font_h+28*96),<(font_h+29*96),<(font_h+30*96),<(font_h+31*96)
src_h_hi
	dc.b >(font_h+0*96),>(font_h+1*96),>(font_h+2*96),>(font_h+3*96)
	dc.b >(font_h+4*96),>(font_h+5*96),>(font_h+6*96),>(font_h+7*96)
	dc.b >(font_h+8*96),>(font_h+9*96),>(font_h+10*96),>(font_h+11*96)
	dc.b >(font_h+12*96),>(font_h+13*96),>(font_h+14*96),>(font_h+15*96)
	dc.b >(font_h+16*96),>(font_h+17*96),>(font_h+18*96),>(font_h+19*96)
	dc.b >(font_h+20*96),>(font_h+21*96),>(font_h+22*96),>(font_h+23*96)
	dc.b >(font_h+24*96),>(font_h+25*96),>(font_h+26*96),>(font_h+27*96)
	dc.b >(font_h+28*96),>(font_h+29*96),>(font_h+30*96),>(font_h+31*96)


	align 256,0

code_v
	dc.b 145+0,145+ROWS,145+2*ROWS
	dc.b 146+0,146+ROWS,146+2*ROWS
	dc.b 147+0,147+ROWS,147+2*ROWS
	dc.b 148+0,148+ROWS,148+2*ROWS
	dc.b 149+0,149+ROWS,149+2*ROWS
	dc.b 150+0,150+ROWS,150+2*ROWS
	dc.b 151+0,151+ROWS,151+2*ROWS
	dc.b 152+0,152+ROWS,152+2*ROWS
	dc.b 153+0,153+ROWS,153+2*ROWS
	dc.b 154+0,154+ROWS,154+2*ROWS
	dc.b 155+0,155+ROWS,155+2*ROWS
	dc.b 156+0,156+ROWS,156+2*ROWS
	dc.b 157+0,157+ROWS,157+2*ROWS
	dc.b 158+0,158+ROWS,158+2*ROWS
	dc.b 159+0,159+ROWS,159+2*ROWS	; 15
	dc.b 160+0,160+ROWS,160+2*ROWS
	dc.b 161+0,161+ROWS,161+2*ROWS
	dc.b 162+0,162+ROWS,162+2*ROWS
	dc.b 163+0,163+ROWS,163+2*ROWS
	dc.b 164+0,164+ROWS,164+2*ROWS
	dc.b 165+0,165+ROWS,165+2*ROWS
	dc.b 166+0,166+ROWS,166+2*ROWS
	dc.b 167+0,167+ROWS,167+2*ROWS
	dc.b 168+0,168+ROWS,168+2*ROWS
	dc.b 169+0,169+ROWS,169+2*ROWS
	dc.b 170+0,170+ROWS,170+2*ROWS
	dc.b 171+0,171+ROWS,171+2*ROWS
	dc.b 172+0,172+ROWS,172+2*ROWS
	dc.b 173+0,173+ROWS,173+2*ROWS
#if ROWS > 29
	dc.b 174+0,174+ROWS,174+2*ROWS
#endif
#if ROWS > 30
	dc.b 175+0,175+ROWS,175+2*ROWS
#endif
#if ROWS > 31
	dc.b 176+0,176+ROWS,176+2*ROWS
#endif
#if ROWS > 32
	dc.b 177+0,177+ROWS,177+2*ROWS
#endif
#if ROWS > 33
	dc.b 178+0,178+ROWS,178+2*ROWS
#endif
#if ROWS > 34
	dc.b 179+0,179+ROWS,179+2*ROWS
#endif
#if ROWS > 35
	dc.b 180+0,180+ROWS,180+2*ROWS
#endif
	dc.b 145+0,145+ROWS,145+2*ROWS
	dc.b 146+0,146+ROWS,146+2*ROWS
	dc.b 147+0,147+ROWS,147+2*ROWS
	dc.b 148+0,148+ROWS,148+2*ROWS
	dc.b 149+0,149+ROWS,149+2*ROWS
	dc.b 150+0,150+ROWS,150+2*ROWS
	dc.b 151+0,151+ROWS,151+2*ROWS
	dc.b 152+0,152+ROWS,152+2*ROWS
	dc.b 153+0,153+ROWS,153+2*ROWS
	dc.b 154+0,154+ROWS,154+2*ROWS
	dc.b 155+0,155+ROWS,155+2*ROWS
	dc.b 156+0,156+ROWS,156+2*ROWS
	dc.b 157+0,157+ROWS,157+2*ROWS
	dc.b 158+0,158+ROWS,158+2*ROWS
	dc.b 159+0,159+ROWS,159+2*ROWS	; 15
	dc.b 160+0,160+ROWS,160+2*ROWS
	dc.b 161+0,161+ROWS,161+2*ROWS
	dc.b 162+0,162+ROWS,162+2*ROWS
	dc.b 163+0,163+ROWS,163+2*ROWS
	dc.b 164+0,164+ROWS,164+2*ROWS
	dc.b 165+0,165+ROWS,165+2*ROWS
	dc.b 166+0,166+ROWS,166+2*ROWS
	dc.b 167+0,167+ROWS,167+2*ROWS
	dc.b 168+0,168+ROWS,168+2*ROWS
	dc.b 169+0,169+ROWS,169+2*ROWS
	dc.b 170+0,170+ROWS,170+2*ROWS
	dc.b 171+0,171+ROWS,171+2*ROWS
	dc.b 172+0,172+ROWS,172+2*ROWS
	dc.b 173+0,173+ROWS,173+2*ROWS
#if ROWS > 29
	dc.b 174+0,174+ROWS,174+2*ROWS
#endif
#if ROWS > 30
	dc.b 175+0,175+ROWS,175+2*ROWS
#endif
#if ROWS > 31
	dc.b 176+0,176+ROWS,176+2*ROWS
#endif
#if ROWS > 32
	dc.b 177+0,177+ROWS,177+2*ROWS
#endif
#if ROWS > 33
	dc.b 178+0,178+ROWS,178+2*ROWS
#endif
#if ROWS > 34
	dc.b 179+0,179+ROWS,179+2*ROWS
#endif
#if ROWS > 35
	dc.b 180+0,180+ROWS,180+2*ROWS
#endif
	dc.b 145+0,145+ROWS,145+2*ROWS

	dc.b 146+0,146+ROWS,146+2*ROWS
	dc.b 147+0,147+ROWS,147+2*ROWS



	align 64,0
hor_map:
	dc.b 0,0,0,0,0,0,0,0
	dc.b 0,1,2,3,4,5,6,7
	dc.b 8,9,10,11,12,13,14,15
	dc.b 16,17,18,19,20,21,22,23
	dc.b 24,25,26,27,28,29,30,31
	dc.b 0,0,0,0,0,0,0,0
	dc.b 32+0,32+1,32+2,32+3,32+4,32+5,32+6,32+7
	dc.b 32+8,32+9,32+10,32+11,32+12,32+13,32+14,32+15
	dc.b 32+16,32+17,32+18,32+19,32+20,32+21,32+22,32+23
	dc.b 32+24,32+25,32+26,32+27,32+28,32+29,32+30,32+31
	dc.b 0,0,0,0,0,0,0,0



columncnt_v
	dc.b 0
columncnt_h
	dc.b 0
v_code
	dc.b 145
h_code
	dc.b 0

text_v
	dc.b "                              "
	dc.b "__VIMM__  CODE AND GFX BY ALBERT OF PU[<]    "
	dc.b "THE ORIGINAL ISQI LOADER BY MARKO M^KEL^     "
	dc.b "__VICPIC__  CODE AND GFX CONVERSION BY ALBERT    ART BY BORIS VALLEJO    "
	dc.b "BABYLON_S  .C. WARNER BROS.     "
	dc.b "__TECHDYCP__  CODE AND GFX BY ALBERT    "
	dc.b "__PLASMA__  CODE BY ALBERT    "
	dc.b "__COPPER__  CODE BY ALBERT    "
	dc.b "__DOUBLECHESS__  CODE AND GFX BY ALBERT   ORIGINAL CHECKERED IDEA BY MARKO M^KEL^    "
	dc.b "__CREDITS__  CODE AND GFX BY ALBERT      "
	dc.b "THE GREAT TUNES CREATED BY ANDERS CARLSSON     "
	dc.b "DATA COMPRESSION PERFORMED BY PUCRUNCH ", 255


E_STRETCH  = 128
E_ROTATE   = 129
E_ROLL     = 130
E_MODULATE = 131
E_BLANK    = 132
E_PIPE     = 133

text_h
	dc.b "WE HAVE REACHED THE END OF VICI ITERUM MM", E_ROTATE
	dc.b "   I HOPE YOU HAVE ENJOYED WATCHING THIS DEMO. "
	dc.b "   CREDITS ARE IN THE OTHER SCROLLER        "
	dc.b "THIS IS THE LAST PART SO SOME FUNNY", E_STRETCH, "STUFF MAY "
	dc.b "BE IN ORDER.    AT LEAST I HAVE TO SHOW", E_ROTATE, "ALL DIFFERENT "
	dc.b "EFFECTS", E_ROLL, "HERE.    YOU CAN ALSO TRIGGER", E_MODULATE, "THE EFFECTS "
	dc.b "WITH KEYBOARD.      I DID NOT WANT TO "
	dc.b "MAKE MORE TRANSFORMATIONS", E_BLANK, "BECAUSE SOME OF THEM WOULD HAVE NECESSARILY "
	dc.b "BECOME", E_PIPE, "QUITE UNREADABLE.      AND BESIDES. LESS IS MORE...          "
	dc.b "REMEMBER...   NO SPRITES", E_ROTATE, "   FIVE", E_ROLL, "KB OF TOTAL GRAPHICS MEMORY"
	dc.b "  AND NO SMOOTH", E_STRETCH, "SCROLL...                   ", 255


h_mod:
	dc.b 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
	dc.b 20,21,22,23,24
#if COLUMNS2 > 25
	dc.b 25
#endif
#if COLUMNS2 > 26
	dc.b 26
#endif
#if COLUMNS2 > 27
	dc.b 27
#endif
#if COLUMNS2 > 28
	dc.b 28
#endif
	dc.b 0,1,2


#if MIDROWS > 4
fivex:
	dc.b 0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95
	dc.b 100,105,110,115,120
#if COLUMNS2 > 25
	dc.b 125
#endif
#if COLUMNS2 > 26
	dc.b 130
#endif
#if COLUMNS2 > 27
	dc.b 135
#endif
#if COLUMNS2 > 28
	dc.b 140
#endif
	dc.b 0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95
	dc.b 100,105,110,115,120
#if COLUMNS2 > 25
	dc.b 125
#endif
#if COLUMNS2 > 26
	dc.b 130
#endif
#if COLUMNS2 > 27
	dc.b 135
#endif
#if COLUMNS2 > 28
	dc.b 140
#endif
	dc.b 0,5,10,15,20,25,30

#else

fivex:
	dc.b 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76
	dc.b 80,84,88,92,96
#if COLUMNS2 > 25
	dc.b 100
#endif
#if COLUMNS2 > 26
	dc.b 104
#endif
#if COLUMNS2 > 27
	dc.b 108
#endif
#if COLUMNS2 > 28
	dc.b 112
#endif
	dc.b 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76
	dc.b 80,84,88,92,96
#if COLUMNS2 > 25
	dc.b 100
#endif
#if COLUMNS2 > 26
	dc.b 104
#endif
#if COLUMNS2 > 27
	dc.b 108
#endif
#if COLUMNS2 > 28
	dc.b 112
#endif
	dc.b 0,4,8,12,16,20

#endif



copyoverlap
	lda xvert+1
	sec
	sbc xhoriz+1
	lsr
	bcc same$
	jmp diff

same$	sec
	adc h_code
	tay

	ldx fivex,y
	lda chrmaph,x
	sta trg0$+2
	sta trg00$+2
	lda chrmapl,x
	sta trg0$+1
	sta trg00$+1

	ldx fivex+1,y
	lda chrmaph,x
	sta trg1$+2
	sta trg10$+2
	lda chrmapl,x
	sta trg1$+1
	sta trg10$+1

	ldx fivex+2,y
	lda chrmaph,x
	sta trg2$+2
	sta trg20$+2
	lda chrmapl,x
	sta trg2$+1
	sta trg20$+1

	lda v_code
	clc
	adc #(1+TOPROWS)
	cmp #145+ROWS
	bcc 1$
	;sec
	sbc #ROWS
1$	sta sub$+1
	tax
	lda chrmaph,x
	sta src0$+2
	lda chrmapl,x
	sta src0$+1

	;lda src0$+1
	clc
	adc #<(ROWS*8)
	sta src1$+1
	lda src0$+2
	adc #>(ROWS*8)
	sta src1$+2
	lda src1$+1
	clc
	adc #<(ROWS*8)
	sta src2$+1
	lda src1$+2
	adc #>(ROWS*8)
	sta src2$+2


	;	180	179	178	177	176
	;	145	180	179	178	177
	;	146	145	180	179	178
	;	147	146	145	180	179
	;	148	147	146	145	180
	;	=8	=16	=24	=32	=40

	lda #145+ROWS	; test how long 'till the wrap
	sec
sub$	sbc #176
	cmp #MIDROWS
	bcc ok$
	lda #MIDROWS
ok$	asl
	asl
	asl
	sta cmp$+1

	ldx #0
	; 00 - background, 01 - border, 11 - aux, 10 - character
plot$
src0$	lda $1400+145*8,x	;4
	eor #$55		;2
trg00$	ldy $1400+0*8,x		;4
	ora shnone,y		;4
	eor #$55		;2
trg0$	sta $1400+0*8,x		;5

src1$	lda $1400+145*8,x	;4
	eor #$55		;2
trg10$	ldy $1400+0*8,x		;4
	ora shnone,y		;4
	eor #$55		;2
trg1$	sta $1400+0*8,x		;5

src2$	lda $1400+145*8,x	;4
	eor #$55		;2
trg20$	ldy $1400+0*8,x		;4
	ora shnone,y		;4
	eor #$55		;2
trg2$	sta $1400+0*8,x		;5

	inx			;2
cmp$	cpx #MIDROWS*8		;2
	bne plot$		;3
				;= 40*70 = 2800 = 39.5 / 43 lines (PAL/NTSC)
	cpx #MIDROWS*8
	bne more$
	rts

more$	lda #<($1400+145*8)
	sec
	sbc cmp$+1
	sta src0$+1
	lda #>($1400+145*8)
	sbc #0
	sta src0$+2
	lda #<($1400+145*8+1*ROWS*8)
	sec
	sbc cmp$+1
	sta src1$+1
	lda #>($1400+145*8+1*ROWS*8)
	sbc #0
	sta src1$+2
	lda #<($1400+145*8+2*ROWS*8)
	sec
	sbc cmp$+1
	sta src2$+1
	lda #>($1400+145*8+2*ROWS*8)
	sbc #0
	sta src2$+2

	ldx cmp$+1
	lda #MIDROWS*8
	sta cmp$+1
	jmp plot$


diff	sec
	adc h_code
	tay

	ldx fivex+0,y
	lda chrmaph,x
	sta trg00$+2
	sta trg01$+2
	lda chrmapl,x
	sta trg00$+1
	sta trg01$+1

	ldx fivex+1,y
	lda chrmaph,x
	sta trg11$+2
	sta trg10$+2
	lda chrmapl,x
	sta trg11$+1
	sta trg10$+1

	ldx fivex+2,y
	lda chrmaph,x
	sta trg21$+2
	sta trg20$+2
	lda chrmapl,x
	sta trg21$+1
	sta trg20$+1

	ldx fivex+3,y
	lda chrmaph,x
	sta trg30$+2
	sta trg31$+2
	lda chrmapl,x
	sta trg30$+1
	sta trg31$+1


	lda v_code
	clc
	adc #(1+TOPROWS)
	cmp #145+ROWS
	bcc 1$
	;sec
	sbc #ROWS
1$	sta sub$+1
	tax
	lda chrmaph,x
	sta src0$+2
	lda chrmapl,x
	sta src0$+1

	;lda src0$+1
	clc
	adc #<(ROWS*8)
	sta src1$+1
	lda src0$+2
	adc #>(ROWS*8)
	sta src1$+2
	lda src1$+1
	clc
	adc #<(ROWS*8)
	sta src2$+1
	lda src1$+2
	adc #>(ROWS*8)
	sta src2$+2


	;	180	179	178	177	176
	;	145	180	179	178	177
	;	146	145	180	179	178
	;	147	146	145	180	179
	;	148	147	146	145	180
	;	=8	=16	=24	=32	=40

	lda #145+ROWS	; test how long 'till the wrap
	sec
sub$	sbc #174
	cmp #MIDROWS
	bcc ok$
	lda #MIDROWS
ok$	asl
	asl
	asl
	sta cmp$+1
	ldx #0

	; 00 - background, 01 - border, 11 - aux, 10 - character
plot$

src0$	ldy $1400+145*8,x	;4
trg00$	lda $1400+0*8,x		;4
	eor #$55		;2
	ora shright,y		;4
	eor #$55		;2
trg01$	sta $1400+0*8,x		;5

trg10$	lda $1400+0*8,x		;4
	eor #$55		;2
	ora shleft,y		;4
src1$	ldy $1400+145*8,x	;4
	ora shright,y		;4
	eor #$55		;2
trg11$	sta $1400+0*8,x		;5

trg20$	lda $1400+0*8,x		;4
	eor #$55		;2
	ora shleft,y		;4
src2$	ldy $1400+145*8,x	;4
	ora shright,y		;4
	eor #$55		;2
trg21$	sta $1400+0*8,x		;5

trg30$	lda $1400+0*8,x		;4
	eor #$55		;2
	ora shleft,y		;4
	eor #$55		;2
trg31$	sta $1400+0*8,x		;5

	inx			;2
cmp$	cpx #MIDROWS*8		;2
	bne plot$		;3
				;= 40*95 = 3800 = 53.5 / 58.5 lines (PAL/NTSC)

	; 00 - background, 01 - border, 11 - aux, 10 - character

	; 01 - black, 11 - brown, 00 - light, 10 - white
	; ^01
	; 00 - black, 10 - brown, 01 - light, 11 - white
	;    00 10 01 11
	; 00 00 10 01 11
	; 10 10 10 11 11
	; 01 01 11 01 11
	; 11 11 11 11 11

	cpx #MIDROWS*8
	bne more$
	rts

more$	lda #<($1400+145*8)
	sec
	sbc cmp$+1
	sta src0$+1
	lda #>($1400+145*8)
	sbc #0
	sta src0$+2
	lda #<($1400+145*8+1*ROWS*8)
	sec
	sbc cmp$+1
	sta src1$+1
	lda #>($1400+145*8+1*ROWS*8)
	sbc #0
	sta src1$+2
	lda #<($1400+145*8+2*ROWS*8)
	sec
	sbc cmp$+1
	sta src2$+1
	lda #>($1400+145*8+2*ROWS*8)
	sbc #0
	sta src2$+2

	ldx cmp$+1
	lda #MIDROWS*8
	sta cmp$+1
	jmp plot$



effect_cnt:
	dc.b 255
effect_type:
	dc.b 0


effects:
	ldx effect_cnt
	bmi check$
	jmp nocheck$

check$
#if 0
	lda #$7f
	sta $9122	; DDR for port B
	lda $9120	; VIA#2 port B
	bmi 0$
			; right

0$	lda #$ff
	sta $9122	; Restore DDR

	lda $9111	; VIA#1 port A
	tay
	and #4
	bne 1$
			; up

1$	tya
	and #8
	bne 2$
			; down


2$	tya
	and #16
	bne 3$
			;left

3$	tya
	and #$20	; fire ?
	bne 4$
	;
4$
#endif
	; Check keyboard	run/stop lshift x v n , / up/down
	lda $9121
	bmi 90$
	; up/down
	ldx #3
	stx effect_arg
	ldx #1
	stx effect_type
	ldx #0
	stx effect_cnt

90$	asl
	bmi 91$
	; /
	ldx #2
	stx effect_arg
	ldx #1
	stx effect_type
	ldx #0
	stx effect_cnt

91$	asl
	bmi 92$
	; ,
	ldx #1
	stx effect_arg
	stx effect_type
	ldx #0
	stx effect_cnt

92$	asl
	bmi 93$
	; n
	ldx #0
	stx effect_type
	stx effect_cnt

93$	asl
	bmi 94$
	; v
	ldx #2
	stx effect_type
	ldx #0
	stx effect_cnt

94$	asl
	bmi 95$
	; x
	ldx #4
	stx effect_arg
	ldx #3
	stx effect_type
	ldx #0
	stx effect_cnt

95$	asl
	bmi 96$
	; lshift
	ldx #4
	stx effect_arg
	ldx #5
	stx effect_type
	ldx #0
	stx effect_cnt

96$	asl
	bmi 97$
	; run/stop
	ldx #4
	stx effect_type
	ldx #0
	stx effect_cnt
97$

nocheck$


	lda effect_cnt
	bpl 5$
	rts
5$	lda effect_type
	beq effect0	; stretch
	cmp #1
	bne 6$
	jmp effect1	; rotate
6$	cmp #2
	bne 7$
	jmp effect2	; scroll
7$	cmp #3
	bne 8$
	jmp effect3	; modulate
8$	cmp #4
	bne 9$
	jmp effect4	; interleaved blank / restore
9$	jmp effect5	; pipe

effect0
	lda effect_cnt
	lsr
	lsr
	lsr
	lsr
	lsr
	and #1
	tax
	lda #0
	rol
	beq 4$

	lda #15
	sec
	sbc effect_cnt
	jmp 3$
4$	lda effect_cnt

3$	and #15
	asl
	sta size$+1
	lda eff0_la,x
	sta add$+1
	lda eff0_ya,x
	sta ya$
	lda eff0_ye,x
	sta ye$+1
	lda eff0_ls,x

	ldy eff0_ys,x
	ldx size$+1
0$	sta hor_map+8,y
	eor #32
	sta hor_map+8+40,y
	eor #32
	dex
	bpl 1$
	clc
add$	adc #0
size$	ldx #0
1$
ya$	dey
ye$	cpy #255
	bne 0$

	inc effect_cnt
	lda effect_cnt
	cmp #64
	bne 2$
	lda #255
	sta effect_cnt
2$	rts


; effect0						ys ye ya ls ladd
; 1 -  0..15	y:a  0..31 : 0..[31/l]			 0 31 +1  0 +l
; 2 - 16..31	y:a  0..31 : [31/l]..0  [reverse]	 0 31 +1 32 -l
; 3 - 32..47	y:a  31..0 : 0..[31/l]			31  0 -1  0 +l
; 4 - 48..63	y:a  31..0 : [31/l]..0  [reverse]	31  0 -1 32 -l

#if MIDROWS > 4
eff0_ys:
	dc.b 0,31
eff0_ye:
	dc.b 32,255
eff0_ya:
	dc.b $c8,$88	; c8 = iny, 88 = dey
eff0_ls:
	dc.b 0,32
eff0_la:
	dc.b 1,-1
#else
eff0_ys:
	dc.b 0,23
eff0_ye:
	dc.b 24,255
eff0_ya:
	dc.b $c8,$88	; c8 = iny, 88 = dey
eff0_ls:
	dc.b 0,24
eff0_la:
	dc.b 1,-1
#endif

effect1:
	lda #0
	ldy #31
0$	sta hor_map+8,y
	sta hor_map+8+40,y
	dey
	bpl 0$

	ldx effect_cnt
	lda eff1_ya,x
	sta ya$
	lda eff1_la,x
	sta la$+1
	ldy eff1_ys,x
#if MIDROWS > 4
	lda #8*31
#else
	lda #8*23
#endif
next$	pha
	lsr
	lsr
	lsr
	sta hor_map+8,y
	ora #32
	sta hor_map+8+40,y
ya$	dey
	pla
	sec
la$	sbc #8*32/32
	bcs next$

	lda effect_cnt
	clc
	adc effect_arg	; allowed speeds 1,2,3
	cmp #64
	bcc 1$
	lda #255
1$	sta effect_cnt
	rts

effect_arg:
	dc.b 1
#if MIDROWS > 4
eff1_ys:
	dc.b 31,31,31,31,31,31,30,29,28,26,25,24,22,20,19,18
	dc.b 15,14,13,12,10, 8, 7, 6, 4, 3, 2, 1, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 1, 1, 2, 3, 4, 6, 7, 8,10,12,13,14
	dc.b 17,18,19,20,22,24,25,26,28,29,30,31,31,31,31,31
#else
eff1_ys:
	dc.b 23,23,23,23,23,23,22,21,20,19,18,17,16,15,14,13
	dc.b 12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11
	dc.b 12,13,14,15,16,17,18,19,20,21,22,23,23,23,23,23
#endif
eff1_ya:
	dc.b $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
	dc.b $c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8
	dc.b $c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8
	dc.b $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
eff1_la:
	dc.b 8*32/32,8*32/32,8*32/31,8*32/31,8*32/30,8*32/28,8*32/27,8*32/25
	dc.b 8*32/23,8*32/20,8*32/18,8*32/15,8*32/12,8*32/9,8*32/6,8*32/3

	dc.b 255,8*32/3,8*32/6,8*32/9,8*32/12,8*32/15,8*32/18,8*32/20
	dc.b 8*32/23,8*32/25,8*32/27,8*32/28,8*32/30,8*32/31,8*32/31,8*32/32

	dc.b 8*32/32,8*32/32,8*32/31,8*32/31,8*32/30,8*32/28,8*32/27,8*32/25
	dc.b 8*32/23,8*32/20,8*32/18,8*32/15,8*32/12,8*32/9,8*32/6,8*32/3

	dc.b 255,8*32/3,8*32/6,8*32/9,8*32/12,8*32/15,8*32/18,8*32/20
	dc.b 8*32/23,8*32/25,8*32/27,8*32/28,8*32/30,8*32/31,8*32/31,8*32/32


effect2:
#if 0
	lda effect_cnt
	clc
	ldy #31
0$	and #31
	sta hor_map+8,y
	ora #32
	sta hor_map+8+40,y
	sbc #64			; obfuscated subtract by 1
	dey
	bpl 0$
#else
	; scroll old values up
	ldy #0
0$	lda hor_map+8+1,y
	sta hor_map+8+0,y
	ora #32
	sta hor_map+8+40,y
	iny
	cpy #32
	bne 0$
	; plot a new value to the end
	lda effect_cnt
	and #31
	sta hor_map+8+31
	ora #32
	sta hor_map+8+40+31
#endif

	lda effect_cnt
	clc
	adc #1
	cmp #3*32
	bcc 2$
	lda #255
2$	sta effect_cnt
	rts


effect3:
	lda effect_cnt
#if MIDROWS > 4
	and #31
#else
	sec		; modulo 24
sub$	sbc #24
	bcs sub$
	adc #24
#endif
	tax
#if MIDROWS > 4
	ldy #31
#else
	ldy #23
#endif
	tya
	jmp ok$
0$	and #31
	clc
	sbc eff3_tab,x
	bpl ok$
	lda #0
ok$	sta hor_map+8,y
	ora #32
	sta hor_map+8+40,y
	inx
	dey
	bpl 0$

	inc effect_cnt
#if MIDROWS > 4
	bpl 1$
#else
	lda effect_cnt
	cmp #120
	bne 1$
#endif
	dec effect_arg
	bmi 1$
	lda #0
	sta effect_cnt
1$	rts


effect5:
	lda effect_cnt
	and #31
#if MIDROWS > 4
	ldy #31
	ldx #16
#else
	ldy #23
	ldx #12
#endif
	jmp ok$
0$	clc
	sbc eff3_tab,x
	and #31
ok$	sta hor_map+8,y
	ora #32
	sta hor_map+8+40,y
	inx
	dey
	bpl 0$

	inc effect_cnt
	bpl 1$
	dec effect_arg
	bpl 2$
	lda #1
	sta effect_arg
	lda #2
	sta effect_type
2$	lda #0
	sta effect_cnt
1$	rts


eff3_tab:
#if MIDROWS > 4
	dc.b  0,-1,-1, 0,-1, 0,-1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0,-1, 0, 0,-1,-1,-1, 0
	dc.b  0,-1,-1, 0,-1, 0,-1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0,-1, 0, 0,-1,-1,-1, 0
#else
	; 24 = 2*2*3  32 = 2*2*2*2
	dc.b -1, 0,-1,-1, 0,-1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0,-1, 0,-1
	dc.b -1, 0,-1,-1, 0,-1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0,-1, 0,-1
#endif

eff4_tab:
	dc.b 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30
	dc.b 31,27,23,19,15,11,7,3
	dc.b 1,5,9,13,17,21,25,29


effect4:
	ldx effect_cnt
	cpx #32
	bcc off$
	txa
	eor #63
	tax
	ldy eff4_tab,x
	tya
	sta hor_map+8,y
	ora #32
	sta hor_map+8+40,y
	jmp ok$

off$	lda #0
	ldy eff4_tab,x
	sta hor_map+8,y
	sta hor_map+8+40,y

ok$	inc effect_cnt
	lda effect_cnt
	cmp #64
	bne 1$
	lda #255
	sta effect_cnt
1$	rts



earg:
	dc.b 0, 1, 0, 4, 0, 4, 0,0

seteffect:
	and #7
	tay
	lda effect_cnt
	bmi 0$
	rts
0$	sty effect_type
	lda #0
	sta effect_cnt
	lda earg,y
	sta effect_arg
	rts

p_size = 12
pacoffset
	dc.b 0
paccnt
	dc.b 8
pacman
	ldx #0
	lda paccnt
	beq 0$
	lda #0
	sta paccnt
	jmp 1$

0$	ldy h_mod+1,x
	sty pacman+1
	lda #p_size
	sta paccnt

1$	ldy pacman+1
	ldx fivex,y
	lda chrmaph,x
	sta trg00$+2
	sta trg01$+2
	lda chrmapl,x
	sta trg00$+1
	sta trg01$+1

	ldx fivex+1,y
	lda chrmaph,x
	sta trg10$+2
	sta trg11$+2
	lda chrmapl,x
	sta trg10$+1
	sta trg11$+1

2$	lda #0
	inc 2$+1
	and #31
	tax
	lda pacoffset
	clc
	adc pacsin,x
	tax		; starting target line
	stx 30$+1
	stx 31$+1
	clc
	adc #p_size
	sta cmp$+1

	lda #<pacdata+0	; compensate
	clc
	adc paccnt
	sec
30$	sbc #0
	sta src0$+1
	lda #>pacdata+0
	sbc #0
	sta src0$+2

	lda #<(pacdata+2*p_size)	; compensate
	clc
	adc paccnt
	sec
31$	sbc #0
	sta src1$+1
	lda #>(pacdata+2*p_size)
	sbc #0
	sta src1$+2


	; 00 - background, 01 - border, 11 - aux, 10 - character

src0$	lda pacdata,x		;4
	;eor #$55		;2
trg00$	ldy $1400+0*8,x		;4
	ora shnone,y		;4
	eor #$55		;2
trg01$	sta $1400+0*8,x		;5

src1$	lda pacdata+2*p_size,x		;4
	;eor #$55		;2
trg10$	ldy $1400+0*8,x		;4
	ora shnone,y		;4
	eor #$55		;2
trg11$	sta $1400+0*8,x		;5

	inx
cmp$	cpx #12
	bne src0$	; 540 cycles = 7.4 PAL lines
	rts

	align 16,0

pacdata
	dc.b $02	; 00 black, 10 dark, 01 light, 11 white
	dc.b $0a
	dc.b $0a
	dc.b $2a
	dc.b $2a
	dc.b $2a
	dc.b $2a
	dc.b $2a
	dc.b $2a
	dc.b $0a
	dc.b $0a
	dc.b $02

	dc.b $0a
	dc.b $2a
	dc.b $2a
	dc.b $a8
	dc.b $aa
	dc.b $aa
	dc.b $aa
	dc.b $aa
	dc.b $aa
	dc.b $2a
	dc.b $2a
	dc.b $02

	dc.b $a0	; 00 black, 10 dark, 01 light, 11 white
	dc.b $a8
	dc.b $aa
	dc.b $28
	dc.b $a0
	dc.b $80
	dc.b $a0
	dc.b $a8
	dc.b $aa
	dc.b $aa
	dc.b $a8
	dc.b $a0

	dc.b $80
	dc.b $a0
	dc.b $a8
	dc.b $a0
	dc.b $80
	dc.b $00
	dc.b $80
	dc.b $a0
	dc.b $a8
	dc.b $a8
	dc.b $a0
	dc.b $80


#if MIDROWS > 4
pacsin	dc.b 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
	dc.b 19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4
#else
pacsin	dc.b 2,2,3,3,4,5,6,7,8,9,10,11,12,12,13,13
	dc.b 13,13,12,12,11,10,9,8,7,6,5,4,3,3,2,2
#endif
