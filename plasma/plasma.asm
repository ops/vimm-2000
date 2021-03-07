
; Copyright © 2000 Pasi Ojala

  processor 6502


loader = $5f00
NEXTENDHI = $5fb3
NEXTPART = NEXTENDHI+1
SYSTEMSEL = NEXTPART+2
AUTO = SYSTEMSEL+1
COMMONIRQ = AUTO+1
copy = $5fc5
MUSPTR = $5fbb




offset = $fb


; 8k memory expansion needed


; $1000-1200	Video matrix 0
; $1200-1400	Video matrix 1
; $1400-1880	plasma chars	3x 3x8 x 16 bytes


NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34
RASTER	= 14	; TOPPOS

COLUMNS = 28
ROWS = 18

#else
LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER	EQU	26
RASTER  = 14	; TOPPOS

COLUMNS = 26
ROWS = 15

#endif
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm



; The BASIC line - Note: it is overwritten by the techtech data later

	.org $1201	; for the expanded Vic-20
basic	.word 0$	; link to next line
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


	.org $1400
plasmaimg:
	dc.b $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
	dc.b $40,$00,$04,$00,$40,$00,$04,$00, $40,$00,$04,$00,$40,$00,$04,$00
	dc.b $44,$00,$11,$00,$44,$00,$11,$00, $44,$00,$11,$00,$44,$00,$11,$00
	dc.b $11,$04,$11,$40,$11,$04,$11,$40, $11,$04,$11,$40,$11,$04,$11,$40
	dc.b $11,$44,$11,$44,$11,$44,$11,$44, $11,$44,$11,$44,$11,$44,$11,$44
	dc.b $11,$45,$11,$54,$11,$45,$11,$54, $11,$45,$11,$54,$11,$45,$11,$54
	dc.b $11,$55,$44,$55,$11,$55,$44,$55, $11,$55,$44,$55,$11,$55,$44,$55
	dc.b $15,$55,$51,$55,$15,$55,$51,$55, $15,$55,$51,$55,$15,$55,$51,$55

	;dc.b $00,$00,$00,$00,$00,$00,$00,$00, $40,$00,$04,$00,$40,$00,$04,$00
	;...

	;dc.b $40,$00,$04,$00,$40,$00,$04,$00, $00,$00,$00,$00,$00,$00,$00,$00
	;...

	.org $1d00

MAPWIDTH = 58

map:
#include "plasma.dat"

	.org $2200
map2:
#include "pulogo.dat"



start
	lda #0
	sta offset
	sta $900f
	;sta $9002	; 0 columns
	;sta $9003	; 0 rows
	;lda #15
	;sta $900e
	lda #$cd	; video matrix $1000, chars $1400
	sta $9005

	lda #RASTER+4
	sta $9001
	lda #SCRCENTER-COLUMNS
	sta $9000

	lda #0
	tax
1$	sta $1000,x
	sta $1100,x
	sta $1200,x
	sta $1300,x
	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	dex
	bne 1$

	; 00 - background, 01 - border, 11 - aux, 10 - character
	ldx #8*16
0$	lda plasmaimg+0 -1,x
	sta plasmaimg+6*8*16 -1,x	;00->01
	clc
	adc #$55
	and #$aa
	clc
	adc #$55
	sta plasmaimg+1*8*16 -1,x	;01->11		00>01>00>01, 01>10>10>11
	sta plasmaimg+7*8*16 -1,x
	lda plasmaimg+0 -1,x
	clc
	adc #$aa
	eor #$55
	sta plasmaimg+2*8*16 -1,x	;11->10		00>10>11, 01>11>10
	sta plasmaimg+8*8*16 -1,x
	dex
	bne 0$

	ldx #8*16
2$	lda plasmaimg+0*8*16 +7,x
	sta plasmaimg+3*8*16 -1,x
	lda plasmaimg+1*8*16 +7,x
	sta plasmaimg+4*8*16 -1,x
	lda plasmaimg+2*8*16 +7,x
	sta plasmaimg+5*8*16 -1,x
	dex
	bne 2$

	ldx #0
3$	lda plasmaimg+0*8*16 +16,x
	sta plasmaimg+6*8*16,x
	lda plasmaimg+1*8*16 +16,x
	sta plasmaimg+7*8*16,x
	lda plasmaimg+2*8*16 +16,x
	sta plasmaimg+8*8*16,x
	inx
	txa
	and #7
	bne 4$
	txa
	clc
	adc #8
	tax
4$	cpx #8*16
	bne 3$

	ldx #0				; second copy, swap
5$	lda plasmaimg+0,x
	eor #$55
	sta plasmaimg+1152+0,x
	lda plasmaimg+$80,x
	eor #$55
	sta plasmaimg+1152+$80,x
	lda plasmaimg+$100,x
	eor #$55
	sta plasmaimg+1152+$100,x
	lda plasmaimg+$180,x
	eor #$55
	sta plasmaimg+1152+$180,x
	lda plasmaimg+$200,x
	eor #$55
	sta plasmaimg+1152+$200,x
	lda plasmaimg+$280,x
	eor #$55
	sta plasmaimg+1152+$280,x
	lda plasmaimg+$300,x
	eor #$55
	sta plasmaimg+1152+$300,x
	lda plasmaimg+$380,x
	eor #$55
	sta plasmaimg+1152+$380,x
	lda plasmaimg+$400,x
	eor #$55
	sta plasmaimg+1152+$400,x
	inx
	bpl 5$

	sei
	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

;synchronize with the screen
	; If the timer is running, wait for it to finish..
;	lda $912b
;	and #$40
;	beq sync
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
pointers:
	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	;lda #$82
	;sta $911e	; enable Restore key
	cli

	jmp waiting



rowmap:
	dc.b 0*MAPWIDTH,1*MAPWIDTH,2*MAPWIDTH,3*MAPWIDTH


	; 00 - background, 01 - border, 11 - aux, 10 - character

plotplasma:
	inc xcnt$+1
xcnt$	lda #0
	asl		; 7+1 bits
	bpl frwrd0$
	eor #$fe	; 8->7 bits
frwrd0$	lsr		; 7->6 bits

	eor #1
	lsr		; 1+5 bits
	sta xypos0+1	; 0..31
	sta xypos1+1
	lda #SCRCENTER-COLUMNS+1
	sbc #0
	sta xpos+1

	inc ycnt$+1
	inc ycnt$+1
ycnt$	lda #0
	asl
	and #$fe
	bpl frwrd$
	eor #$fe
frwrd$	lsr
	lsr
	pha
	and #7
	sta sub$+1
	lda #RASTER+4
	sec
sub$	sbc #0
	sta $9001
	pla
	lsr
	lsr
	lsr
	and #3
	tay
	lda rowmap,y
	clc
	adc xypos0+1
	sta xypos0+1
	sta xypos1+1

	; Update pointers
	lda $9111	; VIA#1 port A
	and #$20	; fire?
	beq noupdate$

	inc offset
	lda offset
	and #7
	sta offset
	bne noupdate$
	ldy colcount
	iny
	cpy #42
	bcc ok$
	ldy #0
ok$	sty colcount
noupdate$
	ldy colcount
	lda plasmacols+1,y
	ora plasmacols+3,y
	and #8
	bne swap$

	lda plasmacols+0,y	; background
	asl
	asl
	asl
	asl
	ora plasmacols+1,y	; border
	sta back+1
	lda plasmacols+2,y	; aux
	asl
	asl
	asl
	asl
	sta aux+1
	lda plasmacols+3,y	; character
	ora #8
	sta m0col+1
	sta m1col+1
	bne chk$

swap$	lda offset		; border or char colors >7, use images 72..141
	clc
	adc #72
	sta offset

	lda plasmacols+1,y	; background
	asl
	asl
	asl
	asl
	ora plasmacols+0,y	; border
	sta back+1
	lda plasmacols+3,y	; aux
	asl
	asl
	asl
	asl
	sta aux+1
	lda plasmacols+2,y	; character
	ora #8
	sta m0col+1
	sta m1col+1
	;bne chk$

chk$	ldx $9002
	bmi matrix0
	jmp matrix1

matrix0
m0col	lda #0
	ldx #63
2$	sta $9400,x
	sta $9440,x
	sta $9480,x
	sta $94c0,x
	sta $9500,x
	sta $9540,x
	sta $9580,x
	sta $95c0,x
	dex
	bpl 2$	; 2880 cycles = 40.5 lines

	ldx #0
	clc
xypos0	ldy #0

loop$	lda map+0*MAPWIDTH,y
	adc offset
	sta $1000+0*COLUMNS,x
	lda map+1*MAPWIDTH,y
	adc offset
	sta $1000+1*COLUMNS,x
	lda map+2*MAPWIDTH,y
	adc offset
	sta $1000+2*COLUMNS,x
	lda map+3*MAPWIDTH,y
	adc offset
	sta $1000+3*COLUMNS,x
	lda map+4*MAPWIDTH,y
	adc offset
	sta $1000+4*COLUMNS,x
	lda map+5*MAPWIDTH,y
	adc offset
	sta $1000+5*COLUMNS,x
	lda map+6*MAPWIDTH,y
	adc offset
	sta $1000+6*COLUMNS,x
	lda map+7*MAPWIDTH,y
	adc offset
	sta $1000+7*COLUMNS,x
	lda map+8*MAPWIDTH,y
	adc offset
	sta $1000+8*COLUMNS,x
	lda map+9*MAPWIDTH,y
	adc offset
	sta $1000+9*COLUMNS,x
	lda map+10*MAPWIDTH,y
	adc offset
	sta $1000+10*COLUMNS,x
	lda map+11*MAPWIDTH,y
	adc offset
	sta $1000+11*COLUMNS,x
	lda map+12*MAPWIDTH,y
	adc offset
	sta $1000+12*COLUMNS,x
	lda map+13*MAPWIDTH,y
	adc offset
	sta $1000+13*COLUMNS,x
	lda map+14*MAPWIDTH,y
	adc offset
	sta $1000+14*COLUMNS,x
#if SYSTEM & PAL
	lda map+15*MAPWIDTH,y
	adc offset
	sta $1000+15*COLUMNS,x
	lda map+16*MAPWIDTH,y
	adc offset
	sta $1000+16*COLUMNS,x
	lda map+17*MAPWIDTH,y
	adc offset
	sta $1000+17*COLUMNS,x
#endif
	iny
	inx
	cpx #COLUMNS
	beq out$
	jmp loop$
out$	lda offset
	and #7
	sta offset
	rts


matrix1
m1col	lda #0
	ldx #63
2$	sta $9600,x
	sta $9640,x
	sta $9680,x
	sta $96c0,x
	sta $9700,x
	sta $9740,x
	sta $9780,x
	sta $97c0,x
	dex
	bpl 2$	; 2880 cycles = 40.5 lines

	ldx #0
	clc
xypos1	ldy #0

loop$	lda map+0*MAPWIDTH,y
	adc offset
	sta $1200+0*COLUMNS,x
	lda map+1*MAPWIDTH,y
	adc offset
	sta $1200+1*COLUMNS,x
	lda map+2*MAPWIDTH,y
	adc offset
	sta $1200+2*COLUMNS,x
	lda map+3*MAPWIDTH,y
	adc offset
	sta $1200+3*COLUMNS,x
	lda map+4*MAPWIDTH,y
	adc offset
	sta $1200+4*COLUMNS,x
	lda map+5*MAPWIDTH,y
	adc offset
	sta $1200+5*COLUMNS,x
	lda map+6*MAPWIDTH,y
	adc offset
	sta $1200+6*COLUMNS,x
	lda map+7*MAPWIDTH,y
	adc offset
	sta $1200+7*COLUMNS,x
	lda map+8*MAPWIDTH,y
	adc offset
	sta $1200+8*COLUMNS,x
	lda map+9*MAPWIDTH,y
	adc offset
	sta $1200+9*COLUMNS,x
	lda map+10*MAPWIDTH,y
	adc offset
	sta $1200+10*COLUMNS,x
	lda map+11*MAPWIDTH,y
	adc offset
	sta $1200+11*COLUMNS,x
	lda map+12*MAPWIDTH,y
	adc offset
	sta $1200+12*COLUMNS,x
	lda map+13*MAPWIDTH,y
	adc offset
	sta $1200+13*COLUMNS,x
	lda map+14*MAPWIDTH,y
	adc offset
	sta $1200+14*COLUMNS,x
#if SYSTEM & PAL
	lda map+15*MAPWIDTH,y
	adc offset
	sta $1200+15*COLUMNS,x
	lda map+16*MAPWIDTH,y
	adc offset
	sta $1200+16*COLUMNS,x
	lda map+17*MAPWIDTH,y
	adc offset
	sta $1200+17*COLUMNS,x
#endif
	iny
	inx
	cpx #COLUMNS
	beq out$
	jmp loop$	; 28*227 = 6356 cycles = 89.5 lines
out$	lda offset
	and #7
	sta offset
	rts


colcount:
	dc.b 0

plasmacols:
	dc.b $0,$0,$0,$2,$4,$c,$1,$c,$4,$2,$0,$6,$e,$3,$1,$1
	dc.b $3,$e,$6,$0,$5,$d,$3,$b,$1,$b,$3,$d,$5,$0,$0,$2
	dc.b $8,$4,$9,$1,$1,$1,$9,$4,$8,$2
	dc.b $0,$0,$0

	; 00 - background, 01 - border, 11 - aux, 10 - character


change:
	dc.b 0

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

	lda $9002
	and #$80
	sta $9002	; 0 columns
	lda #0
	sta $900f	; black border

xpos	lda #SCRCENTER-COLUMNS
	sta $9000
	lda $900e
	and #15
aux	ora #0
	sta $900e

#if SYSTEM & PAL
	ldx #114
0$	dex
	bne 0$
#else
	ldx #108
0$	dex
	bne 0$
#endif

	lda $9002
	and #$80
	eor #COLUMNS+$80
back	ldx #0
	sta $9002
	stx $900f

	lda #2*ROWS+1
	sta $9003

	jsr plotplasma
	jsr fade

	lda change
	beq 80$
	dec change
80$
	lda KEYWAIT+1	; next part loaded?
	bne 90$
	; Check keyboard	run/stop lshift x v n , / up/down
	lda $9121
	lsr
	bcs 90$
	; run/stop
	lda #1
	sta KEYWAIT+1
90$
	lda loader
	cmp #$8e
	bne 100$
	jmp COMMONIRQ
100$	;jmp $eb18	; return from IRQ
	pla
	tay
	pla
	tax
	pla
	rti
	;jmp $eabf	; return to normal IRQ



fadeout:
	dc.b 0
fade:
	ldx #0
	ldy #4
	lda fadeout
	bne 0$
	rts
0$
	lda map2+$000,x
	sta map+$000,x

	lda map2+$100,x
	sta map+$100,x

	lda map2+$200,x
	sta map+$200,x

	lda map2+$300,x
	sta map+$300,x

	lda map2+$400,x
	sta map+$400,x

	inx
	dey
	bne 0$
	stx fade+1

	lda fadeout
	and #31
	bne 1$

	lda $900e	; down volume..
	and #15
	sec
	sbc #1
	bmi 1$
	ora aux+1
	sta $900e

1$	dec fadeout
	rts


waiting
#if 0
	lda #0
	sta KEYWAIT+1
	jmp KEYWAIT
#endif
	lda #255
	sta KEYWAIT+1

	lda loader
1$	cmp #$8e	; stx
	bne 1$		; don't use loader...

	lda #<nextpart
	sta NEXTPART+0
	lda #>nextpart
	sta NEXTPART+1

	ldx #<nextFile
	ldy #>nextFile
	jsr loader

2$	bcs 2$

	lda #0
	sta KEYWAIT+1

	lda AUTO
	beq KEYWAIT

	ldy #2
4$	lda #255
	sta change
3$	lda KEYWAIT+1
	bne KEYWAIT	; premature exit
	lda change	; wait 5 seconds
	bne 3$
	dey
	bpl 4$		; wait 3 times 5 seconds

	inc KEYWAIT+1	; do not actually wait..
KEYWAIT	lda #0
	beq KEYWAIT


	lda #255
	sta fadeout
0$	lda fadeout
	bne 0$

	lda #0
	sta $900f	; black border
	sta $9002	; 0 columns
	sta $9003	; 0 rows

	sei

	ldy #>(musend-musstart+255)
	ldx #0
1$	lda musstart,x
2$	sta musorg,x
	inx
	bne 1$
	inc 1$+2
	inc 2$+2
	dey
	bpl 1$

player_init = $5403
;player_update = $5400
	jsr player_init		; player init sets volume.
	;lda #<player_update
	;sta MUSPTR+1
	;lda #>player_update
	;sta MUSPTR+2

	lda #<COMMONIRQ	; set the raster IRQ routine pointer
	sta $314
	lda #>COMMONIRQ
	sta $315
	cli
	jmp copy

nextFile
#if SYSTEM & PAL
	dc.b "COPPER.P"
#else
	dc.b "COPPER.N"
#endif


musstart:
;#rorg $5400
;#include "/songs/acplay.a65"
musorg = $582e
#rorg musorg
#include "/songs/songdef.i"
#include "/songs/tetris.sng"
#rend
musend:


nextpart:


