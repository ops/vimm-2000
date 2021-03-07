jump_code	equ	$40
repeat_code     equ     $80
transpose_code  equ     $c0
stop_code       equ     $fe
restart_code    equ     $ff

inst_code	equ	$40
blend_code      equ     $7e
silence		equ	$7f
dur_code        equ     $80

	mac instr			; select instrument (1-62)
	.byte inst_code+{1}-1	
	endm

        mac quiet     ;play a silent tone for the preset duration
        .byte silence
        endm

	mac bd
	.byte 40
	endm
	mac sd
	.byte 41
	endm
	mac hh
	.byte 42
	endm

        mac blockend                    ;used to mark end of block.
        .byte blend_code                 ;don't forget it!
        endm

        mac rept                        ;repeat next block (0-63) times
        .byte repeat_code+{1}-1	; may not start the track
        endm
        
        mac transpose     ;transpose following blocks (0-61) steps. Only up.
        .byte transpose_code+{1}       ;0 is no transpose.
        endm
        
        mac stop          ;must have at least 1 more data in track
        .byte stop_code  ;before final stop (2 stops will do).
        endm                    ;Now it's a feature, not a bug!
        
        mac restart
        .byte restart_code
        endm

	mac jump	; jump to track 0-63 in the track list
	.byte jump_code+{1}
	endm
        
        mac block
        .byte {1}
        endm
