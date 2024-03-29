screencolumn	.byte 0
screenrow		.byte 0

reversenibble_tmp	.byte 0
reversenibble_tmp2	.byte 0

jpg_rend_foo
		.repeat 42
			.byte 0, 51, 102, 153, 204, 255
		.endrepeat

jpg_rendinit

		lda #$00
		sta $d015

		DMA_RUN_JOB jpgrender_clearbitmapjob
		DMA_RUN_JOB jpgrender_clearcolorramjob

		lda #$00
		sta screencolumn
		sta screenrow

		lda #<(screen+0)
		sta put0+1
		lda #>(screen+0)
		sta put0+2
		lda #<(screen+1)
		sta put1+1
		lda #>(screen+1)
		sta put1+2

		ldx #<(jpgdata/64)								; char to plot
		ldy #>(jpgdata/64)

put0	stx screen+0									; plot left of 2 chars
put1	sty screen+1									; plot right of 2 chars

		clc												; add 1 to char to plot
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc												; add 2 to screenpos low
		lda put0+1
		adc #02
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc												; add 2 to screenpos high
		lda put1+1
		adc #02
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

		inc screencolumn								; increase screen column until 40
		lda screencolumn
		cmp #jpg_bufwidth
		bne put0

		lda #0											; reset screencolumn to 0, increase row until 25
		sta screencolumn
		inc screenrow
		lda screenrow
		cmp #25
		beq endscreenplot

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #%00000000									; set H320, V200
		sta $d031

		lda #$01
		sta $d05b										; Set display to V200
		lda #25
		sta $d07b										; Display 25 rows of text

		; initialize multiply units

		lda #$00
		sta $d770
		sta $d771
		sta $d772
		sta $d773

		lda #$00
		sta $d774
		sta $d775
		sta $d776
		sta $d777

		jmp put0

endscreenplot

		lda #<.loword(jpgdata)
		sta uidraw_scrptr+0
		lda #>.loword(jpgdata)
		sta uidraw_scrptr+1
		lda #<.hiword(jpgdata)
		sta uidraw_scrptr+2
		lda #>.hiword(jpgdata)
		sta uidraw_scrptr+3

        rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_rend_red		.byte 0
jpg_rend_green		.byte 0
jpg_rend_blue		.byte 0

jpg_rend_column		.byte 0

jpg_render

		;lda #$00
		sta jpgrnd_red+1
		sta jpgrnd_green+1
		sta jpgrnd_blue+1
		tya												; y is either $84 or $8e (jpg_imgbuf or jpg_imgbuf + $1400/2)
		clc
		adc #>(0*jpg_channelbufsize)
		sta jpgrnd_red+2
		clc
		adc #>(1*jpg_channelbufsize)
		sta jpgrnd_green+2
		clc
		adc #>(1*jpg_channelbufsize)
		sta jpgrnd_blue+2


		lda #$00
		sta jpg_rend_column

jpgrnd_column_loop
		ldy #$00
		ldz #$00

jpgrnd_scan_loop
		ldx #$00

jpgrend_getrgb

		phy

jpgrnd_red
		lda $babe,x
		tay
		lda jpg_snaptable,y
		sta $d770
		lda #36
		sta $d774
		lda $d778+0
		sta jpg_rend_red

jpgrnd_green
		lda $babe,x
		tay
		lda jpg_snaptable,y
		sta $d770
		lda #6
		sta $d774
		lda $d778+0
		sta jpg_rend_green

jpgrnd_blue
		lda $babe,x
		tay
		lda jpg_snaptable,y
		clc
		adc jpg_rend_green
		adc jpg_rend_red
		adc #$27										; add 27 to get to front of 216 web palette

		ply

		sta [uidraw_scrptr],z

		inz
		inx
		cpx #8
		bne jpgrend_getrgb

		clc
		lda jpgrnd_red+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_red+1
		lda jpgrnd_red+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_red+2

		clc
		lda jpgrnd_green+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_green+1
		lda jpgrnd_green+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_green+2

		clc
		lda jpgrnd_blue+1
		adc #<(jpg_bufwidth*8)
		sta jpgrnd_blue+1
		lda jpgrnd_blue+2
		adc #>(jpg_bufwidth*8)
		sta jpgrnd_blue+2

		iny
		cpy #8
		bne jpgrnd_scan_loop

		clc											; add 64 to get to the next character
		lda uidraw_scrptr+0
		adc #64
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #$00
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #$00
		sta uidraw_scrptr+2
		lda uidraw_scrptr+3
		adc #$00
		sta uidraw_scrptr+3

		sec
		lda jpgrnd_red+1
		sbc #<(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_red+1
		lda jpgrnd_red+2
		sbc #>(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_red+2

		sec
		lda jpgrnd_green+1
		sbc #<(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_green+1
		lda jpgrnd_green+2
		sbc #>(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_green+2

		sec
		lda jpgrnd_blue+1
		sbc #<(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_blue+1
		lda jpgrnd_blue+2
		sbc #>(jpg_bufwidth*8*8 - 8)
		sta jpgrnd_blue+2

		inc jpg_rend_column
		lda jpg_rend_column
		cmp #jpg_bufwidth
		beq :+
		jmp jpgrnd_column_loop

:		
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_bayer_matrix
/*
		.byte  37, 139,  63, 165,  37, 139,  63, 165
		.byte 190,  88, 216, 114, 190,  88, 216, 114
		.byte  63, 165,  37, 139,  63, 165,  37, 139
		.byte 216, 114, 190,  88, 216, 114, 190,  88
		.byte  37, 139,  63, 165,  37, 139,  63, 165
		.byte 190,  88, 216, 114, 190,  88, 216, 114
		.byte  63, 165,  37, 139,  63, 165,  37, 139
		.byte 216, 114, 190,  88, 216, 114, 190,  88
*/
		.byte 128, 237, 156, 255, 128, 237, 156, 255
		.byte 255, 182, 255, 210, 255, 182, 255, 210
		.byte 156, 255, 128, 237, 156, 255, 128, 237
		.byte 255, 210, 255, 182, 255, 210, 255, 182
		.byte 128, 237, 156, 255, 128, 237, 156, 255
		.byte 255, 182, 255, 210, 255, 182, 255, 210
		.byte 156, 255, 128, 237, 156, 255, 128, 237
		.byte 255, 210, 255, 182, 255, 210, 255, 182

jpg_snaptable
		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		.byte 0,0,0,0,0,0,0,0,0,0
		.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		.byte 1,1,1,1,1,1,1,1,1,1
		.byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
		.byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
		.byte 2,2,2,2,2,2,2,2,2,2
		.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
		.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
		.byte 3,3,3,3,3,3,3,3,3,3
		.byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
		.byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
		.byte 4,4,4,4,4,4,4,4,4,4
		.byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
		.byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
		.byte 5,5,5,5,5,5,5,5,5,5

		.byte 5,5,5,5

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_render_irq

		php
		pha
		phx
		phy
		phz
		
		lda main_event
		beq :+

		;inc $d020

:		jsr mouse_update
		jsr keyboard_update

		lda mouse_released
		beq :+

		lda #$02
		sta main_event
		bra :++

:		lda keyboard_shouldsendreleaseevent
		beq :+

		lda #$02
		sta main_event

:		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (SAFE_COLOR_RAM) >> 20				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM) & $ffff					; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.word $00ff										; ff = red = transparency. this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000								; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrender_clearbitmapjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (jpgdata) >> 20						; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 320*200									; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (jpgdata) & $ffff							; Destination Address LSB + Destination Address MSB
				.byte (((jpgdata) >> 16) & $0f)					; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000
