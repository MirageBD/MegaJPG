screencolumn	.byte 0
screenrow		.byte 0

reversenibble_tmp	.byte 0
reversenibble_tmp2	.byte 0

reversenibble
		sta reversenibble_tmp
		lsr
		lsr
		lsr
		lsr
		sta reversenibble_tmp2
		lda reversenibble_tmp
		asl
		asl
		asl
		asl
		ora reversenibble_tmp2
		rts

jpg_rend_preppalette									; bring ui palette into 'normal' 0-255 range
		ldx #$00
:		lda uipal+$0000,x
		jsr reversenibble
		sta uipal+$0000,x
		lda uipal+$0100,x
		jsr reversenibble
		sta uipal+$0100,x
		lda uipal+$0200,x
		jsr reversenibble
		sta uipal+$0200,x
		inx
		bne :-
		rts

jpg_rendinit
		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #%00000000									; set H640, V400
		sta $d031

		lda #$00
		sta $d05b										; Set display to V400
		lda #25
		sta $d07b										; Display 25 rows of text

		lda #<screen									; set pointer to screen ram
		sta $d060
		lda #>screen
		sta $d061
		lda #(screen & $ff0000) >> 16
		sta $d062
		lda #$00
		sta $d063

		DMA_RUN_JOB jpgrender_clearcolorramjob

		; --------------------------------------------- set palette

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

		ldx #$00
:		txa
		jsr reversenibble
		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		;lda #$ff										; red signifies transparency
		;jsr reversenibble
		;sta $d1ff
		;lda #$00
		;jsr reversenibble
		;sta $d2ff
		;sta $d3ff

		lda $d070
		and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		sta $d070

		; --------------------------------------------- end set palette

		lda #$00
		sta screencolumn
		sta screenrow

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

		;clc												; add 4 to get to next row
		;lda put0+1
		;adc #04
		;sta put0+1
		;lda put0+2
		;adc #0
		;sta put0+2

		;clc												; add 4 to get to next row
		;lda put1+1
		;adc #04
		;sta put1+1
		;lda put1+2
		;adc #0
		;sta put1+2

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

		sta jpgrnd_red+1
		sta jpgrnd_green+1
		sta jpgrnd_blue+1
		tya
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

jpgrnd_scan_loop
		ldz #$00
		ldx #$00

jpgrend_getrgb

jpgrnd_red
		lda $babe,x
		sta jpg_rend_red
jpgrnd_green
		lda $babe,x
		sta jpg_rend_green
jpgrnd_blue
		lda $babe,x
		sta jpg_rend_blue

		jsr jpgrend_getnearestindex
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

		clc
		lda uidraw_scrptr+0
		adc #$08
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

		iny
		cpy #8
		bne jpgrnd_scan_loop

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

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpgrend_nearestindex	.byte 0

jpgrend_getnearestindex

		lda #$00
		sta jpgrend_nearestindex

/*
		phx

		ldx #$00
:		lda jpg_rend_red
		cmp uipal+0*$0100,x
		lda jpg_rend_green
		cmp uipal+1*$0100,x
		lda jpg_rend_blue
		cmp uipal+2*$0100,x
		inx
		bne :-

		plx
*/

		lda jpg_rend_red
		sta jpgrend_nearestindex

		lda jpgrend_nearestindex

		rts
		
; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_bayer_matrix
		.byte  37, 139,  63, 165,  37, 139,  63, 165
		.byte 190,  88, 216, 114, 190,  88, 216, 114
		.byte  63, 165,  37, 139,  63, 165,  37, 139
		.byte 216, 114, 190,  88, 216, 114, 190,  88
		.byte  37, 139,  63, 165,  37, 139,  63, 165
		.byte 190,  88, 216, 114, 190,  88, 216, 114
		.byte  63, 165,  37, 139,  63, 165,  37, 139
		.byte 216, 114, 190,  88, 216, 114, 190,  88

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
