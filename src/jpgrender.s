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
		cmp #38
		bne put0

		lda #0											; reset screencolumn to 0, increase row until 25
		sta screencolumn
		inc screenrow
		lda screenrow
		cmp #25
		beq endscreenplot

		clc												; add 4 to get to next row
		lda put0+1
		adc #04
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc												; add 4 to get to next row
		lda put1+1
		adc #04
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

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

jpg_rend_column
		.byte 0

jpg_render

		sta jpgrnd2+1
		tya
		clc
		adc #0*19										; 0 = render Y buffer, 1 = render Cr buffer, 2 = render Cb buffer
		sta jpgrnd2+2

		lda #$00
		sta jpg_rend_column

jpgrnd_column_loop
		ldy #$00

jpgrnd_scan_loop
		ldz #$00
		ldx #$00
jpgrnd2	lda jpg_ybuf,x
		sta [uidraw_scrptr],z
		inz
		inx
		cpx #8
		bne jpgrnd2

		clc
		lda jpgrnd2+1
		adc #<(38*8)
		sta jpgrnd2+1
		lda jpgrnd2+2
		adc #>(38*8)
		sta jpgrnd2+2

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
		lda jpgrnd2+1
		sbc #<(8*$0130-8)
		sta jpgrnd2+1
		lda jpgrnd2+2
		sbc #>(8*$0130-8)
		sta jpgrnd2+2

		inc jpg_rend_column
		lda jpg_rend_column
		cmp #38
		bne jpgrnd_column_loop

		rts

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
