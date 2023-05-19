screencolumn	.byte 0
screenrow		.byte 0

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

		DMA_RUN_JOB clearcolorramjob

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

jpg_render

		lda #<(jpg_ybuf+1)
		sta jpgrnd2+1
		lda #>(jpg_ybuf+1)
		sta jpgrnd2+2

		ldy #$00

jpgrnd_loop
		ldz #$00
		ldx #$00

jpgrnd2	lda jpg_ybuf,x
		sta [uidraw_scrptr],z
		inz
		inx
		inx
		cpx #64*2
		bne jpgrnd2

		clc
		lda jpgrnd2+1
		adc #64*2
		sta jpgrnd2+1
		lda jpgrnd2+2
		adc #0
		sta jpgrnd2+2

		clc
		lda uidraw_scrptr+0
		adc #64
		sta uidraw_scrptr+0
		lda uidraw_scrptr+1
		adc #0
		sta uidraw_scrptr+1
		lda uidraw_scrptr+2
		adc #0
		sta uidraw_scrptr+2
		lda uidraw_scrptr+3
		adc #0
		sta uidraw_scrptr+3

		iny
		cpy #38
		bne jpgrnd_loop

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------
