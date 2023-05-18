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

		ldx #<(jpgdata/64)								; char to plot
		ldy #>(jpgdata/64)

put0	stx screen+0									; plot char low byte
put1	sty screen+1									; plot char high byte

		clc												; add 1 to char to plot
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc												; add 80 to screenpos low
		lda put0+1
		adc #80
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc												; add 80 to screenpos high
		lda put1+1
		adc #80
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

		inc screenrow									; increase screen row until 25
		lda screenrow
		cmp #25
		bne put0

		lda #0											; reset screenrow to 0, increase column until 80
		sta screenrow
		inc screencolumn
		inc screencolumn
		lda screencolumn
		cmp #80
		beq endscreenplot

		lda #>screen									; set actual value
		sta put0+2
		sta put1+2

		clc												; increase screen column by 1
		lda screencolumn
		sta put0+1
		adc #$01
		sta put1+1

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

jpg_render

		lda #<(jpg_ybuf)
		sta jpgrnd2+1
		lda #>(jpg_ybuf)
		sta jpgrnd2+2

		ldy #$00

jpgrnd_loop
		ldz #$00
		ldx #$00
:
jpgrnd2	lda jpg_ybuf,x
		sta [uidraw_scrptr],z
		inz
		inx
		inx
		cpx #64*2
		bne :-

		inc uidraw_scrptr+1
		inc jpgrnd2+2

		iny
		cpy #$0b
		bne jpgrnd_loop 

		rts