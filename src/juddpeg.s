;
; juddpeg -- a jpeg decoder for the c64
;
; slj 9/17/99
;
; mcm-lace version 10/1/99
; generic version 10/7/99
; last update: 11/04/99
; ifli version: 11/22/99
;

/*
rendinit	= $2000
render		= $2003
display		= $2006





		jmp start
		jmp idct2d


;--------------------------------
;
; main header processing routines
;
;--------------------------------


; and finally -- start of scan!

row		.byte 0
col		.byte 0
rowoff	.byte 0				; row offset
coloff	.byte 0				; col offset
buffpt	.word 0				; image buffer
comp	.byte 0				; current component

sos
		dec skipff			; skip $ff bytes
		jsr rendinit

; debug -- remove
;         lda $dd00
;         and #$fc
;         sta $dd00
; debug -- remove

		jsr get
		sta temp			; # of components
		sta ncomps
:l1		jsr get
		sta temp+1			; component id
		jsr get
		ldx temp+1
		pha
		and #$0f
		sta achuff,x
		pla
		lsr
		lsr
		lsr
		lsr
		sta dchuff,x
		dec temp
		bne :l1
		jsr get				; scan parameters
		jsr get				; (progressive)
		jsr get				; (ignore)

; image data begins here

		lda #00
		sta row
		sta col
		jsr restart

ready						; intensity
		ldx #1				; component
		lda #<ybuf
		ldy #>ybuf
		jsr readdu

readcb						; chrominance
		ldx ncomps
		dex
		beq readdone
		ldx #2
		lda #<cbbuf
		ldy #>cbbuf
		jsr readdu

readcr						; chrominance
		ldx ncomps
		dex
		beq readdone
		ldx #3
		lda #<crbuf
		ldy #>crbuf
		jsr readdu

readdone 
		jsr decres

		lda ateof
		bne :done
		lda csamph			; max sample
		clc
		adc col
		sta col
		cmp numcols
		bcc ready

		jsr torgb

		lda #00
		sta col

		lda #<imgbuf
		ldy #>imgbuf
		ldx csampv
		stx temp2
:rend	sta temp
		sty temp+1

		ldx row
		cpx rowoff
		bcc :norend

		jsr noscpu

		sei
		jsr render			; unto ceaser
		cli

		jsr optscpu

:norend	inc row
		lda row
		cmp numrows
		bcs :done
		sec
		sbc rowoff
		bcc :c2
		cmp #25
		bcs :done
:c2
		lda temp			; next buffer
		clc
		adc buflen
		sta temp
		lda temp+1
		adc buflen+1
		tay
		lda temp
		dec temp2
		bne :rend

:jmp	jmp ready
:done	inc ateof
		rts

;
; read in a data unit
;

; buffer size = 38*8 = $0130 * 8 lines

buflen		.word $0980

curbuf		.word 0
rend		.byte 0
currow		.byte 0
curcol		.byte 0
rendflag	.byte 0

readdu
		sta curbuf
		sty curbuf+1
		stx curcomp
		lda #00
		sta rend

		ldy #00				; compute expansion factors
		tya					; maxsamp/samp
		clc
:l1		iny
		adc csamph,x
		cmp csamph			; max
		bcc :l1
		sty hsamp
		lda #00
		tay
		clc
:l2		iny
		adc csampv,x
		cmp csampv
		bcc :l2
		sty vsamp

		lda csampv,x		; vert samp
		sta temp

:loopy	ldx curcomp
		lda csamph,x		; horiz sampling
		sta temp+1
		lda col
		sec
		sbc coloff
		sta curcol
:loopx	lda rend
		sta rendflag
		jsr fetch
		lda error
		bne :rts
		lda curcol
		clc
		adc hsamp
		sta curcol
		dec temp+1
		bne :loopx

		ldx vsamp
:next	lda curbuf			; next row
		clc
		adc buflen
		sta curbuf
		lda curbuf+1
		adc buflen+1
		sta curbuf+1
		dex
		bne :next

		dec temp
		bne :loopy
:rts	rts

;
; fetch the data
;
fetch
		lda #00
		sta dest+1
		lda curcol
		cmp #37				; catches neg too
		rol rendflag		; c set?

		asl					; offset = col*8
		rol dest+1
		asl
		rol dest+1
		asl
		rol dest+1
		adc curbuf			; ybuf, etc.
		sta dest 			; data storage
		lda curbuf+1
		adc dest+1
		sta dest+1
:decode
		jsr decodedc
		lda error
		bne :c1
		jsr decodeac
		lda error
		bne :c1
		lda rendflag
		bne :c1
		jsr dequantize
		jsr idct2d
		jmp desample

;;;; debug
;         jmp printdu  ;debug
;;;; debug

:c1		rts

;-------------------------------


*/