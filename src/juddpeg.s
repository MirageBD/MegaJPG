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

quantp		= $fc			; quant table

vsamp		= $28			; desample
hsamp		= $29



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

linelen		.word $0130
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

;
; dequantize the vector vec
;
; mult is 16 bit signed x 8 bit unsigned
; with 16-bit result, so sign etc. are
; taken care of automatically.
;
; result -> trans
;
quanttab
		.word qt0
		.word qt1
		.word qt2

; table to un-zigzag coeffs
; multiples of 2, since 2 byte result.

zigzag
		.byte   0,   2,   16,  32,  18,   4,   6,  20
		.byte  34,  48,   64,  50,  36,  22,   8,  10
		.byte  24,  38,   52,  66,  80,  96,  82,  68
		.byte  54,  40,   26,  12,  14,  28,  42,  56
		.byte  70,  84,   98, 112, 114, 100,  86,  72
		.byte  58,  44,   30,  46,  60,  74,  88, 102
		.byte  116, 118, 104,  90,  76,  62,  78,  92
		.byte  106, 120, 122, 108,  94, 110, 124, 126

dequantize
		ldx curcomp
		lda cquant,x
		asl
		tax
		lda quanttab,x
		sta quantp
		lda quanttab+1,x
		sta quantp+1

		ldx #63
:loop	txa
		tay
		lda (quantp),y
		sta mult1lo
		sta mult1hi
		eor #$ff
		clc
		adc #1
		sta mult2lo
		sta mult2hi

		ldy veclo,x
		bne :c1
		sty bitslo
		sty bitshi
		beq :high
:c1
		lda (mult1lo),y
		sec
		sbc (mult2lo),y
		sta bitslo
		lda (mult1hi),y
		sbc (mult2hi),y
		sta bitshi

:high	ldy vechi,x
		lda (mult1lo),y
		sec
		sbc (mult2lo),y
		clc
		adc bitshi

		ldy zigzag,x		; un-zigzag
		iny
		sta trans,y
		dey
		lda bitslo
		sta trans,y
		dex
		bpl :loop
		rts

;
; desample -- expand dct square by sample factor and reorg data.
; on entry: dest = destination buffer
;

;curpos		.word 0			; buffer position
;bufcol		.byte 0			; buffer column

desample 
		lda #00
:newrow	ldx vsamp
		stx huff			; temporary
:oldrow	sta temp2			; current element
		lda #8
		sta count			; column
		ldy #00
:l1		ldx temp2
		lda trans,x
		ldx hsamp
:expand
		sta (dest),y
		iny
		dex
		bne :expand
		inc temp2
		dec count
		bne :l1

		lda dest			; next scanline
		clc
		adc linelen
		sta dest
		lda dest+1
		adc linelen+1
		sta dest+1

		lda temp2
		sec
		sbc #8				; start of row
		dec huff			; horizonal sampling
		bne :oldrow
		lda temp2
		cmp #64
		bne :newrow
		rts



*/