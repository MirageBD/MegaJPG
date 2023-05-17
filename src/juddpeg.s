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

dest		= $04

vsamp		= $28			; desample
hsamp		= $29

index		= $20			; idct stuff
t1			= $22
t2			= $24
t3			= $26
;coeff		= $30

crtab1		= $8400			; rgb conversion
crtab2		= $8480
cbtab1		= $8500
cbtab2		= $8580

trans		= $8600			; transform

ybuf		= imgbuf
cbbuf		= imgbuf+$1300
crbuf		= imgbuf+$2600

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

;
; convert to rgb
;

ypoint	= point
cbpoint	= dest
crpoint	= bitslo

torgb
		lda #<ybuf
		sta ypoint
		lda #>ybuf
		sta ypoint+1
		lda #<cbbuf
		sta cbpoint
		lda #>cbbuf
		sta cbpoint+1
		lda #<crbuf
		sta crpoint
		lda #>crbuf
		sta crpoint+1

		ldy #00
		ldx ncomps
		dex
		bne :loop
		ldx #>cbbuf-ybuf
:l2		lda (ypoint),y
		sta (cbpoint),y
		sta (crpoint),y
		iny
		bne :l2
		inc ypoint+1
		inc cbpoint+1
		inc crpoint+1
		dex
		bne :l2
		rts

:loop	lda #00
		sta temp+1
		lda (cbpoint),y
		eor #$80
		bpl :poscb
:negcb	eor #$ff
		clc
		adc #01
		tax
		lda (ypoint),y
		clc
		adc cbtab1,x
		sta temp
		bcc :c1
		inc temp+1			; high byte
:c1		lda (ypoint),y
		sec
		sbc cbtab2,x
		bcs :cont
		lda #00				; underflow
		beq :cont

:poscb	tax
		lda (ypoint),y
		sec
		sbc cbtab1,x
		sta temp
		bcs :c2
		dec temp+1
:c2		lda (ypoint),y
		clc
		adc cbtab2,x
		bcc :cont
		lda #255
:cont	sta temp2

		lda (crpoint),y
		eor #$80
		bpl :poscr
:negcr	eor #$ff
		clc
		adc #01
		tax
		lda temp
		clc
		adc crtab2,x
		sta temp
		lda temp+1
		adc #00
		beq :c3
		bpl :p1
		lda #00
		.byte $2c
:p1		lda #255
		.byte $2c
:c3		lda temp
		sta (cbpoint),y		; green
		lda (ypoint),y
		sec
		sbc crtab1,x
		bcs :done
		lda #00
		beq :done

:poscr	tax
		lda temp
		sec
		sbc crtab2,x
		sta temp
		lda temp+1
		sbc #00
		beq :c4
		bpl :p2
		lda #00
		.byte $2c
:p2		lda #255
		.byte $2c
:c4		lda temp
		sta (cbpoint),y
		lda (ypoint),y
		clc
		adc crtab1,x
		bcc :done
		lda #255
:done	sta (ypoint),y		; red
		lda temp2
		sta (crpoint),y		; blue
		iny
		beq :inc
:jmp	jmp :loop
:inc	inc ypoint+1
		inc cbpoint+1
		inc crpoint+1
		lda ypoint+1
		cmp #>cbbuf
		bcc :jmp
		rts








;-------------------------------
;
; idct routines
;
;-------------------------------


;trans	= $9000				; transform
							; 128 bytes

a1lo	= $2900				; cos(2a), a=pi/8
a1hi	= $2a00
a2lo	= $2b00				; cos(a)-cos(3a)
a2hi	= $2c00
a3lo	= a1lo				; cos(2a)
a3hi	= a1hi
a4lo	= $2d00				; cos(a)+cos(3a)
a4hi	= $2e00
a4gh	= $2f00
a5lo	= $3000				; cos(3a)
a5hi	= $3100

a1216	= 46341			; a1 * 2^16
a2216	= 35468			; a2 * 2^16
a3216	= a1216
a4216	= 20091			; ...
a5216	= 25080

;coeff		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
coeff	= $02a7

idct2d

; first the columns

cols
		ldx #00
:l0		stx index
		ldy #00
:l1		lda trans,x
		sta dct,y
		lda trans+1,x
		sta dct+1,y
		txa
		clc
		adc #16
		tax
		iny
		iny
		cpy #16
		bne :l1
		jsr idct
		ldy #0
		ldx index
:l1b	lda coeff,y
		sta trans,x
		lda coeff+1,y
		sta trans+1,x
		txa
		clc
		adc #16
		tax
		iny
		iny
		cpy #16
		bne :l1b
		ldx index
		inx
		inx
		cpx #16
		bcc :l0

; then the rows
rows
		ldx #00
		stx index
		stx count
:l0		ldy #00
:l1		lda trans,x
		sta dct,y
		lda trans+1,x
		sta dct+1,y
		inx
		inx
		iny
		iny
		cpy #16
		bne :l1
		stx index
		jsr idct
		ldy count
		ldx #00
:l1b	lda coeff,x
		sta bitslo
		lda coeff+1,x
		cmp #$80
		ror
		ror bitslo
		cmp #$80
		ror
		ror bitslo
		sta bitshi
		lda bitslo
		adc #128			; c determines rounding
		; sta (dest),y
		sta trans,y
		lda bitshi			; range check
		adc #00
		beq :cont
		bpl :pos
		lda #00
		.byte $2c
:pos	lda #$ff
		; sta (dest),y
		sta trans,y
:cont
		; inc dest
		iny
		inx
		inx
		cpx #16
		bne :l1b
		sty count
		ldx index
		cpx #128
		bcc :l0

		rts

;index		.word 0

;dct		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;coeff		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0


c0		= coeff
c1		= coeff+2
c2		= coeff+4
c3		= coeff+6
c4		= coeff+8
c5		= coeff+10
c6		= coeff+12
c7		= coeff+14

;t1		.word 0
;t2		.word 0
;t3		.word 0

;
; compute the inverse dct (1d)
;
; uses modified reversed flowgraph from
; pennebaker & mitchell, p. 52
;
; input: dct coeffs contained in flo/fhi
; output: original coeffs in coeffs
;

idct
		jsr prepdat			; shift and such

; stage 1: f(5) <- f(5) - f(3)
;          f(1) <- f(1) + f(7)
;          f(7) <- f(1) - f(7)
;          f(3) <- f(5) + f(3)

		lda f5
		sec
		sbc f3
		sta t1
		lda f5+1
		sbc f3+1
		sta t1+1

		lda f1
		clc
		adc f7
		sta t2
		lda f1+1
		adc f7+1
		sta t2+1

		lda f1
		sec
		sbc f7
		sta t3
		lda f1+1
		sbc f7+1
		sta t3+1

		lda f5
		clc
		adc f3
		sta f3
		lda f5+1
		adc f3+1
		sta f3+1

		lda t3
		sta f7
		lda t3+1
		sta f7+1

		lda t2
		sta f1
		lda t2+1
		sta f1+1

		lda t1
		sta f5
		lda t1+1
		sta f5+1

; stage 2: f(2) <- f(2) - f(6)
;          f(6) <- f(2) + f(6)
;          f(1) <- f(1) - f(3)
;          f(3) <- f(1) + f(3)

		lda f2
		sec
		sbc f6
		sta t1
		lda f2+1
		sbc f6+1
		sta t1+1

		lda f2
		clc
		adc f6
		sta f6
		lda f2+1
		adc f6+1
		sta f6+1

		lda t1
		sta f2
		lda t1+1
		sta f2+1

		lda f1
		sec
		sbc f3
		sta t1
		lda f1+1
		sbc f3+1
		sta t1+1

		lda f1
		clc
		adc f3
		sta f3
		lda f1+1
		adc f3+1
		sta f3+1

		lda t1
		sta f1
		lda t1+1
		sta f1+1

; stage 3: f(2) <- a1*f(2)
;          f(5) <- -a2*f(5) + t1
;          f(1) <- a3*f(1)
;          f(7) <- a4*f(7) + t1
; where t1 = -a5*(f(5) + f(7))

; f(2) <- a1*f(2)

		ldx f2        	   ; lo
		ldy f2+1      	   ; hi
		lda a1lo,y
		clc
		adc a1hi,x
		sta bitslo    	   ; lo byte
		lda a1hi,y
		adc #00
		cpy #$80
		bcc :pos1
		sta bitshi
		lda bitslo
		sbc #<a1216
		sta bitslo
		lda bitshi
		sbc #>a1216
:pos1	sta f2+1
		lda bitslo
		sta f2

; f(1) = a3*f(1)

		ldx f1       	    ; lo
		ldy f1+1     	    ; hi
		lda a3lo,y
		clc
		adc a3hi,x
		sta bitslo
		lda a3hi,y
		adc #00
		cpy #$80
		bcc :pos1b
		sta bitshi
		lda bitslo
		sbc #<a3216
		sta bitslo
		lda bitshi
		sbc #>a3216
:pos1b	sta f1+1
		lda bitslo
		sta f1

; t1 = -a5*(f(5) + f(7))

		lda f5
		clc
		adc f7
		tax					; lo
		lda f5+1
		adc f7+1
		tay					; hi
		lda a5lo,y
		clc
		adc a5hi,x
		sta bitslo
		lda a5hi,y
		adc #00
		sta bitshi
		cpy #$80
		bcc :pos2
		lda bitslo
		sbc #<a5216
		sta bitslo
		lda bitshi
		sbc #>a5216
		sta bitshi
:pos2	lda bitslo
		eor #$ff
		clc
		adc #01
		sta t1
		lda bitshi
		eor #$ff
		adc #00
		sta t1+1

; f(5) = t1 - a2*f(5)

		ldx f5				; lo
		ldy f5+1			; hi
		lda a2lo,y
		clc
		adc a2hi,x
		sta bitslo
		lda a2hi,y
		adc #00
		cpy #$80
		bcc :pos3
		sta bitshi
		lda bitslo
		sbc #<a2216
		sta bitslo
		lda bitshi
		sbc #>a2216
:pos3	sta bitshi
		lda t1
		sec
		sbc bitslo
		sta f5
		lda t1+1
		sbc bitshi
		sta f5+1

; f(7) = a4*f(7) + t1

		ldx f7				; lo
		ldy f7+1			; hi
		lda a4lo,y
		clc
		adc a4hi,x
		sta bitslo
		lda a4hi,y
		adc a4gh,x			; a4*.x can be >255
		cpy #$80
		bcc :pos4
		sta bitshi
		lda bitslo
		sbc #<a4216
		sta bitslo
		lda bitshi
		sbc #>a4216
:pos4	sta bitshi
		lda bitslo
		clc
		adc t1
		sta f7
		lda bitshi
		adc t1+1
		sta f7+1

; stage 4:
;   f(0) <- f(0) + f(4)
;   f(4) <- f(0) - f(4)
;   f(6) <- f(2) + f(6)

		lda f0
		clc
		adc f4
		sta t1
		lda f0+1
		adc f4+1
		sta t1+1

		lda f0
		sec
		sbc f4
		sta f4
		lda f0+1
		sbc f4+1
		sta f4+1

		lda t1
		sta f0
		lda t1+1
		sta f0+1

		lda f2
		clc
		adc f6
		sta f6
		lda f2+1
		adc f6+1
		sta f6+1

; stage 5:
;   f(0) <- f(0) + f(6)
;   f(4) <- f(2) + f(4)
;   f(2) <- f(4) - f(2)
;   f(6) <- f(0) - f(6)
;   f(3) <- f(3) + f(7)
;   f(7) <- f(7) + f(1)
;   f(1) <- f(1) - f(5)
;   f(5) <- -f(5)

		lda f0
		clc
		adc f6
		sta t1
		lda f0+1
		adc f6+1
		sta t1+1

		lda f0
		sec
		sbc f6
		sta f6
		lda f0+1
		sbc f6+1
		sta f6+1
		lda t1
		sta f0
		lda t1+1
		sta f0+1

		lda f4
		clc
		adc f2
		sta t1
		lda f4+1
		adc f2+1
		sta t1+1

		lda f4
		sec
		sbc f2
		sta f2
		lda f4+1
		sbc f2+1
		sta f2+1
		lda t1
		sta f4
		lda t1+1
		sta f4+1

		lda f3
		clc
		adc f7
		sta f3
		lda f3+1
		adc f7+1
		sta f3+1

		lda f7
		clc
		adc f1
		sta f7
		lda f7+1
		adc f1+1
		sta f7+1

		lda f1
		sec
		sbc f5
		sta f1
		lda f1+1
		sbc f5+1
		sta f1+1

		lda #00
		sec
		sbc f5
		sta f5
		lda #00
		sbc f5+1
		sta f5+1

; final stage:
;   c(0) = f(0) + f(3)
;   c(1) = f(4) + f(7)
;   c(2) = f(2) + f(1)
;   c(3) = f(6) + f(5)
;   c(4) = f(6) - f(5)
;   c(5) = f(2) - f(1)
;   c(6) = f(4) - f(7)
;   c(7) = f(0) - f(3)
;
; note: values are offset -128

		lda f0
		clc
		adc f3
		sta c0
		lda f0+1
		adc f3+1
		sta c0+1

		lda f4
		clc
		adc f7
		sta c1
		lda f4+1
		adc f7+1
		sta c1+1

		lda f2
		clc
		adc f1
		sta c2
		lda f2+1
		adc f1+1
		sta c2+1

		lda f6
		clc
		adc f5
		sta c3
		lda f6+1
		adc f5+1
		sta c3+1

		lda f6
		sec
		sbc f5
		sta c4
		lda f6+1
		sbc f5+1
		sta c4+1

		lda f2
		sec
		sbc f1
		sta c5
		lda f2+1
		sbc f1+1
		sta c5+1

		lda f4
		sec
		sbc f7
		sta c6
		lda f4+1
		sbc f7+1
		sta c6+1

		lda f0
		sec
		sbc f3
		sta c7
		lda f0+1
		sbc f3+1
		sta c7+1
		rts					; sheeew!



*/