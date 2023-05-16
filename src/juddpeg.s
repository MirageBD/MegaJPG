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

temp		= $fe
quantp		= $fc			; quant table
huff		= $fa			; huffman pointers
temp2		= $f9
count		= $f8			; used by getbits
							; and addnode

point		= $02
dest		= $04

bitslo		= $06			; and dequantize
bitshi		= $07

mult1lo		= $08            ; multiplication tables
mult1hi		= $0a
mult2lo		= $0c
mult2hi		= $0e

vsamp		= $28			; desample
hsamp		= $29

index		= $20			; idct stuff
t1			= $22
t2			= $24
t3			= $26
dct			= $10
;coeff		= $30

qt0			= $0340			; quantization tables
qt1			= qt0+64
qt2			= qt1+64		; only use 3

huffmem		= $0400			; huffman trees

negmlo		= $0a00
posmlo		= $0b00			; mult tables
negmhi		= $0d00
posmhi		= $0e00			; 2 pages

crtab1		= $8400			; rgb conversion
crtab2		= $8480
cbtab1		= $8500
cbtab2		= $8580

trans		= $8600			; transform

veclo		= $8680			; vec to be quantized
vechi		= $86c0

imgbuf		= $8700			; image data buffer
imgbufsize	= $3900
;emptybuf	= $9f80

ybuf		= imgbuf
cbbuf		= imgbuf+$1300
crbuf		= imgbuf+$2600

notjpg		= 1				; errors
readerr		= 2
badqt		= 3
badht		= 4
headerr		= 5
hufferr		= 6

		jmp start
		jmp idct2d

start
		lda #$76
		sta $01

		lda #00
		sta error
		sta ateof
		sta skipff

		lda #$ff
		sta filepos
		sta filepos+1
		sta filepos+2
		sta reslen
		sta reslen+1

		lda #>posmlo
		sta mult1lo+1
		lda #>negmlo
		sta mult2lo+1
		lda #>posmhi
		sta mult1hi+1
		lda #>negmhi
		sta mult2hi+1

		jsr getfile
		bcs :rts

		jsr getin			; check jpeg soi
		cmp #$ff			; marker = $ffd8
		bne :err1
		jsr getin
		cmp #$d8
		bne :err1

		jsr inithuff
		jsr initbuff
		jsr optscpu

		jsr getapp0
:loop	lda error
		bne :err
		jsr domarker
		lda ateof
		beq :loop

:rts	jsr noscpu
		jsr closefile
		lda #00
		sta $d020
		jsr display
		jmp :done

:err1	lda #notjpg
		.byte $2c
:err3	lda #readerr
		.byte $2c
:err4	lda #headerr

:err						; .a = err no
		pha
		jsr noscpu
		jsr strout
		.byte 13
		txt 'error @ $',00
		lda filepos+2
		jsr hexout
		lda filepos+1
		jsr hexout
		lda filepos
		jsr hexout
		lda #13
		jsr $ffd2
		pla
		asl
		tax
		lda errtab,x
		sta temp
		lda errtab+1,x
		sta temp+1
		jsr closefile
		lda #13
		jsr $ffd2
		ldy #00
:loop2	lda (temp),y
		beq :done
		jsr $ffd2
		iny
		bne :loop2

:done	lda #$77
		sta $01
		lda #$1b
		sta $d011
		lda #$14
		sta $d018
		lda #$08
		sta $d016
		lda $dd00
		ora #$03
		sta $dd00
		lda #13
		jsr $ffd2
		lda #$00
		sta $c6
		jsr $e3bf
		jsr $e453
		jsr $a65e
		jmp $a474

ateof	.byte 0
error	.byte 0

errtab
		.word 0
		.word notjpg
		.word readerr
		.word dqt
		.word dht
		.word head
		.word huff

notjpg	.byte "not a jpeg!",  0
readerr	.byte "read error",   0
dqt		.byte "dqt error",    0
dht		.byte "dht error",    0
head	.byte "header err",   0
huff	.byte "decoding err", 0

; bit patterns (masks)

bitp	.byte $00
		.byte $01, $02, $04,$08, $10, $20, $40, $80
		.byte $01, $02, $04,$08, $10, $20, $40, $80

;		txt 'wyn wuz here'
		.byte "-wyn-"

;-------------------------------

; zero out image buffer

initbuff
		lda #<imgbuf
		sta point
		lda #>imgbuf
		sta point+1
		ldx #>imgbufsize
		lda #$80
		ldy #$00
:loop	sta (point),y
		dey
		bne :loop
		inc point+1
		dex
		bne :loop
		rts

;
; disable scpu mirroring
;

optscpu
		lda #%10111100
		sta $d07e
		sta $d0b3
		sta $d07f
		rts

noscpu						; enable
		sta $d07e
		sta $d077
		sta $d07f
		rts

;
; domarker -- read marker and call
;   appropriate routine.
;

unknown
		jsr ignore

domarker
		lda ateof
		beq :c1
		rts
:c1		jsr getheader		; find next
		bcs domarker
do2
		lda header+1
;		cmp #$fe
;		beq :com
		cmp #$dd
		beq :dri
		cmp #$db
		beq :dqt
		cmp #$c4
		beq :dht
		cmp #$c0
		beq :sof
		cmp #$da
		bne unknown

:sos	jmp sos
;:com   jmp comment
:dri	jmp dri
:dqt	jmp dqt
:dht	jmp dht
:sof	jmp sof

;:get	jsr getheader
;		bcc :rts
;		lda #headerr
;		sta error
;:rts	rts

;
; getapp0 -- read jfif header
;

getapp0
		jsr getheader
		bcs :jmp
		lda header+1
		cmp #$e0			; app0 marker
		beq ignore

		jmp do2
:jmp	jmp domarker

; ignore rest of segment

ignore
		jsr getbyte
		bcs :rts
		lda ateof
		bne :rts
		jsr declen
		bne ignore
:rts	rts

;
; getheader -- read in header bytes.
; on exit:
;   c set -> error
;   z set -> end of file
;

header	.word 0				; hi, lo
length	.word 0				; lo, hi

getheader
		lda #00
		sta header
		sta header+1
		jsr getbyte
		bcs :rts
		cmp #$ff
		bne :sec

		jsr getbyte
		sta header+1
		cmp #$d8			; start of jpeg
		beq :clc			; lame photoshop
		cmp #$d9			; end of file
		bne :c2
		sta ateof
		beq :clc
:c2
		jsr getbyte
		bcs :rts
		sta length+1
		jsr getbyte
		bcs :rts
		sec
		sbc #2
		sta length
		bcs :c3
		dec length+1
:c3		ora length+1		; empty segment
		beq getheader
:clc	clc
		rts

:sec	sec
:rts	rts

;
; getfile -- open jpeg file.
;
getfile
		lda #$00
		sta $d020
		sta $d021
		sta nbits

		jsr strout
		.byte 13,5
		txt 'jpz-ifli v.blah slj 12/8'
		.byte 13,13
		txt 'renderer by a. gonzalez'
		.byte 13,13
		txt 'file:',00

		ldy #00
:l2		jsr $ffcf
		sta $0200,y			; filename
		iny
		cmp #13
		bne :l2
		jsr $ffd2
		lda #','
		sta $0200,y
		iny
		lda #'r'
		sta $0200,y

		tya
		ldx #00
		ldy #$02
		jsr $ffbd			; setnam

		jsr strout
		txt 'col:0'
		.byte 157, 0
		jsr getnum
		sta coloff

		jsr strout
		txt 'row:0'
		.byte 157, 0
		jsr getnum
		sta rowoff

		lda #2
		tay
		ldx $ba
		jsr $ffba
		jsr $ffc0			; open
		bcs :error
		lda $90
		bne :error
		ldx #2
		jsr $ffc6			; chkin
		bcs :error
		rts					; c set -> error

:error	jsr closefile
		jsr strout
		txt 'load error',00
		sec
:rts	rts

getnum
		lda #00
:loop2	sta temp
:loop	jsr $ffcf
		cmp #13
		beq :done
		sec
		sbc #'0'
		sta temp+1
		ldx #10				; cheesy *10 routine
		lda #00
		clc
:l2		adc temp
		dex
		bne :l2
		adc temp+1
		jmp :loop2
:done	jsr $ffd2
		lda temp
		rts

closefile
		lda #2
		jsr $ffc3			; close
		jmp $ffcc			; clrchn

;
; getbyte
;

skipff	.byte 0				; flag

getbyte
		inc $d020
		jsr getin
		sta header
		cmp #$ff
		bne :rts
		ldx skipff
		beq :rts
		jsr getin
		sta header+1
		cmp #$ff
		beq getbyte			; $ffff -> skip
		cmp #00				; $ff00 -> $ff
		bne :rts
		lda #$ff
:rts	ldx $90
		stx ateof
		cpx #64
		rts					; c set -> error

filepos	.byte 0, 0, 0

getin
		inc filepos
		bne :c1
		inc filepos+1
		bne :c1
		inc filepos+2
:c1		jmp $ffa5			; IECIN. Read byte from serial bus. (Must call TALK and TALKSA beforehands.)

;
; getbit -- get next bit!
;

nbits	.byte 0				; # of bits left
byte	.byte 0

getbit
		dec nbits
		bpl :get
		lda #7
		sta nbits
		jsr getbyte
		sta byte
:get	asl byte
:rts	rts

; print string

strout
		pla
		tay
		pla
		sta :loop+2
:loop	lda $c001,y
		beq :exit
		jsr $ffd2
		iny
		bne :loop
		inc :loop+2
		bne :loop
:exit	lda :loop+2
		cpy #$ff
		iny
		adc #00
		pha
		tya
		pha
		rts

; print hex number

hexout
		pha
		lsr
		lsr
		lsr
		lsr
		jsr :print
		pla
		and #$0f
:print	cmp #10
		bcc :c1
		adc #6
:c1		adc #48
		jmp $ffd2

;--------------------------------
;
; main header processing routines
;
;--------------------------------

declen
		lda length
		bne :c1
		ora length+1
		beq :rts
		dec length+1
:c1		dec length
		lda length
		ora length+1
:rts	rts


comment
		jsr getbyte
		bcs :rts
		cmp #$0a			; lf
		beq :oops2
:c0		cmp #32
		bcc :oops
		cmp #128
		bcc :c1
:oops	lda #'@'
		.byte $2c
:oops2	lda #13
:c1		cmp #65
		bcc :c2
		eor #32				; ->petscii
:c2		jsr $ffd2
		jsr declen
		bne comment
:rts	lda #13
		jmp $ffd2

; lame restart markers

reslen	.word 0
cres	.word 0

dri
		jsr getin
		sta reslen+1
		sta cres+1
		jsr getin
		sta reslen
		sta cres
		rts

decres
		lda reslen+1
		cmp #$ff
		beq :rts

		dec cres
		bne :rts
		lda cres+1
		beq :restart
		dec cres+1
:rts	rts

:restart
		sta nbits			; skip bits
		jsr getbyte			; read $ffxx
		lda reslen
		sta cres
		lda reslen+1
		sta cres+1
restart
		ldx #5
:l2		sta dclo,x
		sta dchi,x
		dex
		bpl :l2
		rts

; define quantization table

dqt
		jsr declen
		beq :rts
		jsr getbyte
		bcs :err
		tay
		and #$0f			; number of qt
		bne :c1
		ldx #<qt0
		lda #>qt0
		bne :ok
:c1		cmp #1
		bne :c2
		ldx #<qt1
		lda #>qt1
		bne :ok
:c2		cmp #2
		bne :err
		ldx #<qt2
		lda #>qt2

:ok		stx point			; qt addr
		sta point+1
		tya
		and #$f0
		bne :err			; 0 = 8-bit
		ldy #00
:loop	sty temp			; counter
		lda length
		ora length+1
		beq :err
		jsr getbyte
		bcs :err
		ldy temp
		sta (point),y
		jsr declen
		iny
		cpy #64
		bne :loop
		jmp dqt				; multiple qt's allowed

:err	lda #badqt			; only 0-3 allowed
		sta error
:rts	rts

; define huffman table

symbols	.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
hufflen	.byte 0

dht
		jsr declen
		beq :jerr
:getb	jsr getbyte
		bcc :cont
:jerr	jmp :err
:cont
		tay					; info byte
		and #$0f
		cmp #$04
		bcs :jerr
		asl
		tax					; table num 0-3
		tya
		and #$f0
		beq :ok				; dc table
		cmp #$10
		bne :jerr
		txa					; ac table
		ora #$08			; +8
		tax
:ok		lda hufftop
		sta dchuff0,x
		sta huff
		lda hufftop+1
		sta dchuff0+1,x
		sta huff+1
		stx temp2
		ldy #01				; right node
		jsr newnode			; root node

		ldx #01
:l1		stx temp
		lda length
		ora length+1
		beq :err
		jsr getbyte
		bcs :err
		ldx temp
		sta symbols-1,x
		jsr declen
		inx
		cpx #17
		bne :l1

		lda #$ff
		sta huffbits
		sta huffbits+1
		lda #1
		sta hufflen
:loop
		inc huffbits+1		; hi,lo!
		bne :c1
		inc huffbits
:c1
:l2		ldx hufflen
		dec symbols-1,x
		bpl :c2
		cpx #16
		beq :next
		asl huffbits+1
		rol huffbits
		inc hufflen
		bne :l2
:c2
		ldx temp2
		lda dchuff0,x
		sta huff
		lda dchuff0+1,x
		sta huff+1
		jsr getbyte
		bcs :err
		ldx hufflen
		jsr addnode
		bcs :rts
		jsr declen
		jmp :loop
:next	jsr declen
		beq :rts
		jmp :getb			; multiple hts

:err	lda #badht
		sta error
:rts	rts

; start of frame

height	.word 0
width	.word 0
numrows	.byte 0
numcols	.byte 0
ncomps	.byte 0				; num components
csampv	.byte 0,0,0,0,0,0	; sampling factors
csamph	.byte 0,0,0,0,0,0	; (horizontal)
cquant	.byte 0,0,0,0,0,0	; quantization table

sof
		ldx #5
		lda #00
:l1		sta csampv,x
		sta csamph,x
		dex
		bpl :l1

		jsr :get
		cmp #8
		beq :ok
		lda #badqt
		sta error
		rts
:ok
		jsr :get
		sta height+1
		jsr :get
		sta height
		sec
		sbc #1
		sta numrows
		lda height+1
		sbc #00
		lsr
		ror numrows
		lsr
		ror numrows
		lsr
		ror numrows
		inc numrows

		jsr :get
		sta width+1
		jsr :get
		sta width
		sec
		sbc #1				; 0..7 instead of 1..8
		sta numcols
		lda width+1
		sbc #00
		lsr
		ror numcols
		lsr
		ror numcols
		lsr
		ror numcols
		inc numcols			; 0..7 => 1 col, etc.

		jsr :get
		sta ncomps
		sta temp
:loop	jsr :get
		sta temp+1			; id
		jsr :get
		ldx temp+1
		pha
		and #$0f
		sta csampv,x
		pla
		lsr
		lsr
		lsr
		lsr
		sta csamph,x
		jsr :get
		ldx temp+1
		sta cquant,x
		dec temp
		bne :loop

		ldx #5				; find max sample
		lda #00
:l2		cmp csamph,x
		bcs :c2
		lda csamph,x
:c2		dex
		bne :l2
		sta csamph			; store in +0

		ldx #5
		lda #00
:l3		cmp csampv,x
		bcs :c3
		lda csampv,x
:c3		dex
		bne :l3
		sta csampv

		rts
:get
get
		lda length
		ora length+1
		beq :err2
		jsr declen
		jsr getbyte
		bcc :rts
:err2	pla
		pla

:err	lda #readerr
		sta error
:rts	rts

; and finally -- start of scan!

dclo	.byte 0,0,0,0,0,0	; dc coeffs
dchi	.byte 0,0,0,0,0,0

row		.byte 0
col		.byte 0
rowoff	.byte 0				; row offset
coloff	.byte 0				; col offset
buffpt	.word 0				; image buffer
comp	.byte 0				; current component

achuff	.byte 0,0,0,0,0,0	; ac table to use
dchuff	.byte 0,0,0,0,0,0	; dc table to use

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
curcomp		.byte 0
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
; decode dc coeff.
;

decodedc
		ldx curcomp			; set huffman
		lda dchuff,x
		asl
		tax
		lda dchuff0,x
		sta huff
		lda dchuff0+1,x
		sta huff+1

		jsr gethuff			; get category
		ldx error
		bne :rts
		jsr getbits			; get the bits
		ldx curcomp
		lda bitslo
		clc
		adc dclo,x
		sta dclo,x
		sta veclo
		lda dchi,x
		adc bitshi
		sta dchi,x
		sta vechi
:rts	rts

;
; decode ac coeffs
;

tmphuf	.byte 0

decodeac
		ldx curcomp			; set huffman
		lda achuff,x
		asl
		tax
		stx tmphuf

		ldy #1
:loop	sty temp2			; index
		ldx tmphuf
		lda achuff0,x
		sta huff
		lda achuff0+1,x
		sta huff+1

		jsr gethuff			; get rle len
		beq :fill
		ldx error
		bne :done
		sta count			; temp
		lsr
		lsr
		lsr
		lsr					; # of zeros
		beq :skip
:fill	tax
		lda #00
		ldy temp2
:lout	sta veclo,y
		sta vechi,y
		iny
		cpy #64
		bcs :done
		dex
		bne :lout
		sty temp2
:skip	lda count
		and #$0f			; category
		jsr getbits
		ldy temp2
		lda bitslo
		sta veclo,y
		lda bitshi
		sta vechi,y
		iny
		cpy #64
		bcc :loop
:done	rts

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

;
; retrieve .a bits and convert
; to signed number in (bitslo, bitshi)
;
sign	.byte 0

getbits
		sta count
		tax
		beq :zero
		jsr getbit
		lda #00
		bcs :c1
		lda #$ff			; 0-> negative
:c1		sta bitshi
		rol
		sta bitslo
		sta sign
		dec count
		beq :done
:loop	jsr getbit
		rol bitslo
		rol bitshi
		dec count
		bne :loop
:done	lda sign
		bpl :rts
		inc bitslo			; make 2's comp
		bne :rts
		inc bitshi
:rts	rts

:zero	sta bitslo
		sta bitshi
		rts

;
; huffman tree routines.
;
; the huffman tree is implemented as
; a series of 2-byte nodes.  left
; nodes are at huff+2, right nodes
; are at (huff) if link < $8000.
; link = $80xx means xx=leaf value,
; link = $ffxx means no right link,
; link+2 = hufftop -> no left link.
;

dchuff0		.word 00				; addresses
dchuff1		.word 00
dchuff2		.word 00
dchuff3		.word 00
achuff0		.word 00
achuff1		.word 00
achuff2		.word 00
achuff3		.word 00

hufftop		.word 00				; end of huffman tree
ty			.byte 0
tx			.byte 0

inithuff 
		lda #<huffmem
		sta hufftop
		lda #>huffmem
		sta hufftop+1
		rts

; create new node; make current node
; point to it.
;
; on entry: .y = 0 -> right node,
;            otherwise left node

newnode
		sty ty
		stx tx

		tya
		bne :skip
		lda hufftop
		sec
		sbc huff
		sta (huff),y		; point -> new node
		iny
		lda hufftop+1
		sbc huff+1
		sta (huff),y
:skip
		lda hufftop
		sta point
		clc
		adc #2
		sta hufftop
		lda hufftop+1
		sta point+1
		adc #00
		sta hufftop+1

		ldy #01
		lda #$ff
		sta (point),y		; init new node

		ldy ty
		ldx tx
		clc
		rts

;:err	lda #badht
;		sta error
;		sec
;		rts

; add a new node; .x = length
; (huff) -> tree root

huffbits	.word 0				; hi,lo
huffval		.byte 0

addnode
		sta huffval
:loop
		ldy #1
		cpx #9
		bcc :c1
		dey
:c1		lda bitp,x
		and huffbits,y
		bne :right

:left	lda huff			; check if at end
		clc
		adc #2
		pha
		tay
		lda huff+1
		adc #00
		pha
		cpy hufftop
		sbc hufftop+1
		bcc :skip1			; not a new node
		ldy #$80			; create left node
		jsr newnode
:skip1	pla
		sta huff+1
		pla
		sta huff
		jmp :dex

:right	ldy #1
		lda (huff),y		; check for rt ptr
		bpl :skip2
		dey					; .y=0 -> rt node
		jsr newnode
:skip2	ldy #00
		lda (huff),y
		clc
		adc huff
		pha
		iny
		lda (huff),y
		adc huff+1
		sta huff+1
		pla
		sta huff

:dex	dex
		bne :loop
		lda #$80
		ldy #01
		sta (huff),y		; store value
		lda huffval
		dey
		sta (huff),y		; $80xx
		clc
		rts

;
; gethuff -- get valid huffman code
;   from (huff)
;

gethuff
		ldy #01
		lda (huff),y
		cmp #$80
		beq :found

		jsr getbit
		bcs :right
		lda huff
		adc #2				; c clear
		tax
		lda huff+1
		adc #00
		tay
		cpx hufftop
		sbc hufftop+1
		bcs :err
		sty huff+1
		stx huff
		bcc gethuff

:right	ldy #01
		lda (huff),y
		bmi :err
		pha
		dey
		lda (huff),y
		clc
		adc huff
		sta huff
		pla
		adc huff+1
		sta huff+1
		bne gethuff

:found	dey
		lda (huff),y
		rts

:err	lda #hufferr
		sta error
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

sec1	= $3200
sec2	= $3400
sec3	= $3600
sec4	= $3800
sec5	= $3a00
sec6	= $3c00
sec7	= $3e00

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

f0		= dct
f1		= dct+2
f2		= dct+4
f3		= dct+6
f4		= dct+8
f5		= dct+10
f6		= dct+12
f7		= dct+14

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

;
; since the algorithm is really an fft converted into a dct, the coefficients need a little massaging before tranformation.
; specifically, f(i) = s(i)/(2cos(i*pi/16)) i=0..7 with f(0)=f(0)*2/sqrt(2), which can be combined with the first step using the table for i=4.
; these multipliers can in part be incorporated in the quantization table, but for now they're out in the open.
;

prepdat
		ldx #00
		lda #<sec4
		sta point
		lda #>sec4
		sta point+1
		lda f0
		sta bitslo
		lda f0+1
		jsr pmult
		sta f0+1
		lda bitslo
		sta f0

		ldx #00
		lda #<sec1
		sta point
		lda #>sec1
		sta point+1
		lda f1
		sta bitslo
		lda f1+1
		jsr pmult
		sta f1+1
		lda bitslo
		sta f1

		ldx #00
		lda #<sec2
		sta point
		lda #>sec2
		sta point+1
		lda f2
		sta bitslo
		lda f2+1
		jsr pmult
		sta f2+1
		lda bitslo
		sta f2

		ldx #00
		lda #<sec3
		sta point
		lda #>sec3
		sta point+1
		lda f3
		sta bitslo
		lda f3+1
		jsr pmult
		sta f3+1
		lda bitslo
		sta f3

		ldx #00
		lda #<sec4
		sta point
		lda #>sec4
		sta point+1
		lda f4
		sta bitslo
		lda f4+1
		jsr pmult
		sta f4+1
		lda bitslo
		sta f4

		ldx #00
		lda #<sec5
		sta point
		lda #>sec5
		sta point+1
		lda f5
		sta bitslo
		lda f5+1
		jsr pmult
		sta f5+1
		lda bitslo
		sta f5

		ldx #00
		lda #<sec6
		sta point
		lda #>sec6
		sta point+1
		lda f6
		sta bitslo
		lda f6+1
		jsr pmult
		sta f6+1
		lda bitslo
		sta f6

		ldx #00
		lda #<sec7
		sta point
		lda #>sec7
		sta point+1
		lda f7
		sta bitslo
		lda f7+1
		jsr pmult
		sta f7+1
		lda bitslo
		sta f7

		rts

pmult						; exit .a = bitshi
		bmi :neg
		beq :ok
:l1		inx					; shift count
		lsr
		ror bitslo
		cmp #00
		bne :l1
:ok		sta bitshi
		lda bitslo
		asl
		rol bitshi
		adc point
		sta point
		lda bitshi
		adc point+1
		sta point+1
		ldy #00
		lda (point),y
		sta bitslo
		iny
		lda (point),y
		dex
		bmi :rts
:l1b	asl bitslo
		rol
		dex
		bpl :l1b
:rts	rts

:neg	sta bitshi
		lda #00
		sec
		sbc bitslo
		sta bitslo
		lda #00
		sbc bitshi
		beq :ok2
:l2		inx					; shift count
		lsr
		ror bitslo
		cmp #00
		bne :l2
:ok2	asl bitslo
		rol
		sta bitshi
		lda bitslo
		adc point
		sta point
		lda bitshi
		adc point+1
		sta point+1
		ldy #00
		lda (point),y
		sta bitslo
		iny
		lda (point),y
		dex
		bmi :rts2
:l2b	asl bitslo
		rol
		dex
		bpl :l2b
:rts2	sta bitshi
		lda #00
		sec
		sbc bitslo
		sta bitslo
		lda #00
		sbc bitshi
		rts

*/