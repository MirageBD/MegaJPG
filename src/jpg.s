
; https://web.archive.org/web/20120403212223/http://class.ee.iastate.edu/ee528/Reading%20material/JPEG_File_Format.pdf

/*
	; ----------------------------------------------------------------------------------------------------------------------------------------

	Each segment starts with $FF identifier byte

		Format of each segment:

			0xFF										1 byte		identifies segment
			Segment Type								1 byte		type of segment (see Segment section below)

			sizehi, sizelo								(optional, based on type of segment) 2 bytes size of the segment, including these two bytes (but not including the 0xFF and the the segment type)

			Contents of the segment						(optional, based on type of segment) max. 65533 bytes.

	; ----------------------------------------------------------------------------------------------------------------------------------------

	Segment types:

		SOI		0xD8	Start Of Image

		APP0	0xE0	JFIF APP0 segment marker,
		APP15	0xEF	ignore

		SOF0	0xC0	Start Of Frame (baseline JPEG), for details see below
		SOF1	0xC1	Start Of Frame (baseline JPEG), for details see below
		SOF2	0xC2	usually unsupported
		SOF3	0xC3	usually unsupported
		SOF5	0xC5	usually unsupported
		SOF6	0xC6	usually unsupported
		SOF7	0xC7	usually unsupported
		SOF9	0xC9	for arithmetic coding, usually unsupported
		SOF10	0xCA	usually unsupported
		SOF11	0xCB	usually unsupported

		SOF13	0xCD	usually unsupported
		SOF14	0xCE	usually unsupported
		SOF15	0xCF	usually unsupported

		DHT		0xC4	Define Huffman Table
		DQT		0xDB	Define Quantization Table
		SOS		0xDA	Start Of Scan

		JPG		0xC8	undefined/reserved (causes decoding error)
		JPG0	0xF0	ignore (skip)
		JPG13	0xFD	ignore (skip)

		DAC		0xCC	Define Arithmetic Table, usually unsupported

		DNL		0xDC	usually unsupported, ignore
		DRI		0xDD	Define Restart Interval, for details see below
		DHP		0xDE	ignore (skip)
		EXP		0xDF	ignore (skip)

		*RST0	0xD0	RSTn are used for resync, may be ignored
		*RST1	0xD1
		*RST2	0xD2
		*RST3	0xD3
		*RST4	0xD4
		*RST5	0xD5
		*RST6	0xD6
		*RST7	0xD7

		*TEM	0x01	usually causes a decoding error, may be ignored

		COM		0xFE	Comment, may be ignored

		EOI		0xD9	End Of Image 

	; ----------------------------------------------------------------------------------------------------------------------------------------

	DQT													Define Quantization Table = $DB marker:

		QT information									1 byte
															bit 0..3: number of QT (0..3, otherwise error)
															bit 4..7: precision of QT, 0 = 8 bit, otherwise 16 bit

		Bytes											n bytes		This gives QT values, n = 64 * (precision+1)

		Remarks:
			• A single DQT segment may contain multiple QTs, each with its own information byte.
			• For precision=1 (16 bit), the order is high-low for each of the 64 words. 

	; ----------------------------------------------------------------------------------------------------------------------------------------

	SOF0												Start Of Frame = $C0 marker:

		Data precision									1 byte		This is in bits/sample, usually 8 (12 and 16 not supported by most software).
		Image height									2 bytes		This must be > 0
		Image Width										2 bytes		This must be > 0
		Number of components							1 byte		Usually 1 = grey scaled, 3 = color YcbCr or YIQ	4 = color CMYK

		Components										3 bytes each:
			component Id								1 byte		(1 = Y, 2 = Cb, 3 = Cr, 4 = I, 5 = Q)
			sampling factors							1 byte		(bit 0-3 vertical., 4-7 horizontal.)
			quantization table number					1 byte 

	; ----------------------------------------------------------------------------------------------------------------------------------------

	DHT													Define Huffman Table = $C4 marker:

		HT information									1 byte
															bit 0..3	number of HT (0..3, otherwise error)
															bit 4		type of HT, 0 = DC table, 1 = AC table
															bit 5..7	not used, must be 0

		Number of Symbols								16 bytes Number of symbols with codes of length 1..16, the sum(n) of these bytes is the total number of codes, which must be <= 256
		
		Symbols											n bytes Table containing the symbols in order of increasing	code length ( n = total number of codes ).

	Remarks: A single DHT segment may contain multiple HTs, each with its own information byte.

	; ----------------------------------------------------------------------------------------------------------------------------------------

	SOS													Start Of Scan = $DA marker:

		Number of Components in scan					1 byte This must be >=1 and <=4 (otherwise error), usually 1 or 3

		Each component									2 bytes For each component, read 2 bytes
			Component id								1 byte (1=Y, 2=Cb, 3=Cr, 4=I, 5=Q)
			Huffman table to use						1 byte
															bit 0..3: AC table (0..3)
															bit 4..7: DC table (0..3)

			Ignorable Bytes								3 bytes

c	; ----------------------------------------------------------------------------------------------------------------------------------------






; ---------------------------------------------------------------------------------------------------------------------------------------- SOI - Start Of Image = $D8

	FF D8

; ---------------------------------------------------------------------------------------------------------------------------------------- APP0 - segment marker = $E0

	FF E0
	00 10
	4A 46 49 46 00										"JFIF", 0
	01 01												JFIF format revision		01 01
	00													units for resolution		0
	00 01												horizontal resolution		1
	00 01												vertical resolution			1
	00													xthumbnail					0
	00													ythumbnail					0

; ---------------------------------------------------------------------------------------------------------------------------------------- COM - Comment (may be ignored) = $FE

	FF FE
	00 4A
	0A 0A 43 52 45 41 54 4F 52 3A 20 58 56 20 56 65		..CREATOR: XV Ve
	72 73 69 6F 6E 20 33 2E 31 30 61 20 20 52 65 76		rsion 3.10a  Rev
	3A 20 31 32 2F 32 39 2F 39 34 20 20 51 75 61 6C		: 12/29/94  Qual
	69 74 79 20 3D 20 37 35 2C 20 53 6D 6F 6F 74 68		ity = 75, Smooth
	69 6E 67 20 3D 20 30 0A								ing = 0.

; ---------------------------------------------------------------------------------------------------------------------------------------- DQT - quantisation table = $DB

	FF DB 												
	00 43
	00													QT table 0
	08 06 06 07 06 05 08 07 07 07 09 09 08 0A 0C 14
	0D 0C 0B 0B 0C 19 12 13 0F 14 1D 1A 1F 1E 1D 1A
	1C 1C 20 24 2E 27 20 22 2C 23 1C 1C 28 37 29 2C
	30 31 34 34 34 1F 27 39 3D 38 32 3C 2E 33 34 32

	FF DB
	00 43
	01													QT table 1
	09 09 09 0C 0B 0C 18 0D 0D 18 32 21 1C 21 32 32
	32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32
	32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32
	32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32

; ---------------------------------------------------------------------------------------------------------------------------------------- SOF0 - Start Of Frame = $C0

	FF C0
	00 11
	08													data precision			8
	00 E8												image height			232
	01 22												image width				290
	03													number of components	3
		01 22 00											component				Y		sampling factors: 2 horizontal, 2 vertical		quantization table: 0
		02 11 01											component				Cb		sampling factors: 1 horizontal, 1 vertical		quantization table: 1
		03 11 01											component				Cr		sampling factors: 1 horizontal, 1 vertical		quantization table: 1

	FF C4												0xC4 (DHT - Define Huffman Table)
	00 1F												
	00													Huffman table 0
	00 01 05 01 01 01 01 01 01 00 00 00 00 00 00 00	
	00 01 02 03 04 05 06 07 08 09 0A 0B				

; ---------------------------------------------------------------------------------------------------------------------------------------- DHT - Define Huffman Table = $C4

	FF C4
	00 B5												
	10													Huffman table 2
	00 02 01 03 03 02 04 03 05 05 04 04 00 00 01 7D	
	01 02 03 00 04 11 05 12 21 31 41 06 13 51 61 07	
	22 71 14 32 81 91 A1 08 23 42 B1 C1 15 52 D1 F0
	24 33 62 72 82 09 0A 16 17 18 19 1A 25 26 27 28
	29 2A 34 35 36 37 38 39 3A 43 44 45 46 47 48 49
	4A 53 54 55 56 57 58 59 5A 63 64 65 66 67 68 69
	6A 73 74 75 76 77 78 79 7A 83 84 85 86 87 88 89
	8A 92 93 94 95 96 97 98 99 9A A2 A3 A4 A5 A6 A7
	A8 A9 AA B2 B3 B4 B5 B6 B7 B8 B9 BA C2 C3 C4 C5
	C6 C7 C8 C9 CA D2 D3 D4 D5 D6 D7 D8 D9 DA E1 E2
	E3 E4 E5 E6 E7 E8 E9 EA F1 F2 F3 F4 F5 F6 F7 F8
	F9 FA

	FF C4
	00 1F												
	01													Huffman table 1
	00 03 01 01 01 01 01 01 01 01 01 00 00 00 00 00	
	00 01 02 03 04 05 06 07 08 09 0A 0B				

	FF C4
	00 B5
	11													Huffman table 3
	00 02 01 02 04 04 03 04 07 05 04 04 00 01 02 77
	00 01 02 03 11 04 05 21 31 06 12 41 51 07 61 71
	13 22 32 81 08 14 42 91 A1 B1 C1 09 23 33 52 F0
	15 62 72 D1 0A 16 24 34 E1 25 F1 17 18 19 1A 26
	27 28 29 2A 35 36 37 38 39 3A 43 44 45 46 47 48
	49 4A 53 54 55 56 57 58 59 5A 63 64 65 66 67 68
	69 6A 73 74 75 76 77 78 79 7A 82 83 84 85 86 87
	88 89 8A 92 93 94 95 96 97 98 99 9A A2 A3 A4 A5
	A6 A7 A8 A9 AA B2 B3 B4 B5 B6 B7 B8 B9 BA C2 C3
	C4 C5 C6 C7 C8 C9 CA D2 D3 D4 D5 D6 D7 D8 D9 DA
	E2 E3 E4 E5 E6 E7 E8 E9 EA F2 F3 F4 F5 F6 F7 F8
	F9 FA

; ---------------------------------------------------------------------------------------------------------------------------------------- SOS - Start Of Scan = $DA

	FF DA
	00 0C
	03													number of components = 3
		01 00												component				Y	huffman AC table: 0		huffman DC table: 0
		02 11												component				Cb	huffman AC table: 1		huffman DC table: 1
		03 11												component				Cr	huffman AC table: 1		huffman DC table: 1

	00 3F 00											ignore

; ---------------------------------------------------------------------------------------------------------------------------------------- SCANS START HERE

	00 01 02 03 04 05 06 07 08

; ---------------------------------------------------------------------------------------------------------------------------------------- END OF FILE

*/

jpg_consolelog			.repeat 80
							.byte 0
						.endrepeat

jpg_error				.byte 0
jpg_eof					.byte 0
jpg_skipff				.byte 0

jpg_filepos				.byte 0, 0, 0				; can probably get rid of this
jpg_reslen				.word 0						; lame restart markers

.define jpg_bitslo		$06							; and dequantize
.define jpg_bitshi		$07

.define jpg_mult1lo		$08							; multiplication tables
.define jpg_mult1hi		$0a
.define jpg_mult2lo		$0c
.define jpg_mult2hi		$0e

.define jpg_dct			$10

.define jpg_f0			jpg_dct+0
.define jpg_f1			jpg_dct+2
.define jpg_f2			jpg_dct+4
.define jpg_f3			jpg_dct+6
.define jpg_f4			jpg_dct+8
.define jpg_f5			jpg_dct+10
.define jpg_f6			jpg_dct+12
.define jpg_f7			jpg_dct+14

.define jpg_count		$f8							; used by getbits, addnode amd decodeac
.define jpg_temp2		$f9							; used for huffman nodes
.define jpg_huff		$fa							; huffman pointers
.define jpg_temp		$fe

.define jpg_negmlo		$0a00						; $0A00 DATA BLOCK!!!
.define jpg_posmlo		$0b00						; mult tables
.define jpg_negmhi		$0d00
.define jpg_posmhi		$0e00						; 2 pages

.define jpg_veclo		$8680						; vec to be quantized
.define jpg_vechi		$86c0

.define jpg_notjpg		1							; errors
.define jpg_readerr		2
.define jpg_badqt		3
.define jpg_badht		4
.define jpg_headerr		5
.define jpg_hufferr		6

.define jpg_qt0			$0340						; quantization tables
.define jpg_qt1			jpg_qt0+64
.define jpg_qt2			jpg_qt1+64					; only use 3

jpg_process

		lda #00
		sta jpg_error
		sta jpg_eof
		sta jpg_skipff

		lda #$ff
		sta jpg_filepos+0
		sta jpg_filepos+1
		sta jpg_filepos+2
		sta jpg_reslen+0
		sta jpg_reslen+1

		lda #>jpg_posmlo
		sta jpg_mult1lo+1
		lda #>jpg_negmlo
		sta jpg_mult2lo+1
		lda #>jpg_posmhi
		sta jpg_mult1hi+1
		lda #>jpg_negmhi
		sta jpg_mult2hi+1

		jsr sdc_getbyte								; check jpeg soi
		cmp #$ff									; marker = $ffd8
		bne jpg_err1
		jsr sdc_getbyte
		cmp #$d8
		bne jpg_err1

		jsr jpg_inithuff
		jsr jpg_initbuff

		UICORE_CALLELEMENTFUNCTION la1listbox, uilistbox_startaddentries
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_jpgstart

		jsr jpg_getapp0

:		lda jpg_error
		bne jpg_err1
		jsr jpg_domarker
		lda jpg_eof
		beq :-

		UICORE_CALLELEMENTFUNCTION la1listbox, uilistbox_draw

		rts

jpg_err1
		inc $d020
		jmp *-3

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_imgbuf		= $8700								; image data buffer
jpg_imgbufsize	= $3900

.define jpg_point	$02

jpg_initbuff

		lda #<jpg_imgbuf
		sta jpg_point
		lda #>jpg_imgbuf
		sta jpg_point+1
		ldx #>jpg_imgbufsize
		lda #$80
		ldy #$00
jpgibloop
		sta (jpg_point),y
		dey
		bne jpgibloop
		inc jpg_point+1
		dex
		bne jpgibloop
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_getapp0											; read jfif header

		jsr jpg_getheader
		bcs :+
		lda jpg_header+1
		cmp #$e0									; app0 marker
		beq jpg_ignoresegment

		jmp jpg_domarker2							; doesn't hit?
:		jmp jpg_domarker							; doesn't hit?

jpg_ignoresegment									; ignore rest of segment
		jsr sdc_getbyte
		bcs :+
		lda jpg_eof
		bne :+
		jsr jpg_declen
		bne jpg_ignoresegment
:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

;
; domarker -- read marker and call
;   appropriate routine.
;

jpg_marker_unknown
		jsr jpg_ignoresegment
		; fall through and get next marker

jpg_domarker
		lda jpg_eof
		beq :+
		rts

:		jsr jpg_getheader							; find next
		bcs jpg_domarker

jpg_domarker2
		lda jpg_header+1
		cmp #$dd
		beq jpg_jmpmarker_dri
		cmp #$db
		beq jpg_jmpmarker_dqt
		cmp #$c4
		beq jpg_jmpmarker_dht
		cmp #$c0
		beq jpg_jmpmarker_sof
		cmp #$da
		bne jpg_marker_unknown

jpg_jmpmarker_sos
		jmp jpg_marker_sos
jpg_jmpmarker_dri
		jmp jpg_marker_dri
jpg_jmpmarker_dqt
		jmp jpg_marker_dqt
jpg_jmpmarker_dht
		jmp jpg_marker_dht
jpg_jmpmarker_sof
		jmp jpg_marker_sof

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_marker_sos
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_sos
		inc jpg_eof
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_cres	.word 0

jpg_marker_dri										; lame restart interval markers
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_dri

		jsr sdc_getbyte
		sta jpg_reslen+1
		sta jpg_cres+1
		jsr sdc_getbyte
		sta jpg_reslen+0
		sta jpg_cres+0
		rts

decres	lda jpg_reslen+1
		cmp #$ff
		beq :+

		dec jpg_cres+0
		bne :+
		lda jpg_cres+1
		beq :++
		dec jpg_cres+1
:		rts
:		sta sdc_nbits								; skip bits
		jsr sdc_getbyte								; read $ffxx
		lda jpg_reslen+0
		sta jpg_cres+0
		lda jpg_reslen+1
		sta jpg_cres+1

restart	ldx #5
:		sta jpg_dclo,x
		sta jpg_dchi,x
		dex
		bpl :-
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_marker_dqt
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_dqt

jpg_marker_dqt_start
		jsr jpg_declen
		beq jpg_marker_dqt_end
		jsr sdc_getbyte
		bcs jpg_marker_dqt_err
		tay
		and #$0f									; number of qt
		bne :+
		ldx #<(jpg_qt0)
		lda #>(jpg_qt0)
		bne dqt_ok
:		cmp #1
		bne :+
		ldx #<(jpg_qt1)
		lda #>(jpg_qt1)
		bne dqt_ok
:		cmp #2
		bne jpg_marker_dqt_err
		ldx #<(jpg_qt2)
		lda #>(jpg_qt2)

dqt_ok	stx jpg_point+0								; qt addr
		sta jpg_point+1
		tya
		and #$f0
		bne jpg_marker_dqt_err						; 0 = 8-bit
		ldy #00
dqt_loop
		sty jpg_temp								; counter
		lda jpg_headerlength+0
		ora jpg_headerlength+1
		beq jpg_marker_dqt_err
		jsr sdc_getbyte
		bcs jpg_marker_dqt_err
		ldy jpg_temp
		sta (jpg_point),y
		jsr jpg_declen
		iny
		cpy #64
		bne dqt_loop
		jmp jpg_marker_dqt_start					; multiple qt's allowed

jpg_marker_dqt_err
		lda #jpg_badqt								; only 0-3 allowed
		sta jpg_error

jpg_marker_dqt_end
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_huff_symbols	.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
jpg_hufflen			.byte 0


jpg_marker_dht
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_dht

jpg_marker_dht_start
		jsr jpg_declen
		beq dhtjerr

dhtgetb	jsr sdc_getbyte
		bcc dhtcont
dhtjerr	jmp jpg_marker_dht_error

dhtcont
		tay											; info byte
		and #$0f
		cmp #$04
		bcs dhtjerr
		asl
		tax											; table num 0-3
		tya
		and #$f0
		beq dht_ok									; dc table
		cmp #$10
		bne dhtjerr
		txa											; ac table
		ora #$08									; +8
		tax

dht_ok		
		lda jpg_hufftop
		sta jpg_dchuff0,x
		sta jpg_huff
		lda jpg_hufftop+1
		sta jpg_dchuff0+1,x
		sta jpg_huff+1
		stx jpg_temp2
		ldy #01										; right node
		jsr jpg_newnode								; root node

		ldx #01
:		stx jpg_temp
		lda jpg_headerlength+0
		ora jpg_headerlength+1
		beq jpg_marker_dht_error
		jsr sdc_getbyte
		bcs jpg_marker_dht_error
		ldx jpg_temp
		sta jpg_huff_symbols-1,x
		jsr jpg_declen
		inx
		cpx #17
		bne :-

		lda #$ff
		sta jpg_huffbits+0
		sta jpg_huffbits+1
		lda #1
		sta jpg_hufflen

dhtloop	inc jpg_huffbits+1							; hi,lo!
		bne :+
		inc jpg_huffbits

:		ldx jpg_hufflen
		dec jpg_huff_symbols-1,x
		bpl :+
		cpx #16
		beq dhtnext
		asl jpg_huffbits+1
		rol jpg_huffbits
		inc jpg_hufflen
		bne :-

:		ldx jpg_temp2
		lda jpg_dchuff0,x
		sta jpg_huff
		lda jpg_dchuff0+1,x
		sta jpg_huff+1
		jsr sdc_getbyte
		bcs jpg_marker_dht_error
		ldx jpg_hufflen
		jsr jpg_addnode
		bcs jpg_marker_dht_end
		jsr jpg_declen
		jmp dhtloop
dhtnext	jsr jpg_declen
		beq jpg_marker_dht_end
		jmp dhtgetb									; multiple hts

jpg_marker_dht_error
		lda #jpg_badht
		sta jpg_error

jpg_marker_dht_end		
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_height	.word 0
jpg_width	.word 0
jpg_numrows	.byte 0
jpg_numcols	.byte 0
jpg_ncomps	.byte 0									; num components
jpg_csampv	.byte 0,0,0,0,0,0						; sampling factors
jpg_csamph	.byte 0,0,0,0,0,0						; (horizontal)
jpg_cquant	.byte 0,0,0,0,0,0						; quantization table

jpg_marker_sof
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_sof

jpg_marker_sof_start
		ldx #5
		lda #00
:		sta jpg_csampv,x
		sta jpg_csamph,x
		dex
		bpl :-

		jsr sof_get
		cmp #8
		beq sof_ok
		lda #jpg_badqt
		sta jpg_error
		rts

sof_ok	jsr sof_get
		sta jpg_height+1
		jsr sof_get
		sta jpg_height
		sec
		sbc #1
		sta jpg_numrows
		lda jpg_height+1
		sbc #00
		lsr
		ror jpg_numrows
		lsr
		ror jpg_numrows
		lsr
		ror jpg_numrows
		inc jpg_numrows

		jsr sof_get
		sta jpg_width+1
		jsr sof_get
		sta jpg_width
		sec
		sbc #1										; 0..7 instead of 1..8
		sta jpg_numcols
		lda jpg_width+1
		sbc #00
		lsr
		ror jpg_numcols
		lsr
		ror jpg_numcols
		lsr
		ror jpg_numcols
		inc jpg_numcols								; 0..7 => 1 col, etc.

		jsr sof_get
		sta jpg_ncomps
		sta jpg_temp+0
sof_loop		
		jsr sof_get
		sta jpg_temp+1								; id
		jsr sof_get
		ldx jpg_temp+1
		pha
		and #$0f
		sta jpg_csampv,x
		pla
		lsr
		lsr
		lsr
		lsr
		sta jpg_csamph,x
		jsr sof_get
		ldx jpg_temp+1
		sta jpg_cquant,x
		dec jpg_temp
		bne sof_loop

		ldx #5										; find max sample
		lda #00
:		cmp jpg_csamph,x
		bcs :+
		lda jpg_csamph,x
:		dex
		bne :-
		sta jpg_csamph								; store in +0

		ldx #5
		lda #00
:		cmp jpg_csampv,x
		bcs :+
		lda jpg_csampv,x
:		dex
		bne :-
		sta jpg_csampv
		rts

; ----------------------------------

sof_get
		lda jpg_headerlength+0
		ora jpg_headerlength+1
		beq sof_err2
		jsr jpg_declen
		jsr sdc_getbyte
		bcc sof_end
sof_err2
		pla
		pla

sof_err
		lda #jpg_readerr
		sta jpg_error
sof_end
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_declen
		lda jpg_headerlength+0						; lo byte 0?
		bne :+										; nope, decrease
		ora jpg_headerlength+1
		beq :++
		dec jpg_headerlength+1
:		dec jpg_headerlength+0
		lda jpg_headerlength+0
		ora jpg_headerlength+1
:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_header			.word 0							; hi, lo
jpg_headerlength	.word 0							; lo, hi

; getheader -- read in header bytes.
; on exit:
;   c set -> error
;   z set -> end of file

jpg_getheader
		;UICORE_SETLISTBOXTEXT la1listbox, uitxt_jpgheader

		lda #00
		sta jpg_header+0
		sta jpg_header+1

		jsr sdc_getbyte
		cmp #$ff									; expect to find $ff marker
		bne jpg_getheader_error

		jsr sdc_getbyte
		sta jpg_header+1
		cmp #$d8									; start of jpeg
		beq jpg_getheader_ok						; lame photoshop
		cmp #$d9									; end of file
		bne :+
		sta jpg_eof
		beq jpg_getheader_ok

:		jsr sdc_getbyte								; get high byte of length
		bcs jpg_getheader_end
		sta jpg_headerlength+1
		jsr sdc_getbyte								; get lo byte of length
		bcs jpg_getheader_end
		sec
		sbc #2
		sta jpg_headerlength+0						; subtract 2 from length (to get rid of first two bytes included in the length)
		bcs :+
		dec jpg_headerlength+1
:		ora jpg_headerlength+1						; empty segment
		beq jpg_getheader

jpg_getheader_ok		
		clc
		rts

jpg_getheader_error
		sec
		;fallthrough

jpg_getheader_end

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; huffman tree routines.
;
; the huffman tree is implemented as a series of 2-byte nodes.
; left nodes are at huff+2
; right nodes are at (huff) if link < $8000.

; link   = $80xx means xx=leaf value,
; link   = $ffxx means no right link,
; link+2 = hufftop -> no left link.

.define jpg_huffmem		$1000						; huffman trees

jpg_dchuff0		.word 0								; addresses
jpg_dchuff1		.word 0
jpg_dchuff2		.word 0
jpg_dchuff3		.word 0
jpg_achuff0		.word 0
jpg_achuff1		.word 0
jpg_achuff2		.word 0
jpg_achuff3		.word 0

jpg_hufftop		.word 0								; end of huffman tree
jpg_hufftx		.byte 0
jpg_huffty		.byte 0

jpg_huffbits	.word 0								; hi,lo
jpg_huffval		.byte 0

jpg_bitp		.byte $00									; bit patterns (masks)
				.byte $01, $02, $04,$08, $10, $20, $40, $80
				.byte $01, $02, $04,$08, $10, $20, $40, $80
				.byte "-wyn-"

jpg_inithuff

		lda #<jpg_huffmem
		sta jpg_hufftop+0
		lda #>jpg_huffmem
		sta jpg_hufftop+1
		rts

; create new node; make current node
; point to it.
;
; on entry: .y = 0 -> right node, otherwise left node

jpg_newnode
		sty jpg_huffty
		stx jpg_hufftx

		tya
		bne jpg_nnskip
		lda jpg_hufftop
		sec
		sbc jpg_huff+0
		sta (jpg_huff),y							; point -> new node
		iny
		lda jpg_hufftop+1
		sbc jpg_huff+1
		sta (jpg_huff),y

jpg_nnskip
		lda jpg_hufftop
		sta jpg_point+0
		clc
		adc #2
		sta jpg_hufftop
		lda jpg_hufftop+1
		sta jpg_point+1
		adc #00
		sta jpg_hufftop+1

		ldy #01
		lda #$ff
		sta (jpg_point),y							; init new node

		ldx jpg_hufftx
		ldy jpg_huffty
		clc
		rts

;:err	lda #badht
;		sta error
;		sec
;		rts

; add a new node; .x = length
; (huff) -> tree root

jpg_addnode
		sta jpg_huffval

jpg_anloop

		ldy #1
		cpx #9
		bcc :+
		dey
:		lda jpg_bitp,x
		and jpg_huffbits,y
		bne jpg_anright

jpg_anleft
		lda jpg_huff								; check if at end
		clc
		adc #2
		pha
		tay
		lda jpg_huff+1
		adc #00
		pha
		cpy jpg_hufftop
		sbc jpg_hufftop+1
		bcc :+										; not a new node
		ldy #$80									; create left node
		jsr jpg_newnode
:		pla
		sta jpg_huff+1
		pla
		sta jpg_huff
		jmp jpgancontinue

jpg_anright
		ldy #1
		lda (jpg_huff),y							; check for rt ptr
		bpl :+
		dey											; .y=0 -> rt node
		jsr jpg_newnode
:		ldy #00
		lda (jpg_huff),y
		clc
		adc jpg_huff
		pha
		iny
		lda (jpg_huff),y
		adc jpg_huff+1
		sta jpg_huff+1
		pla
		sta jpg_huff

jpgancontinue
		dex
		bne jpg_anloop
		lda #$80
		ldy #01
		sta (jpg_huff),y							; store value
		lda jpg_huffval
		dey
		sta (jpg_huff),y							; $80xx
		clc
		rts

; ----------------------------------------------------------------

; decode DC coeffs

decodedc
		ldx jpg_curcomp								; set huffman
		lda jpg_dchuff,x
		asl
		tax
		lda jpg_dchuff0,x
		sta jpg_huff
		lda jpg_dchuff0+1,x
		sta jpg_huff+1

		jsr jpg_gethuff								; get category
		ldx jpg_error
		bne :+
		jsr jpg_getbits								; get the bits
		ldx jpg_curcomp
		lda jpg_bitslo
		clc
		adc jpg_dclo,x
		sta jpg_dclo,x
		sta jpg_veclo
		lda jpg_dchi,x
		adc jpg_bitshi
		sta jpg_dchi,x
		sta jpg_vechi
:		rts

; ----------------------------------------------------------------

; decode AC coeffs

jpg_tmphuf	.byte 0
jpg_achuff	.byte 0, 0, 0, 0, 0, 0					; ac table to use
jpg_dchuff	.byte 0, 0, 0, 0, 0, 0					; dc table to use
jpg_dclo	.byte 0, 0, 0, 0, 0, 0					; dc coeffs
jpg_dchi	.byte 0, 0, 0, 0, 0, 0

jpg_curcomp	.byte 0

jpg_decodeac
		ldx jpg_curcomp								; set huffman
		lda jpg_achuff,x
		asl
		tax
		stx jpg_tmphuf

		ldy #1
decacloop
		sty jpg_temp2								; index
		ldx jpg_tmphuf
		lda jpg_achuff0,x
		sta jpg_huff
		lda jpg_achuff0+1,x
		sta jpg_huff+1

		jsr jpg_gethuff								; get rle len
		beq decacfill
		ldx jpg_error
		bne jpg_decodeac_end
		sta jpg_count								; temp
		lsr
		lsr
		lsr
		lsr											; # of zeros
		beq :++
decacfill		
		tax
		lda #00
		ldy jpg_temp2
:		sta jpg_veclo,y
		sta jpg_vechi,y
		iny
		cpy #64
		bcs jpg_decodeac_end
		dex
		bne :-
		sty jpg_temp2
:		lda jpg_count
		and #$0f									; category
		jsr jpg_getbits
		ldy jpg_temp2
		lda jpg_bitslo
		sta jpg_veclo,y
		lda jpg_bitshi
		sta jpg_vechi,y
		iny
		cpy #64
		bcc decacloop

jpg_decodeac_end		
		rts

; ----------------------------------------------------------------

; gethuff -- get valid huffman code from (huff)

jpg_gethuff
		ldy #01
		lda (jpg_huff),y
		cmp #$80
		beq jpgghfound

		jsr sdc_getbit
		bcs jpgghright
		lda jpg_huff+0
		adc #2										; c clear
		tax
		lda jpg_huff+1
		adc #00
		tay
		cpx jpg_hufftop+0
		sbc jpg_hufftop+1
		bcs jpg_gethuff_error
		sty jpg_huff+1
		stx jpg_huff+0
		bcc jpg_gethuff

jpgghright
		ldy #01
		lda (jpg_huff),y
		bmi jpg_gethuff_error
		pha
		dey
		lda (jpg_huff),y
		clc
		adc jpg_huff+0
		sta jpg_huff+0
		pla
		adc jpg_huff+1
		sta jpg_huff+1
		bne jpg_gethuff

jpgghfound
		dey
		lda (jpg_huff),y
		rts

jpg_gethuff_error
		lda #jpg_hufferr
		sta jpg_error
		rts

; ----------------------------------------------------------------

; retrieve .a bits and convert to signed number in (bitslo, bitshi)

jpg_sign	.byte 0

jpg_getbits
		sta jpg_count
		tax
		beq jpg_getbits_zero
		jsr sdc_getbit
		lda #00
		bcs :+
		lda #$ff									; 0-> negative
:		sta jpg_bitshi
		rol
		sta jpg_bitslo
		sta jpg_sign
		dec jpg_count
		beq jpggbsdone
jpggbsloop
		jsr sdc_getbit
		rol jpg_bitslo
		rol jpg_bitshi
		dec jpg_count
		bne jpggbsloop
jpggbsdone
		lda jpg_sign
		bpl jpggbsrts
		inc jpg_bitslo								; make two's complement
		bne jpggbsrts
		inc jpg_bitshi
jpggbsrts		
		rts

jpg_getbits_zero
		sta jpg_bitslo
		sta jpg_bitshi
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; since the algorithm is really an FFT converted into a DCT, the coefficients need a little massaging before tranformation.
;
; specifically,
;     f(i) = s(i) / (2cos(i * pi / 16))      i = 0..7
;        with
;     f(0) = f(0) * 2 / sqrt(2)
;
;     which can be combined with the first step using the table for i=4.
;
; these multipliers can in part be incorporated in the quantization table, but for now they're out in the open.

.define jpg_sec1	$3200
.define jpg_sec2	$3400
.define jpg_sec3	$3600
.define jpg_sec4	$3800
.define jpg_sec5	$3a00
.define jpg_sec6	$3c00
.define jpg_sec7	$3e00

jpg_prepdat
		ldx #00
		lda #<jpg_sec4
		sta jpg_point+0
		lda #>jpg_sec4
		sta jpg_point+1
		lda jpg_f0+0
		sta jpg_bitslo
		lda jpg_f0+1
		jsr jpg_pmult
		sta jpg_f0+1
		lda jpg_bitslo
		sta jpg_f0+0

		ldx #00
		lda #<jpg_sec1
		sta jpg_point+0
		lda #>jpg_sec1
		sta jpg_point+1
		lda jpg_f1+0
		sta jpg_bitslo
		lda jpg_f1+1
		jsr jpg_pmult
		sta jpg_f1+1
		lda jpg_bitslo
		sta jpg_f1+0

		ldx #00
		lda #<jpg_sec2
		sta jpg_point+0
		lda #>jpg_sec2
		sta jpg_point+1
		lda jpg_f2+0
		sta jpg_bitslo
		lda jpg_f2+1
		jsr jpg_pmult
		sta jpg_f2+1
		lda jpg_bitslo
		sta jpg_f2+0

		ldx #00
		lda #<jpg_sec3
		sta jpg_point+0
		lda #>jpg_sec3
		sta jpg_point+1
		lda jpg_f3+0
		sta jpg_bitslo
		lda jpg_f3+1
		jsr jpg_pmult
		sta jpg_f3+1
		lda jpg_bitslo
		sta jpg_f3+0

		ldx #00
		lda #<jpg_sec4
		sta jpg_point+0
		lda #>jpg_sec4
		sta jpg_point+1
		lda jpg_f4+0
		sta jpg_bitslo
		lda jpg_f4+1
		jsr jpg_pmult
		sta jpg_f4+1
		lda jpg_bitslo
		sta jpg_f4+0

		ldx #00
		lda #<jpg_sec5
		sta jpg_point+0
		lda #>jpg_sec5
		sta jpg_point+1
		lda jpg_f5+0
		sta jpg_bitslo
		lda jpg_f5+1
		jsr jpg_pmult
		sta jpg_f5+1
		lda jpg_bitslo
		sta jpg_f5+0

		ldx #00
		lda #<jpg_sec6
		sta jpg_point+0
		lda #>jpg_sec6
		sta jpg_point+1
		lda jpg_f6+0
		sta jpg_bitslo
		lda jpg_f6+1
		jsr jpg_pmult
		sta jpg_f6+1
		lda jpg_bitslo
		sta jpg_f6+0

		ldx #00
		lda #<jpg_sec7
		sta jpg_point+0
		lda #>jpg_sec7
		sta jpg_point+1
		lda jpg_f7+0
		sta jpg_bitslo
		lda jpg_f7+1
		jsr jpg_pmult
		sta jpg_f7+1
		lda jpg_bitslo
		sta jpg_f7+0

		rts

jpg_pmult											; exit .a = bitshi
		bmi jpg_pmult_neg
		beq :++
:		inx											; shift count
		lsr
		ror jpg_bitslo
		cmp #00
		bne :-
:		sta jpg_bitshi
		lda jpg_bitslo
		asl
		rol jpg_bitshi
		adc jpg_point+0
		sta jpg_point+0
		lda jpg_bitshi
		adc jpg_point+1
		sta jpg_point+1
		ldy #00
		lda (jpg_point),y
		sta jpg_bitslo
		iny
		lda (jpg_point),y
		dex
		bmi :++
:		asl jpg_bitslo
		rol
		dex
		bpl :-
:		rts

jpg_pmult_neg
		sta jpg_bitshi
		lda #00
		sec
		sbc jpg_bitslo
		sta jpg_bitslo
		lda #00
		sbc jpg_bitshi
		beq :++
:		inx					; shift count
		lsr
		ror jpg_bitslo
		cmp #00
		bne :-
:		asl jpg_bitslo
		rol
		sta jpg_bitshi
		lda jpg_bitslo
		adc jpg_point+0
		sta jpg_point+0
		lda jpg_bitshi
		adc jpg_point+1
		sta jpg_point+1
		ldy #00
		lda (jpg_point),y
		sta jpg_bitslo
		iny
		lda (jpg_point),y
		dex
		bmi :++
:		asl jpg_bitslo
		rol
		dex
		bpl :-
:		sta jpg_bitshi
		lda #00
		sec
		sbc jpg_bitslo
		sta jpg_bitslo
		lda #00
		sbc jpg_bitshi
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------
