
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

.define jpg_negmlo		$0400						; $0A00-$1000 DATA BLOCK!!!
.define jpg_posmlo		$0500						; mult tables
.define jpg_negmhi		$0600
.define jpg_posmhi		$0700						; 2 pages

.define jpg_a1216		46341						; a1 * 2^16
.define jpg_a2216		35468						; a2 * 2^16
.define jpg_a3216		jpg_a1216
.define jpg_a4216		20091						; ...
.define jpg_a5216		25080

/*
.define jpg_a1lo		$2900						; cos(2a), a=pi/8
.define jpg_a1hi		$2a00
.define jpg_a2lo		$2b00						; cos(a) - cos(3a)
.define jpg_a2hi		$2c00
.define jpg_a3lo		jpg_a1lo					; cos(2a)
.define jpg_a3hi		jpg_a1hi
.define jpg_a4lo		$2d00						; cos(a) + cos(3a)
.define jpg_a4hi		$2e00
.define jpg_a4gh		$2f00
.define jpg_a5lo		$3000						; cos(3a)
.define jpg_a5hi		$3100

.define jpg_sec1		$3200
.define jpg_sec2		$3400
.define jpg_sec3		$3600
.define jpg_sec4		$3800
.define jpg_sec5		$3a00
.define jpg_sec6		$3c00
.define jpg_sec7		$3e00						; ends $4000
*/

.define jpg_a1lo		$0900						; cos(2a), a=pi/8
.define jpg_a1hi		$0a00
.define jpg_a2lo		$0b00						; cos(a) - cos(3a)
.define jpg_a2hi		$0c00
.define jpg_a3lo		jpg_a1lo					; cos(2a)
.define jpg_a3hi		jpg_a1hi
.define jpg_a4lo		$0d00						; cos(a) + cos(3a)
.define jpg_a4hi		$0e00
.define jpg_a4gh		$0f00
.define jpg_a5lo		$1000						; cos(3a)
.define jpg_a5hi		$1100

.define jpg_sec1		$1200
.define jpg_sec2		$1400
.define jpg_sec3		$1600
.define jpg_sec4		$1800
.define jpg_sec5		$1a00
.define jpg_sec6		$1c00
.define jpg_sec7		$1e00						; ends $4000


.define jpg_crtab1		$8400						; rgb conversion
.define jpg_crtab2		$8480
.define jpg_cbtab1		$8500
.define jpg_cbtab2		$8580

.define jpg_trans		$8600						; transform

.define jpg_veclo		$8680						; vec to be quantized
.define jpg_vechi		$86c0

.define jpg_imgbuf		$8700						; image data buffer $8700-$c000
.define jpg_imgbufsize	$3900

.define jpg_ybuf		jpg_imgbuf+$0000			; $8700
.define jpg_cbbuf		jpg_imgbuf+$1300			; $9a00
.define jpg_crbuf		jpg_imgbuf+$2600			; $ad00

.define jpg_point		$02
.define jpg_dest		$04
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

.define jpg_index		$20							; idct stuff

.define jpg_t1			$22
.define jpg_t2			$24
.define jpg_t3			$26

.define jpg_vsamp		$28							; desample
.define jpg_hsamp		$29


.define jpg_coeff		$02a7
.define jpg_c0			jpg_coeff+0
.define jpg_c1			jpg_coeff+2
.define jpg_c2			jpg_coeff+4
.define jpg_c3			jpg_coeff+6
.define jpg_c4			jpg_coeff+8
.define jpg_c5			jpg_coeff+10
.define jpg_c6			jpg_coeff+12
.define jpg_c7			jpg_coeff+14


.define jpg_count		$f8							; used by getbits, addnode amd decodeac
.define jpg_temp2		$f9							; used for huffman nodes
.define jpg_huff		$fa							; huffman pointers
.define jpg_quantp		$fc							; quant table
.define jpg_temp		$fe


.define jpg_notjpg		1							; errors
.define jpg_readerr		2
.define jpg_badqt		3
.define jpg_badht		4
.define jpg_headerr		5
.define jpg_hufferr		6

.define jpg_qt0			$0340						; quantization tables
.define jpg_qt1			jpg_qt0+64					; $0380
.define jpg_qt2			jpg_qt1+64					; $03c0 - only use 3

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

jpg_row		.byte 0
jpg_rowoff	.byte 0				; row offset

jpg_marker_sos
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_sos

		dec jpg_skipff								; skip $ff bytes
		
		; -------------------------------
		jsr jpg_rendinit
		; -------------------------------

		jsr sdc_getbyte

		sta jpg_temp								; # of components
		sta jpg_ncomps

:		jsr sdc_getbyte
		sta jpg_temp+1								; component id    01 00     02 11     03 11

		jsr sdc_getbyte
		ldx jpg_temp+1

		pha
		and #$0f
		sta jpg_achuff,x
		pla
		lsr
		lsr
		lsr
		lsr
		sta jpg_dchuff,x
		dec jpg_temp
		bne :-
		jsr sdc_getbyte								; scan parameters		00
		jsr sdc_getbyte								; (progressive)			3F
		jsr sdc_getbyte								; (ignore)				00

; image data begins here							$02BB in pup.jpg file

		lda #00
		sta jpg_row
		sta jpg_col
		jsr restart

jpg_sos_ready

		inc $d020

		ldx #1										; luma/intensity
		lda #<jpg_ybuf
		ldy #>jpg_ybuf
		jsr jpg_readdataunit

		ldx jpg_ncomps
		dex
		beq jpg_sos_readdone
		ldx #2										; read chroma
		lda #<jpg_cbbuf
		ldy #>jpg_cbbuf
		jsr jpg_readdataunit

		ldx jpg_ncomps
		dex
		beq jpg_sos_readdone
		ldx #3										; read chroma
		lda #<jpg_crbuf
		ldy #>jpg_crbuf
		jsr jpg_readdataunit

jpg_sos_readdone
		jsr decres

		lda jpg_eof
		bne jpg_sos_done
		lda jpg_csamph								; max sample
		clc
		adc jpg_col
		sta jpg_col
		cmp jpg_numcols
		bcc jpg_sos_ready

		jsr jpg_torgb

		lda #00
		sta jpg_col

		lda #<jpg_imgbuf
		ldy #>jpg_imgbuf
		ldx jpg_csampv
		stx jpg_temp2

		;inc $d020
		;jmp *-3

jpg_sos_rend
		sta jpg_temp+0
		sty jpg_temp+1

		ldx jpg_row									; don't render anything if we haven't reached the start row yet
		cpx jpg_rowoff
		bcc jpg_sos_norend

		; -----------------------------
		jsr jpg_render
		; -----------------------------

jpg_sos_norend
		inc jpg_row
		lda jpg_row
		cmp jpg_numrows
		bcs jpg_sos_done
		sec
		sbc jpg_rowoff
		bcc :+
		cmp #25
		bcs jpg_sos_done

:		lda jpg_temp			; next buffer
		clc
		adc jpg_buflen+0
		sta jpg_temp+0
		lda jpg_temp+1
		adc jpg_buflen+1
		tay
		lda jpg_temp
		dec jpg_temp2
		bne jpg_sos_rend

		jmp jpg_sos_ready

jpg_sos_done
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
		sta jpg_huff+0
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
		sta jpg_huff+0
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
jpg_csampv	.byte 0, 0, 0, 0, 0, 0					; sampling factors
jpg_csamph	.byte 0, 0, 0, 0, 0, 0					; (horizontal)
jpg_cquant	.byte 0, 0, 0, 0, 0, 0					; quantization table

jpg_marker_sof
		UICORE_SETLISTBOXTEXT la1listbox, uitxt_marker_sof

jpg_marker_sof_start
		ldx #5
		lda #00
:		sta jpg_csampv,x
		sta jpg_csamph,x
		dex
		bpl :-

		jsr sof_get									; get bit depth - should be 8
		cmp #8
		beq sof_ok
		lda #jpg_badqt
		sta jpg_error
		rts

sof_ok	jsr sof_get									; get height - $00e8
		sta jpg_height+1
		jsr sof_get
		sta jpg_height+0
		sec
		sbc #1										; 0..7 instead of 1..8
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

		jsr sof_get									; get width - 40122
		sta jpg_width+1
		jsr sof_get
		sta jpg_width+0
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

		jsr sof_get									; get components - 3
		sta jpg_ncomps
		sta jpg_temp+0
sof_loop

		; 01 22 00 Y
		; 02 11 01 Cb
		; 03 11 01 Cr

		jsr sof_get									; read component ID
		sta jpg_temp+1								;
		jsr sof_get									; read 22, 11, 11
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
		jsr sof_get									; read 0 (Y) or 1 (Cb, Cr)
		ldx jpg_temp+1
		sta jpg_cquant,x
		dec jpg_temp
		bne sof_loop

		; csampv = 0 2 1 1 0 0
		; csamph = 0 2 1 1 0 0
		; cquant = 0 0 1 1 0 0

		ldx #5										; find max sample
		lda #00
:		cmp jpg_csamph,x
		bcs :+
		lda jpg_csamph,x
:		dex
		bne :--
		sta jpg_csamph								; store in +0

		; csamph = 2 2 1 1 0 0

		ldx #5										; find max sample
		lda #00
:		cmp jpg_csampv,x
		bcs :+
		lda jpg_csampv,x
:		dex
		bne :--
		sta jpg_csampv								; store in +0

		; csampv = 2 2 1 1 0 0

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
		lda jpg_huff+0								; check if at end
		clc
		adc #2
		pha
		tay
		lda jpg_huff+1
		adc #00
		pha
		cpy jpg_hufftop+0
		sbc jpg_hufftop+1
		bcc :+										; not a new node
		ldy #$80									; create left node
		jsr jpg_newnode
:		pla
		sta jpg_huff+1
		pla
		sta jpg_huff+0
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
		adc jpg_huff+0
		pha
		iny
		lda (jpg_huff),y
		adc jpg_huff+1
		sta jpg_huff+1
		pla
		sta jpg_huff+0

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

jpg_decodedc
		ldx jpg_curcomp								; set huffman
		lda jpg_dchuff,x
		asl
		tax
		lda jpg_dchuff0,x
		sta jpg_huff+0
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
		sta jpg_huff+0
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

; compute the inverse dct (1d)
; uses modified reversed flowgraph from pennebaker & mitchell, p. 52
;
; input: dct coeffs contained in flo/fhi
; output: original coeffs in coeffs

jpg_idct
		jsr jpg_prepdat								; shift and such

; stage 1: f(5) <- f(5) - f(3)
;          f(1) <- f(1) + f(7)
;          f(7) <- f(1) - f(7)
;          f(3) <- f(5) + f(3)

		lda jpg_f5
		sec
		sbc jpg_f3
		sta jpg_t1
		lda jpg_f5+1
		sbc jpg_f3+1
		sta jpg_t1+1

		lda jpg_f1
		clc
		adc jpg_f7
		sta jpg_t2
		lda jpg_f1+1
		adc jpg_f7+1
		sta jpg_t2+1

		lda jpg_f1
		sec
		sbc jpg_f7
		sta jpg_t3
		lda jpg_f1+1
		sbc jpg_f7+1
		sta jpg_t3+1

		lda jpg_f5
		clc
		adc jpg_f3
		sta jpg_f3
		lda jpg_f5+1
		adc jpg_f3+1
		sta jpg_f3+1

		lda jpg_t3
		sta jpg_f7
		lda jpg_t3+1
		sta jpg_f7+1

		lda jpg_t2
		sta jpg_f1
		lda jpg_t2+1
		sta jpg_f1+1

		lda jpg_t1
		sta jpg_f5
		lda jpg_t1+1
		sta jpg_f5+1

; stage 2: f(2) <- f(2) - f(6)
;          f(6) <- f(2) + f(6)
;          f(1) <- f(1) - f(3)
;          f(3) <- f(1) + f(3)

		lda jpg_f2
		sec
		sbc jpg_f6
		sta jpg_t1
		lda jpg_f2+1
		sbc jpg_f6+1
		sta jpg_t1+1

		lda jpg_f2
		clc
		adc jpg_f6
		sta jpg_f6
		lda jpg_f2+1
		adc jpg_f6+1
		sta jpg_f6+1

		lda jpg_t1
		sta jpg_f2
		lda jpg_t1+1
		sta jpg_f2+1

		lda jpg_f1
		sec
		sbc jpg_f3
		sta jpg_t1
		lda jpg_f1+1
		sbc jpg_f3+1
		sta jpg_t1+1

		lda jpg_f1
		clc
		adc jpg_f3
		sta jpg_f3
		lda jpg_f1+1
		adc jpg_f3+1
		sta jpg_f3+1

		lda jpg_t1
		sta jpg_f1
		lda jpg_t1+1
		sta jpg_f1+1

; stage 3: f(2) <- a1*f(2)
;          f(5) <- -a2*f(5) + t1
;          f(1) <- a3*f(1)
;          f(7) <- a4*f(7) + t1
; where t1 = -a5*(f(5) + f(7))

; f(2) <- a1*f(2)

		ldx jpg_f2									; lo
		ldy jpg_f2+1								; hi
		lda jpg_a1lo,y
		clc
		adc jpg_a1hi,x
		sta jpg_bitslo								; lo byte
		lda jpg_a1hi,y
		adc #00
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a1216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a1216
:		sta jpg_f2+1
		lda jpg_bitslo
		sta jpg_f2

; f(1) = a3*f(1)

		ldx jpg_f1									; lo
		ldy jpg_f1+1								; hi
		lda jpg_a3lo,y
		clc
		adc jpg_a3hi,x
		sta jpg_bitslo
		lda jpg_a3hi,y
		adc #00
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a3216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a3216
:		sta jpg_f1+1
		lda jpg_bitslo
		sta jpg_f1

; t1 = -a5*(f(5) + f(7))

		lda jpg_f5
		clc
		adc jpg_f7
		tax											; lo
		lda jpg_f5+1
		adc jpg_f7+1
		tay											; hi
		lda jpg_a5lo,y
		clc
		adc jpg_a5hi,x
		sta jpg_bitslo
		lda jpg_a5hi,y
		adc #00
		sta jpg_bitshi
		cpy #$80
		bcc :+
		lda jpg_bitslo
		sbc #<jpg_a5216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a5216
		sta jpg_bitshi
:		lda jpg_bitslo
		eor #$ff
		clc
		adc #01
		sta jpg_t1
		lda jpg_bitshi
		eor #$ff
		adc #00
		sta jpg_t1+1

; f(5) = t1 - a2*f(5)

		ldx jpg_f5									; lo
		ldy jpg_f5+1								; hi
		lda jpg_a2lo,y
		clc
		adc jpg_a2hi,x
		sta jpg_bitslo
		lda jpg_a2hi,y
		adc #00
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a2216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a2216
:		sta jpg_bitshi
		lda jpg_t1
		sec
		sbc jpg_bitslo
		sta jpg_f5
		lda jpg_t1+1
		sbc jpg_bitshi
		sta jpg_f5+1

; f(7) = a4*f(7) + t1

		ldx jpg_f7									; lo
		ldy jpg_f7+1								; hi
		lda jpg_a4lo,y
		clc
		adc jpg_a4hi,x
		sta jpg_bitslo
		lda jpg_a4hi,y
		adc jpg_a4gh,x								; a4*.x can be >255
		cpy #$80
		bcc :+
		sta jpg_bitshi
		lda jpg_bitslo
		sbc #<jpg_a4216
		sta jpg_bitslo
		lda jpg_bitshi
		sbc #>jpg_a4216
:		sta jpg_bitshi
		lda jpg_bitslo
		clc
		adc jpg_t1
		sta jpg_f7
		lda jpg_bitshi
		adc jpg_t1+1
		sta jpg_f7+1

; stage 4:
;   f(0) <- f(0) + f(4)
;   f(4) <- f(0) - f(4)
;   f(6) <- f(2) + f(6)

		lda jpg_f0
		clc
		adc jpg_f4
		sta jpg_t1
		lda jpg_f0+1
		adc jpg_f4+1
		sta jpg_t1+1

		lda jpg_f0
		sec
		sbc jpg_f4
		sta jpg_f4
		lda jpg_f0+1
		sbc jpg_f4+1
		sta jpg_f4+1

		lda jpg_t1
		sta jpg_f0
		lda jpg_t1+1
		sta jpg_f0+1

		lda jpg_f2
		clc
		adc jpg_f6
		sta jpg_f6
		lda jpg_f2+1
		adc jpg_f6+1
		sta jpg_f6+1

; stage 5:
;   f(0) <- f(0) + f(6)
;   f(4) <- f(2) + f(4)
;   f(2) <- f(4) - f(2)
;   f(6) <- f(0) - f(6)
;   f(3) <- f(3) + f(7)
;   f(7) <- f(7) + f(1)
;   f(1) <- f(1) - f(5)
;   f(5) <- -f(5)

		lda jpg_f0
		clc
		adc jpg_f6
		sta jpg_t1
		lda jpg_f0+1
		adc jpg_f6+1
		sta jpg_t1+1

		lda jpg_f0
		sec
		sbc jpg_f6
		sta jpg_f6
		lda jpg_f0+1
		sbc jpg_f6+1
		sta jpg_f6+1
		lda jpg_t1
		sta jpg_f0
		lda jpg_t1+1
		sta jpg_f0+1

		lda jpg_f4
		clc
		adc jpg_f2
		sta jpg_t1
		lda jpg_f4+1
		adc jpg_f2+1
		sta jpg_t1+1

		lda jpg_f4
		sec
		sbc jpg_f2
		sta jpg_f2
		lda jpg_f4+1
		sbc jpg_f2+1
		sta jpg_f2+1
		lda jpg_t1
		sta jpg_f4
		lda jpg_t1+1
		sta jpg_f4+1

		lda jpg_f3
		clc
		adc jpg_f7
		sta jpg_f3
		lda jpg_f3+1
		adc jpg_f7+1
		sta jpg_f3+1

		lda jpg_f7
		clc
		adc jpg_f1
		sta jpg_f7
		lda jpg_f7+1
		adc jpg_f1+1
		sta jpg_f7+1

		lda jpg_f1
		sec
		sbc jpg_f5
		sta jpg_f1
		lda jpg_f1+1
		sbc jpg_f5+1
		sta jpg_f1+1

		lda #00
		sec
		sbc jpg_f5
		sta jpg_f5
		lda #00
		sbc jpg_f5+1
		sta jpg_f5+1

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

		lda jpg_f0
		clc
		adc jpg_f3
		sta jpg_c0
		lda jpg_f0+1
		adc jpg_f3+1
		sta jpg_c0+1

		lda jpg_f4
		clc
		adc jpg_f7
		sta jpg_c1
		lda jpg_f4+1
		adc jpg_f7+1
		sta jpg_c1+1

		lda jpg_f2
		clc
		adc jpg_f1
		sta jpg_c2
		lda jpg_f2+1
		adc jpg_f1+1
		sta jpg_c2+1

		lda jpg_f6
		clc
		adc jpg_f5
		sta jpg_c3
		lda jpg_f6+1
		adc jpg_f5+1
		sta jpg_c3+1

		lda jpg_f6
		sec
		sbc jpg_f5
		sta jpg_c4
		lda jpg_f6+1
		sbc jpg_f5+1
		sta jpg_c4+1

		lda jpg_f2
		sec
		sbc jpg_f1
		sta jpg_c5
		lda jpg_f2+1
		sbc jpg_f1+1
		sta jpg_c5+1

		lda jpg_f4
		sec
		sbc jpg_f7
		sta jpg_c6
		lda jpg_f4+1
		sbc jpg_f7+1
		sta jpg_c6+1

		lda jpg_f0
		sec
		sbc jpg_f3
		sta jpg_c7
		lda jpg_f0+1
		sbc jpg_f3+1
		sta jpg_c7+1
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_idct2d

jpg_idct2d_cols										; first the columns

		ldx #00
ji2dcloop		
		stx jpg_index
		ldy #00
:		lda jpg_trans,x
		sta jpg_dct,y
		lda jpg_trans+1,x
		sta jpg_dct+1,y
		txa
		clc
		adc #16
		tax
		iny
		iny
		cpy #16
		bne :-
		jsr jpg_idct
		ldy #0
		ldx jpg_index
:		lda jpg_coeff,y
		sta jpg_trans,x
		lda jpg_coeff+1,y
		sta jpg_trans+1,x
		txa
		clc
		adc #16
		tax
		iny
		iny
		cpy #16
		bne :-
		ldx jpg_index
		inx
		inx
		cpx #16
		bcc ji2dcloop

jpg_idct2d_rows										; then the rows

		ldx #00
		stx jpg_index
		stx jpg_count
ji2drloop		
		ldy #00
:		lda jpg_trans,x
		sta jpg_dct,y
		lda jpg_trans+1,x
		sta jpg_dct+1,y
		inx
		inx
		iny
		iny
		cpy #16
		bne :-
		stx jpg_index
		jsr jpg_idct
		ldy jpg_count
		ldx #00
:		lda jpg_coeff,x
		sta jpg_bitslo
		lda jpg_coeff+1,x
		cmp #$80
		ror
		ror jpg_bitslo
		cmp #$80
		ror
		ror jpg_bitslo
		sta jpg_bitshi
		lda jpg_bitslo
		adc #128									; c determines rounding
		; sta (dest),y
		sta jpg_trans,y
		lda jpg_bitshi								; range check
		adc #00
		beq ji2drcont
		bpl ji2drpos
		lda #00
		.byte $2c									; BIT $xxxx
ji2drpos
		lda #$ff
		; sta (dest),y
		sta jpg_trans,y
ji2drcont		
		; inc dest
		iny
		inx
		inx
		cpx #16
		bne :-
		sty jpg_count
		ldx jpg_index
		cpx #128
		bcc ji2drloop

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

;
; convert to rgb
;

.define jpg_ypoint		jpg_point
.define jpg_cbpoint		jpg_dest
.define jpg_crpoint		jpg_bitslo

jpg_torgb
		lda #<jpg_ybuf
		sta jpg_ypoint
		lda #>jpg_ybuf
		sta jpg_ypoint+1
		lda #<jpg_cbbuf
		sta jpg_cbpoint
		lda #>jpg_cbbuf
		sta jpg_cbpoint+1
		lda #<jpg_crbuf
		sta jpg_crpoint
		lda #>jpg_crbuf
		sta jpg_crpoint+1

		ldy #00
		ldx jpg_ncomps
		dex
		bne jpg_torgb_loop
		ldx #>(jpg_cbbuf-jpg_ybuf)
:		lda (jpg_ypoint),y
		sta (jpg_cbpoint),y
		sta (jpg_crpoint),y
		iny
		bne :-
		inc jpg_ypoint+1
		inc jpg_cbpoint+1
		inc jpg_crpoint+1
		dex
		bne :-
		rts

jpg_torgb_loop
		lda #00
		sta jpg_temp+1
		lda (jpg_cbpoint),y
		eor #$80
		bpl jpg_torgb_poscb
		eor #$ff ; negcb
		clc
		adc #01
		tax
		lda (jpg_ypoint),y
		clc
		adc jpg_cbtab1,x
		sta jpg_temp+0
		bcc :+
		inc jpg_temp+1								; high byte
:		lda (jpg_ypoint),y
		sec
		sbc jpg_cbtab2,x
		bcs jpg_torgb_cont
		lda #00										; underflow
		beq jpg_torgb_cont

jpg_torgb_poscb
		tax
		lda (jpg_ypoint),y
		sec
		sbc jpg_cbtab1,x
		sta jpg_temp+0
		bcs :+
		dec jpg_temp+1
:		lda (jpg_ypoint),y
		clc
		adc jpg_cbtab2,x
		bcc jpg_torgb_cont
		lda #255
jpg_torgb_cont
		sta jpg_temp2

		lda (jpg_crpoint),y
		eor #$80
		bpl jpg_torgb_poscr
		eor #$ff ; negcr
		clc
		adc #01
		tax
		lda jpg_temp+0
		clc
		adc jpg_crtab2,x
		sta jpg_temp+0
		lda jpg_temp+1
		adc #00
		beq :++
		bpl :+
		lda #00
		.byte $2c									; BIT $xxxx
:		lda #255
		.byte $2c									; BIT $xxxx
:		lda jpg_temp
		sta (jpg_cbpoint),y							; green
		lda (jpg_ypoint),y
		sec
		sbc jpg_crtab1,x
		bcs jpg_torgb_done
		lda #00
		beq jpg_torgb_done

jpg_torgb_poscr
		tax
		lda jpg_temp+0
		sec
		sbc jpg_crtab2,x
		sta jpg_temp+0
		lda jpg_temp+1
		sbc #00
		beq :++
		bpl :+
		lda #00
		.byte $2c									; BIT $xxxx
:		lda #255
		.byte $2c									; BIT $xxxx
:		lda jpg_temp
		sta (jpg_cbpoint),y
		lda (jpg_ypoint),y
		clc
		adc jpg_crtab1,x
		bcc jpg_torgb_done
		lda #255

jpg_torgb_done
		sta (jpg_ypoint),y							; red
		lda jpg_temp2
		sta (jpg_crpoint),y							; blue
		iny
		beq :++
:		jmp jpg_torgb_loop

:		inc jpg_ypoint+1
		inc jpg_cbpoint+1
		inc jpg_crpoint+1
		lda jpg_ypoint+1
		cmp #>jpg_cbbuf
		bcc :--
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; desample -- expand dct square by sample factor and reorg data.
; on entry: dest = destination buffer

jpg_linelen		.word $0130

jpg_desample 

		lda #00
jpg_desample_newrow		
		ldx jpg_vsamp
		stx jpg_huff								; temporary
jpg_desample_oldrow
		sta jpg_temp2								; current element
		lda #8
		sta jpg_count								; column
		ldy #00
:		ldx jpg_temp2
		lda jpg_trans,x
		ldx jpg_hsamp

jpg_desample_expand		
		sta (jpg_dest),y
		iny
		dex
		bne jpg_desample_expand
		inc jpg_temp2
		dec jpg_count
		bne :-

		lda jpg_dest								; next scanline
		clc
		adc jpg_linelen
		sta jpg_dest
		lda jpg_dest+1
		adc jpg_linelen+1
		sta jpg_dest+1

		lda jpg_temp2
		sec
		sbc #8										; start of row
		dec jpg_huff								; horizonal sampling
		bne jpg_desample_oldrow
		lda jpg_temp2
		cmp #64
		bne jpg_desample_newrow
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; dequantize the vector vec
; mult is 16 bit signed x 8 bit unsigned with 16-bit result, so sign etc. are taken care of automatically.
; result -> trans

jpg_quanttab
		.word jpg_qt0
		.word jpg_qt1
		.word jpg_qt2

jpg_zigzag
		.byte   0,   2,   16,  32,  18,   4,   6,  20		; table to un-zigzag coeffs
		.byte  34,  48,   64,  50,  36,  22,   8,  10		; multiples of 2, since 2 byte result.
		.byte  24,  38,   52,  66,  80,  96,  82,  68
		.byte  54,  40,   26,  12,  14,  28,  42,  56
		.byte  70,  84,   98, 112, 114, 100,  86,  72
		.byte  58,  44,   30,  46,  60,  74,  88, 102
		.byte  116, 118, 104,  90,  76,  62,  78,  92
		.byte  106, 120, 122, 108,  94, 110, 124, 126

jpg_dequantize
		ldx jpg_curcomp
		lda jpg_cquant,x
		asl
		tax
		lda jpg_quanttab,x
		sta jpg_quantp
		lda jpg_quanttab+1,x
		sta jpg_quantp+1

		ldx #63
jpg_dequantize_loop
		txa
		tay
		lda (jpg_quantp),y
		sta jpg_mult1lo
		sta jpg_mult1hi
		eor #$ff
		clc
		adc #1
		sta jpg_mult2lo
		sta jpg_mult2hi

		ldy jpg_veclo,x
		bne :+
		sty jpg_bitslo
		sty jpg_bitshi
		beq jpg_dequantize_high
:		lda (jpg_mult1lo),y
		sec
		sbc (jpg_mult2lo),y
		sta jpg_bitslo
		lda (jpg_mult1hi),y
		sbc (jpg_mult2hi),y
		sta jpg_bitshi

jpg_dequantize_high
		ldy jpg_vechi,x
		lda (jpg_mult1lo),y
		sec
		sbc (jpg_mult2lo),y
		clc
		adc jpg_bitshi

		ldy jpg_zigzag,x							; un-zigzag
		iny
		sta jpg_trans,y
		dey
		lda jpg_bitslo
		sta jpg_trans,y
		dex
		bpl jpg_dequantize_loop
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

jpg_curbuf		.word 0
jpg_rend		.byte 0
jpg_currow		.byte 0
jpg_curcol		.byte 0
jpg_rendflag	.byte 0

jpg_fetch											; fetch the data
		lda #00
		sta jpg_dest+1
		lda jpg_curcol
		cmp #37										; catches neg too
		rol jpg_rendflag							; c set?

		asl											; offset = col*8
		rol jpg_dest+1
		asl
		rol jpg_dest+1
		asl
		rol jpg_dest+1
		adc jpg_curbuf								; ybuf, etc.
		sta jpg_dest+0 								; data storage
		lda jpg_curbuf+1
		adc jpg_dest+1
		sta jpg_dest+1
:decode
		jsr jpg_decodedc
		lda jpg_error
		bne :+
		jsr jpg_decodeac
		lda jpg_error
		bne :+
		lda jpg_rendflag
		bne :+
		jsr jpg_dequantize
		jsr jpg_idct2d
		jmp jpg_desample

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

; buffer size = 38*8 = $0130 * 8 lines

jpg_buflen		.word $0980

jpg_col			.byte 0
jpg_coloff		.byte 0								; col offset

jpg_readdataunit
		sta jpg_curbuf+0
		sty jpg_curbuf+1
		stx jpg_curcomp
		lda #00
		sta jpg_rend

		ldy #00										; compute expansion factors
		tya											; maxsamp/samp
		clc
:		iny
		adc jpg_csamph,x
		cmp jpg_csamph								; max
		bcc :-
		sty jpg_hsamp
		lda #00
		tay
		clc
:		iny
		adc jpg_csampv,x
		cmp jpg_csampv
		bcc :-
		sty jpg_vsamp

		lda jpg_csampv,x							; vert samp
		sta jpg_temp

jpg_readdataunit_loopy
		ldx jpg_curcomp
		lda jpg_csamph,x							; horiz sampling
		sta jpg_temp+1
		lda jpg_col
		sec
		sbc jpg_coloff
		sta jpg_curcol

jpg_readdataunit_loopx
		lda jpg_rend
		sta jpg_rendflag
		jsr jpg_fetch								; FETCH!!!
		lda jpg_error
		bne jpg_readdataunit_end
		lda jpg_curcol
		clc
		adc jpg_hsamp
		sta jpg_curcol
		dec jpg_temp+1
		bne jpg_readdataunit_loopx

		ldx jpg_vsamp
jpg_readdataunit_nextrow
		lda jpg_curbuf
		clc
		adc jpg_buflen+0
		sta jpg_curbuf
		lda jpg_curbuf+1
		adc jpg_buflen+1
		sta jpg_curbuf+1
		dex
		bne jpg_readdataunit_nextrow

		dec jpg_temp
		bne jpg_readdataunit_loopy

jpg_readdataunit_end		
		rts

; ----------------------------------------------------------------------------------------------------------------------------------------
