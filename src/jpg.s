
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
