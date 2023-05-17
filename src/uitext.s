.segment "TEXT"

uitxt_save			.byte "save", 0

uitxt_jpgstart		.byte "start jpg parse", 0
uitxt_jpgheader		.byte "jpg header", 0

uitxt_marker_sos	.byte "marker sos - start of scan", 0
uitxt_marker_dri	.byte "marker dri", 0
uitxt_marker_dqt	.byte "marker dqt - define quantisation table", 0
uitxt_marker_dht	.byte "marker dht - define huffman table", 0
uitxt_marker_sof	.byte "marker sof - start of frame", 0

la1boxtxt
.repeat 128, I
					.word .ident(.sprintf("la1boxtxt%s", .string(I)))
.endrepeat
					.word $ffff

.repeat 128,I
	.ident(.sprintf("la1boxtxt%s", .string(I)))
		.byte " ", 0
.endrepeat

fa1directorytxt		.byte 0, "                              "

.align 256

fa1boxtxt			.word fa1boxtxt00
					.word $ffff

					.repeat 512
					.byte 0
					.endrepeat

.align 256			; leave enough room for fa1boxtxt to grow. 256 directory entries allowed

fa1boxtxt00			.byte %00010000, $31, $03, "",            0
