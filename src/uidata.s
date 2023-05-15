; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ main windows

root
		UIELEMENT_ADD ui_root1,					root,				windows,				0,  0, 80, 50,  0,		$ffff,						uidefaultflags
		UIELEMENT_END

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ main windows

windows

		UIELEMENT_ADD ui_windows0,				window,				window0area,			 0,  0, 80, 50,  0,		$ffff,						uidefaultflags
		UIELEMENT_END

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ child windows

window0area
		UIELEMENT_ADD fa1nineslice,				nineslice,			filearea1elements,		 1,  0, 38, 17,  0,		$ffff,						uidefaultflags
		UIELEMENT_END

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ scrollbar elements

filearea1elements
		UIELEMENT_ADD fa1filebox,				filebox,			$ffff,					 3,  2, -7, -4,  0,		fa1filebox_data,			uidefaultflags
		UIELEMENT_ADD fa1scrollbartrack,		scrolltrack,		$ffff,					-3,  2,  2, -4,  0,		fa1scrollbar_data,			uidefaultflags
		UIELEMENT_END

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ scrollbar elements

filetabgroup1_data			.word $ffff,														0								; group bitmask

tabgroup1_data				.word $ffff,														0								; group bitmask

fa1scrollbar_data			.word fa1scrollbar_functions, 										0, 0, 20, fa1filebox			; start position, selection index, number of entries, ptr to list
fa1filebox_data				.word fa1scrollbar_functions,			filebox1_functions,			fa1scrollbar_data, fa1boxtxt, fa1directorytxt

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ listeners

fa1scrollbar_functions			.word fa1scrollbartrack,				uiscrolltrack_draw
								.word fa1filebox,						uifilebox_draw
								.word $ffff

filebox1_functions				.word fa1filebox,						userfunc_openfile
								.word $ffff

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

userfunc_openfile

		jsr uifilebox_getstringptr									; get filename/dir string

		ldx #$00
		ldy #$03													; skip attributes, file type and length-until-extension
:		lda (zpptrtmp),y
		beq :+
		and #$7f
		sta sdc_transferbuffer,x
		iny
		inx
		bra :-

:		sta sdc_transferbuffer,x

		ldy #$00													; get attribute and check if it's a directory
		lda (zpptrtmp),y
		and #%00010000
		cmp #%00010000
		bne :+

		jsr sdc_chdir
		jsr uifilebox_opendir
		jsr uifilebox_draw
		rts

:		jsr sdc_openfile

		rts

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
		