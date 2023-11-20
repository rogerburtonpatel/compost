	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0
	.globl	"_<="                           ; -- Begin function <=
	.p2align	2
"_<=":                                  ; @"<="
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	w0, w1
	cset	w0, le
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_>="                           ; -- Begin function >=
	.p2align	2
"_>=":                                  ; @">="
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	w0, w1
	cset	w0, ge
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_<"                            ; -- Begin function <
	.p2align	2
"_<":                                   ; @"<"
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	w0, w1
	cset	w0, lt
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_>"                            ; -- Begin function >
	.p2align	2
"_>":                                   ; @">"
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	w0, w1
	cset	w0, gt
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_neg                            ; -- Begin function neg
	.p2align	2
_neg:                                   ; @neg
	.cfi_startproc
; %bb.0:                                ; %entry
	neg	w0, w0
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_%"                            ; -- Begin function %
	.p2align	2
"_%":                                   ; @"%"
	.cfi_startproc
; %bb.0:                                ; %entry
	sdiv	w8, w0, w1
	msub	w0, w8, w1, w0
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_/"                            ; -- Begin function /
	.p2align	2
"_/":                                   ; @"/"
	.cfi_startproc
; %bb.0:                                ; %entry
	sdiv	w0, w0, w1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_*"                            ; -- Begin function *
	.p2align	2
"_*":                                   ; @"*"
	.cfi_startproc
; %bb.0:                                ; %entry
	mul	w0, w0, w1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_-"                            ; -- Begin function -
	.p2align	2
"_-":                                   ; @-
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	w0, w0, w1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_+"                            ; -- Begin function +
	.p2align	2
"_+":                                   ; @"+"
	.cfi_startproc
; %bb.0:                                ; %entry
	add	w0, w0, w1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_u="                           ; -- Begin function u=
	.p2align	2
"_u=":                                  ; @"u="
	.cfi_startproc
; %bb.0:                                ; %entry
	mov	w0, #1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_b="                           ; -- Begin function b=
	.p2align	2
"_b=":                                  ; @"b="
	.cfi_startproc
; %bb.0:                                ; %entry
	eon	w8, w0, w1
	and	w0, w8, #0x1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_s="                           ; -- Begin function s=
	.p2align	2
"_s=":                                  ; @"s="
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	x0, x1
	cset	w0, eq
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_i="                           ; -- Begin function i=
	.p2align	2
"_i=":                                  ; @"i="
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	w0, w1
	cset	w0, eq
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_in                             ; -- Begin function in
	.p2align	2
_in:                                    ; @in
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	bl	_getchar
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	"_print-unit"                   ; -- Begin function print-unit
	.p2align	2
"_print-unit":                          ; @print-unit
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh0:
	adrp	x0, l_unit@PAGE
Lloh1:
	add	x0, x0, l_unit@PAGEOFF
	bl	_printf
	mov	w0, wzr
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	"_print-bool"                   ; -- Begin function print-bool
	.p2align	2
"_print-bool":                          ; @print-bool
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh2:
	adrp	x8, l_false@PAGE
Lloh3:
	add	x8, x8, l_false@PAGEOFF
Lloh4:
	adrp	x9, l_true@PAGE
Lloh5:
	add	x9, x9, l_true@PAGEOFF
	tst	w0, #0x1
	csel	x0, x9, x8, ne
	bl	_printf
	mov	w0, wzr
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
	.cfi_endproc
                                        ; -- End function
	.globl	"_print-int"                    ; -- Begin function print-int
	.p2align	2
"_print-int":                           ; @print-int
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #32
	.cfi_def_cfa_offset 32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w8, w0
Lloh6:
	adrp	x0, l_fmt_int@PAGE
Lloh7:
	add	x0, x0, l_fmt_int@PAGEOFF
	str	x8, [sp]
	bl	_printf
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	mov	w0, wzr
	add	sp, sp, #32
	ret
	.loh AdrpAdd	Lloh6, Lloh7
	.cfi_endproc
                                        ; -- End function
	.globl	"_print-sym"                    ; -- Begin function print-sym
	.p2align	2
"_print-sym":                           ; @print-sym
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	bl	_printf
	mov	w0, wzr
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #32
	.cfi_def_cfa_offset 32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w0, #60
	bl	__fib
	mov	w8, w0
Lloh8:
	adrp	x0, l_fmt_int.1@PAGE
Lloh9:
	add	x0, x0, l_fmt_int.1@PAGEOFF
	str	x8, [sp]
	bl	_printf
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	mov	w0, wzr
	add	sp, sp, #32
	ret
	.loh AdrpAdd	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.globl	__fib                           ; -- Begin function _fib
	.p2align	2
__fib:                                  ; @_fib
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w19, w0
	subs	w0, w0, #1
	b.eq	LBB20_3
; %bb.1:                                ; %entry
	cbz	w19, LBB20_4
; %bb.2:                                ; %else3
	bl	__fib
	mov	w20, w0
	sub	w0, w19, #2
	bl	__fib
	add	w19, w20, w0
	b	LBB20_4
LBB20_3:                                ; %then2
	mov	w19, #1
LBB20_4:                                ; %common.ret
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	mov	w0, w19
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_unit:                                 ; @unit
	.asciz	"unit"

l_true:                                 ; @true
	.asciz	"true"

l_false:                                ; @false
	.asciz	"false"

l_fmt_int:                              ; @fmt_int
	.asciz	"%d"

l_fmt_int.1:                            ; @fmt_int.1
	.asciz	"%d"

.subsections_via_symbols
