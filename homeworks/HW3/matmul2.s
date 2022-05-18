	.file	"matmul2.c"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"starting multiply"
.LC3:
	.string	"a result %g \n"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB11:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	$.LC0, %edi
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$80024, %rsp
	.cfi_offset 15, -24
	.cfi_offset 14, -32
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	.cfi_offset 3, -56
	leaq	7(%rsp), %r8
	subq	$80000, %rsp
	leaq	7(%rsp), %r15
	movq	%r8, %r14
	subq	$80000, %rsp
	andq	$-8, %r8
	movq	%r8, -56(%rbp)
	shrq	$3, %r14
	movq	%r15, %r13
	leaq	7(%rsp), %r12
	call	puts
	shrq	$3, %r13
	movq	%r12, %rbx
	movq	-56(%rbp), %r8
	andq	$-8, %r15
	shrq	$3, %rbx
	andq	$-8, %r12
	xorl	%edi, %edi
	movq	.LC1(%rip), %rax
	movq	%rax, 808(,%r14,8)
	movq	.LC2(%rip), %rax
	movq	%rax, 808(,%r13,8)
.L2:
	leaq	(%r8,%rdi), %rsi
	leaq	(%r15,%rdi), %rcx
	xorl	%eax, %eax
	leaq	(%r12,%rdi), %rdx
	.p2align 4,,10
	.p2align 3
.L3:
	movupd	(%rsi,%rax), %xmm0
	movupd	(%rcx,%rax), %xmm1
	mulpd	%xmm1, %xmm0
	movups	%xmm0, (%rdx,%rax)
	addq	$16, %rax
	cmpq	$800, %rax
	jne	.L3
	addq	$800, %rdi
	cmpq	$80000, %rdi
	jne	.L2
	movsd	808(,%rbx,8), %xmm0
	movl	$.LC3, %edi
	movl	$1, %eax
	call	printf
	leaq	-40(%rbp), %rsp
	xorl	%eax, %eax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE11:
	.size	main, .-main
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC1:
	.long	0
	.long	1074528256
	.align 8
.LC2:
	.long	858993459
	.long	1072902963
	.ident	"GCC: (GNU) 10.2.1 20200723 (Red Hat 10.2.1-1)"
	.section	.note.GNU-stack,"",@progbits
