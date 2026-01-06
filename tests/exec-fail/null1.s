	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	call M_Main_main
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
M_Main_new:
	pushq %rbp
	movq %rsp, %rbp
L_1:
	movq %rbp, %rsp
	popq %rbp
	ret
M_Main_main:
	pushq %rbp
	movq %rsp, %rbp
	pushq $0
	xorq %rdi, %rdi
	movq %rdi, -8(%rbp)
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_string
L_2:
	movq %rbp, %rsp
	popq %rbp
	ret
M_A_new:
	pushq %rbp
	movq %rsp, %rbp
L_3:
	movq %rbp, %rsp
	popq %rbp
	ret
print_string:
        leaq    8(%rdi), %rsi
        mov     $S_string, %rdi
        xorq    %rax, %rax
        call    _printf
        ret
string_of_int:
        pushq   %rdi
        movq    $30, %rdi
        call    _malloc
        movq    $C_String, (%rax)
        leaq    8(%rax), %rdi
        movq    $S_int, %rsi
        popq    %rdx
        pushq   %rax
        xorq    %rax, %rax
        call    _sprintf
        popq    %rax
        ret
string_concat:
        addq    $8, %rdi
        pushq   %rdi
        addq    $8, %rsi
        pushq   %rsi
        call    _strlen
        pushq   %rax
        movq    8(%rsp), %rdi
        call    _strlen
        addq    (%rsp), %rax
        leaq    9(%rax), %rdi
        call    _malloc
        movq    $C_String, (%rax)
        movq    %rax, (%rsp)
        leaq    8(%rax), %rdi
        movq    16(%rsp), %rsi
        call    _strcpy
        movq    (%rsp), %rdi
        addq    $8, %rdi
        movq    8(%rsp), %rsi
        call    _strcat
        popq    %rax
        addq    $16, %rsp
        ret
string_equals:
        movq    16(%rsp), %rdi
        addq    $8, %rdi
        movq    8(%rsp), %rsi
        addq    $8, %rsi
        call    _strcmp
        testq   %rax, %rax
        sete    %al
        movzbq  %al, %rax
        ret
M_Object_new:
        ret
_malloc:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    malloc
	movq    %rbp, %rsp
	popq    %rbp
	ret
_printf:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    printf
	movq    %rbp, %rsp
	popq    %rbp
	ret
_sprintf:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    sprintf
	movq    %rbp, %rsp
	popq    %rbp
	ret
_strlen:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strlen
	movq    %rbp, %rsp
	popq    %rbp
	ret
_strcmp:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strcmp
	movq    %rbp, %rsp
	popq    %rbp
	ret
_strcat:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strcat
	movq    %rbp, %rsp
	popq    %rbp
	ret
_strcpy:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strcpy
	movq    %rbp, %rsp
	popq    %rbp
	ret
	.data
### chaînes
S_string:
	.string "%s"
S_int:
	.string "%d"
### classes
C_String:
	.quad C_Object
        .quad string_equals
C_Object:
        .quad 0
C_A:
	.quad C_Object
	.quad 0
	.quad 0
C_Main:
	.quad C_Object
	.quad 0
	.quad M_Main_main
