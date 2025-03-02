.section .rodata
.text
.globl main
main:
    push %rbp
    movq %rsp, %rbp
    # This allocate ensures stack is 16-byte aligned
    subq $32, %rsp
    movl $20, %edi
    call _malloc
    movq %rax, -8(%rbp)      # Store pointer in 'a'
    movq %rax, %r10          # Temp register for initialization
    movl $4, (%r10)          # Store length (4) at offset 0
    movl $43, 4(%r10)       # a[0] = 43
    movl $2, 8(%r10)         # a[1] = 2
    movl $18, 12(%r10)       # a[2] = 18
    movl $1, 16(%r10)        # a[3] = 1
    movq -8(%rbp), %r10
    movq %r10, -16(%rbp)
    movq -16(%rbp), %r9
    movl $0, %r10d
    call _arrLoad4
    movl %r9d, -24(%rbp)
    movl -24(%rbp), %esi
    movl (%rsi), %r8d
    movl %r8d, -28(%rbp)
    movl -28(%rbp), %edi
    call _printi
    call _println
    movq %rbp, %rsp
    pop %rbp
    movq $0, %rax
    ret

.section .rodata
# length of prints_str0
    .int 4
.L_prints_str0:
    .asciz "%.*s"
.text
_prints:
    push %rbp
    movq %rsp, %rbp
    andq $-16, %rsp
    movq %rdi, %rdx
    movl -4(%rdi), %esi
    leaq .L_prints_str0(%rip), %rdi
    movb $0, %al
    call printf@PLT
    movq $0, %rdi
    call fflush@PLT
    movq %rbp, %rsp
    pop %rbp
    ret

.text
_arrLoad4:
    push %rbx
    testl %r10d, %r10d
    cmovl %r10, %rsi
    jl _errOutOfBounds
    movl -4(%r9), %ebx
    cmpl %ebx, %r10d
    cmovge %r10, %rsi
    jge _errOutOfBounds
    movl (%r9, %r10, 4), %r9d
    pop %rbx
    ret

.section .rodata
# length of errOutOfMemory_str0
    .int 28
.L_errOutOfMemory_str0:
    .asciz "fatal error: out of memory\n"
.text
_errOutOfMemory:
    andq $-16, %rsp
    leaq .L_errOutOfMemory_str0(%rip), %rdi
    call _prints
    movb $-1, %dil
    call exit@PLT

.text
_malloc:
    push %rbp
    movq %rsp, %rbp
    andq $-16, %rsp
    call malloc@PLT
    cmpq $0, %rax
    je _errOutOfMemory
    movq %rbp, %rsp
    pop %rbp
    ret

.section .rodata
# length of errOutOfBounds_str0
    .int 41
.L_errOutOfBounds_str0:
    .asciz "fatal error: array index %d out of bounds"
.text
_errOutOfBounds:
    andq $-16, %rsp
    leaq .L_errOutOfBounds_str0(%rip), %rdi
    movb $0, %al
    call printf@PLT
    movq $0, %rdi
    call fflush@PLT
    movb $-1, %dil
    call exit@PLT

.section .rodata
# length of printi_str0
    .int 2
.L_printi_str0:
    .asciz "%d"
.text
_printi:
    push %rbp
    movq %rsp, %rbp
    andq $-16, %rsp
    movl %edi, %esi
    leaq .L_printi_str0(%rip), %rdi
    movb $0, %al
    call printf@PLT
    movq $0, %rdi
    call fflush@PLT
    movq %rbp, %rsp
    pop %rbp
    ret

.section .rodata
# length of println_str0
    .int 0
.L_println_str0:
    .asciz ""
.text
_println:
    push %rbp
    movq %rsp, %rbp
    andq $-16, %rsp
    leaq .L_println_str0(%rip), %rdi
    call puts@PLT
    movq $0, %rdi
    call fflush@PLT
    movq %rbp, %rsp
    pop %rbp
    ret

