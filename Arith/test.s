.data
newline: 	.asciiz "\n"
x: 	.word 5
.text
	.globl	main
main:
	lw $t0, x
	sub $sp, $sp, 4
	sw $t0, ($sp)
	li $t0, 3
	sub $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, ($sp)
	add $sp, $sp, 4
	lw $t1, ($sp)
	add $sp, $sp, 4
	bge $t1, $t0, else1
	lw $t0, x
	sub $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, ($sp)
	add $sp, $sp, 4
	li $v0, 1
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	j main1
else1:
main1:
loop1:
	lw $t0, x
	sub $sp, $sp, 4
	sw $t0, ($sp)
	li $t0, 6
	sub $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, ($sp)
	add $sp, $sp, 4
	lw $t1, ($sp)
	add $sp, $sp, 4
	bge $t1, $t0, exit1
	lw $t0, x
	sub $sp, $sp, 4
	sw $t0, ($sp)
	li $t0, 1
	sub $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, ($sp)
	add $sp, $sp, 4
	lw $t1, ($sp)
	add $sp, $sp, 4
	add $t0, $t0, $t1
	sub $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, ($sp)
	add $sp, $sp, 4
	sw $t0, x
	j loop1
exit1:
	lw $t0, x
	sub $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, ($sp)
	add $sp, $sp, 4
	li $v0, 1
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	li $v0, 10
	syscall
