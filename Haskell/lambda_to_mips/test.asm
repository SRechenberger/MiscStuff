.data
.text
# BEGIN ((\x . (x x)) (\x . (x x)))
	move $fp, $sp
# BEGIN codeP ((\x . (x x)) (\x . (x x)))
# BEGIN codeE (\x . (x x))
	j lambda0_end
lambda0:
# Building Stack Frame
	addi $sp, $sp, -12
	sw $fp, 8($sp)
	move $fp, $sp
	sw $ra, 4($sp)
	sw $a0, 0($sp)
	sw $a0, 0($gp)
	addi $gp, $gp, 4
# BEGIN codeE (x x)
# BEGIN codeE x
	addi $t0, $gp, -4
	addi $t0, $t0, 0
	lw $v1, 0($t0)
# END codeE x
	move $v0, $v1
# BEGIN codeE x
	addi $t0, $gp, -4
	addi $t0, $t0, 0
	lw $v1, 0($t0)
# END codeE x
	move $a0, $v1
	jalr $v0
# END codeE (x x)
# Removing Stack Frame
	lw $fp, 8($sp)
	lw $ra, 4($sp)
	lw $a0, 0($sp)
	addi $sp, $sp, 12
	jr $ra
lambda0_end:
	la $v1, lambda0
# END codeE (\x . (x x))
	move $v0, $v1
# BEGIN codeE (\x . (x x))
	j lambda1_end
lambda1:
# Building Stack Frame
	addi $sp, $sp, -12
	sw $fp, 8($sp)
	move $fp, $sp
	sw $ra, 4($sp)
	sw $a0, 0($sp)
	sw $a0, 0($gp)
	addi $gp, $gp, 4
# BEGIN codeE (x x)
# BEGIN codeE x
	addi $t0, $gp, -8
	addi $t0, $t0, 4
	lw $v1, 0($t0)
# END codeE x
	move $v0, $v1
# BEGIN codeE x
	addi $t0, $gp, -8
	addi $t0, $t0, 4
	lw $v1, 0($t0)
# END codeE x
	move $a0, $v1
	jalr $v0
# END codeE (x x)
# Removing Stack Frame
	lw $fp, 8($sp)
	lw $ra, 4($sp)
	lw $a0, 0($sp)
	addi $sp, $sp, 12
	jr $ra
lambda1_end:
	la $v1, lambda1
# END codeE (\x . (x x))
	move $a0, $v1
	jalr $v0
	move $a0, $v1
	li $v0, 4
	syscall
# END codeP ((\x . (x x)) (\x . (x x)))
# END ((\x . (x x)) (\x . (x x)))
	li $v0, 10
	syscall

