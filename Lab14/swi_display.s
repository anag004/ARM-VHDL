	b User @Start in user mode
	b swi_display1
	b swi_display4
gap1:	.space 0x0C
	b IRQ

IRQ:	mov r4, #0
	mov r5, #4
	mov r3, #24
	ldr r2, [r3] @ Load the digit position
	cmp r2, #4  @ Check if it is multi-digit
	beq Multi
	
	mov r3, #8
	ldr r0, [r3]
	cmp r2, #0  @ Check if zero position
	streq r0, [r5]
	mov r3, #14 @ Anode - 1110
	streq r3, [r4]
	
	mov r3, #12
	ldr r0, [r3]
	cmp r2, #1
	streq r0, [r5]
	mov r3, #13 @ Anode - 1101
	streq r3, [r4]

	mov r3, #16
	ldr r0, [r3]
	cmp r2, #2
	streq r0, [r5]
	mov r3, #11 @ Anode - 1011
	streq r3, [r4]

	mov r3, #20
	ldr r0, [r3]
	cmp r2, #3
	streq r0, [r5]
	mov r3, #7 @ Anode - 0111
	streq r3, [r4]
	movs pc, lr

@ Display multiple digits
Multi: 
	mov r3, #0
	mov r7, #4
	ldr r2, [r3] @ Load anode value
	
	cmp r2, #14    @ Anode is at position 0
	mov r4, #13    @ New anode value
	mov r5, #12    @ Position of digit
	ldr r6, [r5]   @ Load the digit
	streq r6, [r7] @ Store the digit
	streq r4, [r3] @ Store the new anode 	   

	cmp r2, #13
	mov r4, #11
	mov r5, #16
	ldr r6, [r5]
	streq r6, [r7]
	streq r4, [r3]

	cmp r2, #11
	mov r4, #7
	mov r5, #20
	ldr r6, [r5]
	streq r6, [r7]
	streq r4, [r3]
	
	cmp r2, #7
	mov r4, #14
	mov r5, #8
	ldr r6, [r5]
	streq r6, [r7]
	streq r4, [r3]

	movs pc, lr

swi_display1: @r0 contains digit, r1 contains position
	mov r3, #24
	str r1, [r3] @ Store the position
	
	mov r3, #8
	cmp r1, #0 @ If the position is 0
	streq r0, [r3]
	
	mov r3, #12
	cmp r1, #1
	streq r0, [r3]
	
	mov r3, #16
	cmp r1, #2
	streq r0, [r3]
	
	mov r3, #20
	cmp r1, #3
	streq r0, [r3]
	
	movs pc, lr

User:
	mov r0, #1
	mov r1, #2
	mov r2, #3
	mov r3, #4
	swi 0x08
	b User		

swi_display4: @ Display a 4 digits, stored in r0, r1, r2, r3
	mov r5, #24
	mov r6, #4
	str r6, [r5] @ Store 4 in the 4DIGIT memory
	
	mov r6, #8
	str r0, [r6]

	mov r6, #12
	str r1, [r6]

	mov r6, #16
	str r2, [r6]

	mov r6, #20
	str r3, [r6]
	
	movs pc,lr
	
