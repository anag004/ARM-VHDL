.equ SWI_Exit, 0x11
.text



.align	
	mov r0, #12
	mov r1, #-1
	mov r2, #3
	mov r3, #0xffffffff

	str r1, [r0]
	strh r1, [r0, #6]
	strb r1, [r0, #9]
	ldr r4, [r0]
	ldr r4, [r0], r2, lsl #1
	ldrh r4, [r0, #-6]!
	ldrsh r4, [r0, #6]!
	ldrb r4, [r0, #3]
	ldrsb r4, [r0]

G:	swi SWI_Exit
.data
.end