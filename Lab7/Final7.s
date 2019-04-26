;; Program#1:

;; Fibonacci number
	mov r1, #0
	mov r2, #1
	mov r3, #0
	mov r4, #0
	add r4, r4, #8
	mov r5, #100
	str r1, [r5, #0]
	str r2, [r5, #4]
Fib: 	
	add r3, r1, r2
	mov r1, r2
	mov r2, r3
	add r4, r4, #4
	cmp r4, #40
	bne Fib	
	swi 0x11

;; Program #2:
;; Swaps two numbers
;; Makes it 2, 3, 1
	mov r0, #1
	mov r1, #2
	mov r2, #3
	add r0, r0, r1
	sub r1, r0, r1
	sub r0, r0, r1
	add r1, r1, r2
	sub r2, r1, r2
	sub r1, r1, r2

;; Program #3:
;; Tests all the branching, compare instructions
	mov r1, #12
	mov r2, #13
	cmp r1, #12
	beq branch1
branch2:
	b endbranch	
branch1:
	cmp r1, r2
	bne branch2
endbranch:

;; Program #4:
;; Tests load/store instructions
mov r0, #100
mov r1, #123
str r1, [r0, #20]
add r0, r0, #40
ldr r2, [r0, #-20]

;; Program #5:
;; Program to multiply two numbers
	mov r0, #16
	mov r1, #63
	mov r2, #0
mult:	
	cmp r0, #0
	beq End
	add r2, r2, r1
	sub r0, r0, #1
	b mult
End:	


;; Program #6:
@@@ Find sum of array elements
	.text
@ Writing to the array
	mov r1, #1
	mov r2, #100		@ Starting address of the array
Lab1:
	str r1, [r2, #0]		@ Stores i at array[i]
	add r2, r2, #4		@ Pointing to next element 
	add r1, r1, #1
	cmp r1, #11		@ Loop termination check
	bne Lab1

@ Reading from the array
	mov r3, #0			@ Initialize sum
	mov r2, #100		@ Initialize address pointer
Lab2:
	sub r1, r1, #1
	cmp r1, #0			@ Loop termination check
	beq Over
	ldr r4, [r2, #0]		@ array[i] is read
	add r3, r3, r4		@ Add to the sum
	add r2, r2, #4		@ Pointing to next element 
	b Lab2
Over:


;; Program #7:
;; This calculates sum from one end to other and back
	mov r1, #1
	mov r2, #100		
	mov r3, #200
B1: str r1, [r2, #0]		
	add r2, r2, #4		
	add r1, r1, #1
	cmp r1, #11	
	bne B1
B2: sub r1, r1, #1
	str r1, [r3, #0]		
	add r3, r3, #4		 	
	cmp r1, #0		
	bne B2
	mov r2, #100
	mov r3, #200
	mov r1, #10
B3:	sub r1, r1, #1
	cmp r1, #0			
	beq OverEnd
	ldr r4, [r2, #0]
	ldr r5, [r3, #0]
	add r6, r4, r5
	cmp r6, #11
	bne Over
	add r2, r2, #4		
	add r3, r3, #4
	b B3
OverEnd:


;; Program #8:
;; Same as 1   





