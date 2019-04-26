.text
mov r0,#1
mov r1,#4
mov r2,#3
muls r4,r1,r2
mlas r4,r1,r2,r0
mov r0,#-2
mov r1,#-5
mov r2,#-4
umulls r4,r1,r2,r0
umlals r4,r1,r2,r0
mov r0,#2
mov r1,#-4
mov r2,#3
smulls r4,r1,r2,r0
smlals r4,r1,r2,r0
mov r5,#0
muls r4,r4,r5
swi 0x11

.equ SWI_Exit, 0x11
.text
  mov r0,#20
 mov r1, #0
 ldr r3, [r0]
 add r6, r3, #80
L: ldr r5, [r3, #0]
 add r1, r1, r5
 add r3, r3, #4
 cmp r3, r6 
 blt L
 swi SWI_Exit
 
 mov r1, #0
	mov r2, #1
	mov r3, #0
	mov r4, #0
	add r4, r4, #8
	mov r5, #100
	str r1, [r5,#0]
	str r2, [r5,#4]
Fib:cmp r4, #40
	addne r3, r1, r2
	movne r1, r2
	movne r2, r3
	str r3, [r5, r4]
	addne r4, r4, #4
	bal Fib
	swi 0x11
	
.text
	mov r1, #1
	mov r2, #80
	mov r3, #0
	mov r4, #2
xyz:
	str r1, [r2, #0]		
	add r2, r2, #4	
	add r1, r1, r4
	cmp r1, #10				
	movne r2, #80	
	bal xyz
	ldr r5, [r2, #120]
abc:
	ldr r4, [r2, #0]		
	add r3, r3, r4		
	add r2, r2, #4		
	cmp r2, r5
	bne abc
	.end
	
.text
mov r0,#16
mov r1,#15
mov r2,#17
cmp r0,#16
addeq r4,r0,r3
cmp r1,#16
movle r4,r0
addlt r1, r1, #1
cmp r2,#16
bgt j
mov r0,#0
mov r1,#0
mov r2,#0
j: suble r2,r2,#1
swi 0x11

.text
mov r0,#8
cmp r0,#8
addhs r1, r0, #8
sublo r2, r0, #16
cmp r1, #32
bmi k
mov r0,#0
k:cmp r0, #16
movpl r2,r0
mov r4, #10
cmp r4, #0xffffffff
addvs r2,r2,#8
subvc r2,r2,#10
bal e
mov r3,#0
e:swi 0x11

