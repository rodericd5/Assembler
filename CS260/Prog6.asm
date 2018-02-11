; *************************************************************
; Student Name: Roderic Deichler
; COMSC-260 Fall 2016
; Date:11/09/2016
; Assignment # 6
; Version of Visual Studio used:2015
; Did program compile? Yes
; Did program produce correct results? Yes
;
; Estimate of time in hours to complete assignment: 5
;
; Short description of what program does: This program implements a series of math functions made 
; to show how one can use bitmap addressing and unsigned math to evaluate a series of numbers.  
; It uses a while loop and macros to display the results obtained by the math functions.
;
; *************************************************************
;
; Reminder: each assignment should be the result of your
; individual effort with no collaboration with other students.
;
; Reminder: every line of code must be commented and formatted
; per the ProgramExpectations.pdf file on the class web site
;
; *************************************************************

.386				   		;identifies minimum CPU for this program

.MODEL flat,stdcall    				;flat - protected mode program
                       				;stdcall - enables calling of MS_windows programs

					   	;allocate memory for stack
					   	;(default stack size for 32 bit implementation is 1MB without .STACK directive 
					   	;  - default works for most situations)
	
.STACK 4096            				;allocate 4096 bytes (1000h) for stack

;*******************MACROS********************************

;*************mPrtChar macro****************
;mPrtChar - used to print single characters
;usage: mPrtChar character
;ie to display a 'm' say:
;mPrtChar 'm'


mPrtChar MACRO  arg1			 	;arg1 is replaced by the name of character to be displayed
         push eax				;save eax
         mov al, arg1			 	;character to display should be in al
         call WriteChar			 	;display character in al
         pop eax				;restore eax
ENDM

;*************mPrtStr macro****************
;usage: mPrtStr nameOfString
;ie to display a 0 terminated string named message say:
;mPrtStr message

;Macro definition of mPrtStr. Wherever mPrtStr appears in the code
;it will  be replaced with 

mPrtStr  MACRO  arg1			 	;arg1 is replaced by the name of string to be displayed
         push edx				;save eax
         mov edx, offset arg1    		;address of str to display should be in dx
         call WriteString        		;display 0 terminated string
         pop edx				;restore eax
ENDM


;*************************PROTOTYPES*****************************

ExitProcess PROTO,
    dwExitCode:DWORD				     ;from Win32 api not Irvine to exit to dos with exit code

ReadChar PROTO					     ;Irvine code for getting a single char from keyboard
						     ;Character is stored in the al register.
						     ;Can be used to pause program execution until key is hit.

WriteBin PROTO					     ;Write the number in eax to the console in binary

WriteString PROTO			             ;Write a null terminated string to the console
						     					;Address of string is in edx

WriteDec PROTO					     ;Write number in eax to console			 

WriteChar PROTO					     ;Write the character in al to the console

WriteString PROTO				     ;Irvine code to write null-terminated string to output
											 ;EDX points to string
                        	
;************************  Constants  ***************************
LF         equ     0Ah                   		 ; ASCII Line Feed

$parm1 EQU DWORD PTR [ebp + 8]			 	 ;parameter 1
$parm2 EQU DWORD PTR [ebp + 12]			 	 ;parameter 2

operand1Pos    equ    0					 ;operand1 position global constant
operandBits    equ    14				 ;operand bit size global constant
operand2Pos    equ	  18				 ;operand2 position global constant
operatorPos	   equ    14				 ;operator position global constant
operatorBits   equ    4					 ;operator bit size global constant
;************************DATA SEGMENT****************************

.data
;use the following data for your program

;WARNING: do not display this file on a web page and then use copy and paste

;you must download this file by right clicking on the link
;then load it into visual studio and then copy the data 

                  ;|<----op1--->||op||<----op2--->|
                  ;01234567890123456789012345678901 
bitmap       dword 11000001001111010111101000001111b
             dword 01111010101111010111001010101001b
             dword 11001011010011010100000011001101b
             dword 01011010101111010100000010100001b
             dword 00000000000000010111111010011001b
             dword 11111111111111101011111111111111b
             dword 00000000011101101000000000000001b
             dword 00000000000000101000000000000000b
             dword 00000000000000101000001010111101b
             dword 11000101000101101000000000000000b
             dword 10000010001111111111101010001001b
             dword 11001010101111111100000010001001b
             dword 00000000000000111111111110010001b
             dword 11111111111111110100000011111011b
             dword 11110011110101110101111100111011b
             dword 10000000001101101110000000000101b
             dword 11111111111111101111111111111111b 
             dword 00000000000000101100000000000000b
             ARRAY_SIZE    equ     ($ - bitmap)

;The ARRAY_SIZE constant should be defined immediately after the operand2 array otherwise the value contained in it will not be correct.
;Do not change any of the above expressions or change the order of the above expressions.
;Do not try and type the above expressions into your program. Instead use copy and paste.

 titleMsg     byte "Program 6 by Roderic Deichler",LF,LF,0		;opening message
 spaceEqual	  byte " = ",0						;string for space and equal sign
 blankLine	  byte  LF,0						;string for a single line feed

;************************CODE SEGMENT****************************

.code

Main PROC
	mov		 esi, 0				  ;set esi to 0 for dword array addressing
	mPrtStr  titleMsg				  ;print the title message to console


looptop:
	cmp		 esi, ARRAY_SIZE		 ;compare esi to size of dword array
	jae		 done				 ;if esi >= size of the array, we have iterated though the entire array
	mov      eax, bitmap[esi]		 	 ;put the dword at esi of the bitmap into eax
	call	 WriteBin				 ;write in binary the dword at esi of the bitmap
	mPrtStr  blankline				 ;print a blank line for formatting
	mov		 cl, operand1Pos		 ;move operand1 position into cl for GetBits function
	mov		 ch, operandBits		 ;move operand bit size into ch for GetBits function
	call	 GetBits				 ;call GetBits function (eax = operand1)
	call	 WriteDec				 ;write eax to console in decimal
	mov		 edx, eax			 ;put eax into edx for safekeeping (edx = operand1)

	mov		 eax, bitmap[esi]		 ;put the dword at esi of the bitmap into eax
	mov		 cl, operatorPos		 ;move the operator position into cl for GetBits function
	mov		 ch, operatorBits		 ;move the operator bit size into ch for GetBits function
	call	 GetBits				 ;call GetBits function (eax = operator)
	or		 eax, 32			 ;set the 6th bit of eax (to make it into operator)
	mPrtChar ' '					 ;print a space for formatting
	call	 WriteChar				 ;print out the operator
	mPrtChar ' '					 ;print a space for formatting
	mov		 bl, al				 ;move al into bl to check operator later

	mov		 eax, bitmap[esi]		 ;put the dword at esi of the bitmap into eax
	mov		 cl, operand2Pos		 ;move the operand2 position into cl for GetBits function
	mov		 ch, operandBits		 ;mvoe the operand bit size into ch for GetBits function
	call	 GetBits				 ;call GetBits function (eax = operand2)
	call	 WriteDec				 ;write eax to console in decimal

	push	 eax					 ;push eax (operand 2) onto stack
	push	 edx					 ;push edx (operand 1) onto stack

	cmp		 bl, '+'			 ;compare bl to '+'
	jne		 subtract			 ;if not equal, continue down
	call	 doAdder				 ;otherwise call the add function
	jmp		 found				 ;end cycle 

	;option 2: the operator is subraction
subtract:
	cmp		 bl, '-'				 ;compare bl to '-'
	jne		 multiply				 ;if not equal, continue down
	call	 doSub					 	 ;otherwise call the sub function
	jmp		 found					 ;end cycle 

	;option 3: the operator is multiplication
multiply:
	cmp		 bl, '*'				 ;compare bl to '*'
	jne		 divide					 ;if not equal, continue down
	call	 doMultShift			 		 ;otherwise call the mult function
	jmp		 found					 ;otherwise end cycle

	;option 4: the operator is division
divide:
	call	 doDiv					 	 ;call do div since last two options use it
	cmp		 bl, '/'				 ;compare bl to '/'
	jne		 modulo					 ;if not equal, continue down
	jmp		 found					 ;end cycle

	;last/default option: the operator is modulo
modulo:
	mov		 eax, edx				 ;move remainder into eax for printing

found:
	mPrtStr	 spaceEqual				 	 ;print " = " for formatting
	call	 WriteDec					 ;print value in eax to console
	add		 esi, 4					 ;add 4 to esi for addressing bitmap
	mPrtStr  blankLine					 ;print blank line for formatting
	mPrtStr  blankLine				 	 ;print extra blank line for formatting
	jmp		 looptop				 ;continue loop

	;executes if loop breaks
done:
    call     ReadChar                	 			 ;pause execution
	INVOKE   ExitProcess,0           			 ;exit to dos: like C++ exit(0)

Main ENDP

;************** doSub - dword subtraction
;
; ENTRY - operand 1 and operand 2 are pushed on the stack
;
; EXIT -EAX = result (operand 1 - operand 2)
; REGS - List registers changed in this function
;
; note: Before calling doSub push operand 2 onto the stack and then push operand 1.
;
; to call doSub in main function:
; push 2 ;32 bit operand2
; push 11 ;32 bit operand1
; call doSub ;11  2 = 9 (answer in eax)
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;--------------
;The same circuits can be used for addition and subtraction because negative numbers are stored in 2s complement form
;You can do a subtraction by doing a twos complement and then addition.
;To prove that this is true do not use the sub instruction in doSub but use the following method to do the subtraction:
;do a twos complement (neg instruction) on operand 2 then add operand 1 + operand 2 and store the answer in EAX.

doSub proc
	push ebp						;push ebp on the stack
	mov	 ebp, esp					;move esp into ebp
	neg	 $parm2						;perform the two's complement on operand2
	mov	 eax, $parm1					;move operand 1 into eax
	add  eax, $parm2					;eax = operand 1 + (-operand 2)
	pop	 ebp						;pop ebp before return
	ret  8							;remove local variables from stack
doSub endp

;************** doMultShift - unsigned dword multiplication
;
; ENTRY - operand 1 and operand 2 are on the stack
;
; EXIT - EDX:EAX = result (operand 1 * operand 2)
; (for this assignment the product is assumed to fit in EAX and EDX is ignored)
;
; REGS - List registers changed in this function
;
; note: Before calling doMult push operand 2 onto the stack and then push operand 1.
;
; to call doMult in main function:
; push 3 ;32 bit operand2
; push 4 ;32 bit operand1
; call doMult ; 4 * 3 = 12 (answer in eax)
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;--------------
;Take operand1 times operand2 using signed multiplication and the result is returned in EDX:EAX.
;Note: this function does signed multiplication not unsigned. See imul.asm on the class web site.
;Only use the single operand version of imul.
;Please note that this program assumes the product fits in EAX. If part of the produce is in EDX, it is ignored.
;Note you must keep the operands in the correct order: op1*op2 not op2*op1.

doMultShift  proc
	push ebp						;push ebp on the stack
	mov	 ebp, esp					;move esp into ebp
	mov  eax, 0						;clear eax for product result

looptop:
	cmp  $parm2, 0						;loop break condition compare operand2 to 0
	jbe	 done						;if below or equal, then break loop
	shr  $parm2, 1						;shift operand2 one to the right (divide by 2)
	jnc  skip						;if there wasn't a carry, it was even so skip next line
	add  eax, $parm1					;if operand2 is odd, add operand1 to product
skip:
	shl  $parm1, 1						;shift operand1 to the left (multiply by 2)
	jmp  looptop						;repeat loop

done:
	pop	 ebp						;pop ebp before return
	ret  8							;remove local variables from stack
doMultShift	endp

;************** doDiv - unsigned dword / dword division
;
; ENTRY - operand 1(dividend) and operand 2(divisor) are on the stack
;
; EXIT - EAX = quotient
; EDX = remainder
; REGS - List registers changed in this function
;
; note: Before calling doDiv push operand 2(divisor) onto the stack and then push operand 1(dividend).
;
; to call doDiv in main function:
; push 4 ;32 bit operand2 (Divisor)
; push 21 ;32 bit operand1 (Dividend)
; call doDiv ;21 / 4 = 5 R1(5 = quotient in eax, 1 = remainder in edx )
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;--------------
;Take operand1 /operand 2 and the quotient is returned in EAX and the
; remainder is returned in EDX.
;doDiv does unsigned division. 
;Note: after calling doDiv for the modulus operation (%) look at the value that is returned in edx which is the remainder. doDiv does not
;process the modulus operator. 
;
doDiv  proc
	push ebp					   ;push ebp on the stack
	mov	 ebp, esp				   ;move esp into ebp
	mov eax, $parm1				   	   ;move operand 1 into eax
	mov edx, 0					   ;clear edx 
	div $parm2					   ;operand 1 (edx:eax) / operand 2  quotient: eax	  remainder: edx
	pop	 ebp					   ;pop ebp before return
	ret  8						   ;remove local variables from stack
doDiv  endp

;******************** GetBits  isolate and extract bits from a 32 bit bitmap
;           ENTRY  EAX = bitmap to extract bits from
;                   CH = number of bits to extract
;                   CL = starting bit position of bits to extract
;           EXIT  - AX = extracted bits
;           REGS  - EAX,ECX,FLAGS
   
GetBits proc

    push    ecx                    ;save ecx register
	 
    shl     eax,cl                 ;shift bits position times to isolate to left edge
    mov     cl,32                  ;total bits to work with is 32
    sub     cl,ch                  ;how far to shift right? 32 minus number of bits to extract
    shr     eax,cl                 ;shift bits to extract to right edge - 
                                   ;fills in with zeros on the left isolating the bits we wish to extract
    pop     ecx                    ;restore ecx register
	  
	ret			   ;return function
GetBits endp

;************** Adder  Simulate a full Adder circuit  
;  Adder will simulate a full Adder circuit that will add together 
;  3 input bits and output a sum bit and a carry bit
;
;    Each input and output represents one bit.
;
;  Note: do not access the arrays in main directly in the Adder function. 
;        The data must be passed into this function via the required registers below.
;
;       ENTRY - EAX = input bit A 
;               EBX = input bit B
;               ECX = Cin (carry in bit)
;       EXIT  - EAX = sum bit
;               ECX = carry out bit
;       REGS  -  (list registers you use)
;
;       For the inputs in the input columns you should get the 
;       outputs in the output columns below:
;
;        input                  output
;     eax  ebx   ecx   =      eax     ecx
;      A  + B +  Cin   =      Sum     Cout
;      0  + 0 +   0    =       0        0
;      0  + 0 +   1    =       1        0
;      0  + 1 +   0    =       1        0
;      0  + 1 +   1    =       0        1
;      1  + 0 +   0    =       1        0
;      1  + 0 +   1    =       0        1
;      1  + 1 +   0    =       0        1
;      1  + 1 +   1    =       1        1
;
;   Note: the Adder function does not do any output. 
;         All the output is done in the main function.
;
;Do not change the name of the Adder function.
;
;See additional specifications for the Adder function on the 
;class web site.
;
;You should use AND, OR and XOR to simulate the full adder circuit.
;
;You should save any registers whose values change in this function 
;using push and restore them with pop.
;
;The saving of the registers should
;be done at the top of the function and the restoring should be done at
;the bottom of the function.
;
;Note: do not save any registers that return a value (ecx and eax).
;
;Each line of the Adder function must be commented and you must use the 
;usual indentation and formating like in the main function.
;
;Don't forget the "ret" instruction at the end of the function
;
;Do not delete this comment block. Every function should have 
;a comment block before it describing the function. FA16


Adder proc

;Write code for the "Adder" procedure here. 

push	 edx					;push edx onto stack 
push	 ebx					;push ebx onto stack

mov		 edx, eax			;move inputA into edx
and		 edx, ebx			;and edx and inputB (bottom AND)

xor		 eax, ebx			;xor inputA and inputB (first XOR)
mov		 ebx, eax			;move eax into ebx for second AND

xor		 eax, ecx			;perform final XOR and store in eax (sum)
and		 ecx, ebx			;perform second AND and store in ecx
or		 ecx, edx			;perform final OR and store in ecx (carry out)

pop		ebx				;pop ebx from stack, because it was not stored
pop		edx				;pop edx from stack, because it was not stored

ret						;return function

Adder endp

;************** doAdder - dword addition
;
; ENTRY  operand 1 and operand 2 are on the stack
;
; EXIT - EAX = result (operand 1 + operand 2) (any carry is ignored so the answer must fit in 32 bits)
; REGS - EAX,FLAGS
;
; note: Before calling doAdder push operand 2 onto the stack and then push operand 1.
;
; note: doAdder calls the Adder function from program 4 to add up 2 bits and a carry.
;
; to call doAdder in main function:
; push 20 ;32 bit operand2
; push 3 ;32 bit operand1
; call doAdder ;3 + 20 = 23 (answer in eax)
;
; Note; at the end of this function use ret 8 (instead of just ret) to remove the parameters from the stack.
; Do not use add esp, 8 in the main function.
;-------------

doAdder proc
	push ebp						;push ebp on the stack
	mov	 ebp, esp					;move esp into ebp
	push ebx						;push ebx on the stack
	push ecx						;push ecx on the stack
	push edx						;push edx on the stack
	push edi						;push edi on the stack
	
	mov  ecx, 0						;set ecx to 0 for adder (carry)
	mov	 edi, 0						;set edi to 0 for sum holder
	mov	 edx, 0					    	;set edx to 0 for counter

	;loop that adds two 32 bit #'s
looptop:
	cmp edx, 32						;compare edx to 32 for counter
	je done							;if edx = 32, break loop
	mov	eax, 0						;set eax to 0 to start loop
	mov ebx, 0						;set ebx to 0 to start loop
	shr	$parm1, 1					;shift operand1 right to obtain first number (in carry)
	rcl	eax, 1						;rotate the carry into eax to use as first operand in adder
	shr	$parm2, 1					;shift operand2 right to obtain second number (in carry)
	rcl	ebx, 1						;rotate the carry into ebx to use as second operand in adder
	call Adder						;call adder to add eax and ebx with carry
	shr eax, 1						;shift eax right to put sum of first numbers into carry
	rcr edi, 1						;rotate the carry into edi to hold the sum
	inc edx							;increment edx for counter
	jmp looptop						;repeat the loop

done:
	mov eax, edi						;move edi (full sum) into eax

	pop edi							;pop edi from stack
	pop edx							;pop edx from stack
	pop ecx							;pop ecx from stack
	pop ebx							;pop ebx from stack
	pop ebp							;pop ebp from stack

	ret 8							;remove variables from stack
doAdder endp

END Main
