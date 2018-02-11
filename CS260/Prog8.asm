; *************************************************************
; Student Name: Roderic Deichler
; COMSC-260 Fall 2016
; Date:12/05/2016
; Assignment # 8
; Version of Visual Studio used:2015
; Did program compile? Yes
; Did program produce correct results? Yes
;
; Estimate of time in hours to complete assignment: 3
;
; Short description of what program does:  This program is a redo of program 2 but using 64 bit programming
; instead of 32 bit programming.
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
;
;*************************PROTOTYPES***************************

    ExitProcess PROTO					;Exit code in rcx
	
	DspChar PROTO					;Display the character in rcx to the console
	
	DspHex PROTO					;Display the unsigned decimal number in rcx to the console
	
	DspStr PROTO					;Display a zero terminated string to the console. Str address in rcx.

    MessageBoxA PROTO					;MessageBoxA takes 4 parameters: 
									;  1. window owner handle (rcx)
									;  2. message address (zero terminated string) (rdx)
									;  3. title address(zero terminated string) (r8)   
									;  4. which button(s) to display (r9)

;*******************MACROS*************************************

;*************mPrtStr macro***************
;usage: mPrtStr nameOfString
;ie to display a 0 terminated string named message say:
;mPrtStr message

;Macro definition of mPrtStr. Wherever mPrtStr appears in the code
;it will  be replaced with 

mPrtStr  MACRO  arg1				;arg1 is replaced by the name of string to be displayed
		 push rcx					;save rcx
         mov  rcx, offset arg1		;address of str to display should be in rcx
         call DspStr				;display 0 terminated string
         pop  rcx					;restore rcx
ENDM

;Wherever "mPrtStr message" appears in the code it will  be replaced with 
;push edx
;mov edx, offset arg1   
;call WriteString       
;pop edx
;arg1 is replaced with message if that is the name of the string passed in.


;*************mPrtChar macro****************
;mPrtChar - used to print single characters
;usage: mPrtChar character
;ie to display a 'm' say:
;mPrtChar 'm'


mPrtChar MACRO  arg1				;arg1 is replaced by the name of character to be displayed
         push rcx				;save rcx
         mov cl, arg1				;character to display should be in cl
         call DspChar				;display character
         pop rcx				;restore rcx
ENDM


;*************mPrtHex macro****************
;mPrtHex - used to print hex characters
;usage: mPrtHex character
;ie to display a 'FFF' say:
;mPrtHex 'FFF'


mPrtHex  MACRO  arg1				;arg1 is replaced by the hex number to be displayed
         push rcx				;save rcx
         mov rcx, arg1				;hex number to display should be in rcx
         call DspHex				;display hex number
         pop rcx				;restore eax
ENDM


;************************DATA SEGMENT***************************

.data
   
    num1    qword   0D967361CB7FB71h				       			   ;num1 = 0CB7FB71h (32bit)
    num2    qword   23FDD2456h							   	   ;num2 = 0FDD2456h (32bit)
	num3	qword	0F234C57h							   ;num3 = 0C57h	  (32bit)
	num4	qword	1B67A45A9h							   ;num4 = 0A45A9h	  (32bit)
	num5	qword	1D23B492FDE2B46Bh					   	   ;num5 = 0FDE2B46Bh(32bit)
	num6	qword	6567D3494h							   ;num6 = 0D3494h	  (32bit)
	num7	qword	5BC4ABC12h							   ;num7 = 4ABCh	  (32bit)

	openingMSG	byte	"Program 8 by Roderic Deichler",0  ;Program header
	exitingMSG	byte	'"',"Sometimes it is the people no one can imagine anything of who do the things no one can imagine",'"'," Alan Turing", 0				;Program ending
														   ;MessageBox message^^
	hMin	byte	"h-",0								   ;String for h minus
	hPlus	byte	"h+",0								   ;String for h plus
	hTimes	byte	"h*",0								   ;String for h times
	hDiv	byte	"h/(",0								   ;String for h division and parenthesis
	hMod	byte	"h%",0								   ;String for h modulo
	hEqu	byte	"h)=",0								   ;String for h, parenthesis, & equals


;************************CODE SEGMENT****************************

.code

main PROC

	sub   rsp,28h			;32 bytes for shadow space plus 8 bytes to
					;align stack on 16 byte boundary
					;4 parameters + return address
					;only 8 bytes for ret add so need another 8
	
	mPrtHex	num1			;print num1 in hex		
	mPrtStr hPlus			;print h plus for formatting
	
	mPrtHex num2			;print num2 in hex
	mPrtStr hTimes			;print h * for formatting

	mPrtHex num3			;print num3 in hex
	mPrtStr hMod			;print h % for formatting

	mPrtHex num4			;print num4 in hex
	mPrtStr hMin			;print h - for formatting

	mPrtHex num5			;print num5 in hex
	mPrtStr hDiv			;print h / for formatting

	mPrtHex num6			;print num6 in hex
	mPrtStr hMin			;print h - for formatting

	mPrtHex num7			;print num7 in hex
	mPrtStr hEqu			;print h equals for formatting

	;num6 - num7
	mov		rbx, num6		;rbx = num6
    sub     rbx, num7	    ;rbx = num6 - num7

	;num 2 * num 3 % num4
    mov     rax, num2	    ;rax = num2
    mul		num3				;rax * num3
	div		num4			;rdx:rax / num4 -> remainder in rdx
	
	;moving remainder to an unused register and preparing rax/rdx for division
	;num5/ (rbx)    rbx = num6 - num7
	mov		rcx, rdx		;rcx = rdx
	mov		rax, num5		;rax = num5
	mov		rdx, 0			;rdx = 0000000000000000h
	div		rbx				;rdx:rax / rbx  -> quotient in rax
	mov		rdx, rax		;rdx = rax (quotient above)

	;adding num1 to result from step3 (ecx)
	mov		rbx, num1		;rbx = num1
	add		rbx, rcx		;rbx = num1 + rcx  -> rcx = num2 * num3 % num4
	sub		rbx, rdx		;rbx = rbx (num1 + num2 * num3 % num4) - rdx (num5 / (num6 - num7))

	mPrtHex rbx				;print out result
	mPrtChar 'h'				;print out h for formatting

	mov   rcx, 0				;A handle to the owner window of the message box to be created. 
							;If this parameter is NULL, the message box has no owner window.
	lea   rdx, exitingMSG   		;The message to be displayed in message box
	lea   r8,  openingMSG   		;The message box title
	mov   r9d, 0            		;0 = display ok button
	Call  MessageBoxA       		;call MessageBox API function
	mov   rcx, 0				;exit code for ExitProcess
	call  ExitProcess       		;exit program

main ENDP
End

