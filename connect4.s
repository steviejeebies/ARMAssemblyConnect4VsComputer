;
; CS1022 Introduction to Computing II 2018/2019
; Mid-Term Assignment - Connect 4 - SOLUTION
;
; get, put and puts subroutines provided by jones@scss.tcd.ie
;

PINSEL0	EQU	0xE002C000
U0RBR	EQU	0xE000C000
U0THR	EQU	0xE000C000
U0LCR	EQU	0xE000C00C
U0LSR	EQU	0xE000C014


	AREA	globals, DATA, READWRITE
BOARD	DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0
		DCB	0,0,0,0,0,0,0

MEM		DCB 0

	AREA	RESET, CODE, READONLY
	ENTRY

	; initialise SP to top of RAM
	LDR		R13, =0x40010000	; initialse SP

	; initialise the console
	BL	inithw

	;
	; your program goes here
	;
	
restart	LDR		R0, =str_go
		BL		puts
		BL		initB
		BL		disp
L1		BL		flBrd
		CMP		R2, #1
		BEQ		drawEnd
		MOV		R1, #0
		MOV		R0, #'Y'		;yellow goes first
		BL 		turn
		CMP		R1, #2
		BEQ		restart
		MOV		R1, #0
		BL		chck
		CMP		R1, #3
		BEQ		winner
		MOV		R0, #'R'
		MOV		R1, #1
		MOV		R2, R4
		MOV		R3, R5
		BL		chck
		MOV		R0, #'R'
		MOV		R1, R2		;places return value of selected column from chckCmp into R1 for place parameter
		BL		place
		CMP		R1, #1		;if turn wasn't sucessful, then run flBrd subroutine to either find out if the board is full, or find an available space
		BEQ		L2
		BL		flBrd
		CMP		R2, #1
		BEQ		drawEnd
		BL		place
		MOV		R1, #0
		BL		chck
L2		MOV		R4, R2
		MOV		R5, R3
		MOV		R1, #0
		BL		chck

		CMP		R1, #3
		BEQ		winner
		B		L1
		
winner	CMP		R0, #'Y'
		BNE		winner1
		LDR		R0, =win_yellow
		BL		puts
		B		restart
winner1	LDR		R0, =win_red
		BL		puts
		B		restart
drawEnd LDR		R0, =draw
		BL		puts
		B		restart
		
		

stop	B	stop


;
; your subroutines go here
; 

; initB - initialize board. Fills each element of the board with char 'O'.
; no parameters
; no return value

initB	LDR		R0, =MEM			
		LDR 	R1, =BOARD
		LDR 	R2, =0x4F			;R3 = (ASCII) 'O'
initB2	CMP		R1, R0				;if increment>total indexes, end subroutine
		BHS		initB1
		STRB	R2, [R1], #1		;Store 'O' in BOARD address + increment
		B		initB2
initB1	BX		LR

; disp - displays the board in the console
; no parameters
; no return values
		
disp	STMFD 	sp!, {R4, R5, LR}
		MOV		R4, #0				;current row
		MOV		R5, #0				;current column
		LDR		R1, =BOARD			;R4 = first index of board
		LDR		R2, =MEM			;first address of MEM
		LDR		R3, =new_board		;gets " 1 2 3 4 5 6 7 (new line)"
disp1	LDRB	R0, [R3], #1		;stores char from new_board into MEM
		CMP		R0, #0				;ends if null-terminated
		BEQ		disp2
		STRB	R0, [R2], #1		;stores char from new_board into MEM
		B		disp1
disp2	ADD		R0, R4, #0x31		;R0 = current Row in ASCII form
		STRB	R0, [R2], #1		;add this to MEM
		LDR		R0, =0x20			;add space char to MEM
		STRB	R0, [R2], #1
disp3	LDRB	R0, [R1], #1		;load current char from BOARD into R0
		STRB	R0, [R2], #1		;current char -> MEM (incremented by 1 after)
		LDR		R0, =0x20
		STRB	R0, [R2], #1		;' ' -> mem (incremented by 1 after)
		CMP		R5, #6				;if current column <= column length, increment column (else new line, reset column, increment row)
		BHS		disp4
		ADD		R5, #1				;column++
		B		disp3
disp4	LDR		R0, =0x0A			;new line and carriage return
		STRB	R0, [R2], #1		;store new line & carriage return in MEM
		LDR		R0, =0x0D			
		STRB	R0, [R2], #1		
		MOV		R5, #0				;reset column
		ADD		R4, R4, #1			;increment row	
		CMP		R4, #6				;if row>= total rows
		BHS		disp5				;end
		B		disp2		
disp5	MOV		R0, #0				;null terminates MEM string for puts subroutine
		STRB	R0, [R2], #1
 		LDR		R0, =MEM			;moves address of MEM to R0 for puts subroutine
		LDR		R4, =0x5FFFF		;timer to slow down the printing of the board for animation
		MOV		R5, #0
disp6	ADD		R5, #1
		CMP		R5, R4
		BNE		disp6
		BL		puts
		LDMFD 	sp!, {R4, R5, PC}
		
;turn - carries out a given players turn. Checks their input is a valid input (or returns an invalid message in the console), 
;and will use the place subroutine to place the piece in their selected column.
;parameters:
;	R0 - current player (Y or R)
;	R1 - if it is computers go, then this register is the column where the computer wants to place their piece.
;return:
;	R1 - int succesfulTurn. If successfulTurn = 0, turn was not sucessful and this subroutine will ask the player to go again. 
;							If successfulTurn = 1, the piece was placed correctly and will return to main to ask next player to go.
;							If successfulTurn = 2, then the player has requested a restart, this subroutine will end and main will deal with restarting the game.
;  	R2 - last row of placed piece
;	R3 - last row of placed piece

turn	STMFD 	sp!, {R4-R6, LR}
		MOV		R4, R0						;puts current piece (Y or R) in R4
		MOV		R6, R1						;R6 = column where computer wants to place piece
turn3	CMP		R4, #'Y'
		BNE		turn1
		LDR		R0, =choose_yellow
		BL		puts
		BL		get							;column player wants to place piece in is in R0
		CMP		R0, #'q'					;if user wants to restart, return 2 in successfulTurn (will be used in main)
		BNE		turn5						;if player requests a restart, successfulTurn is set to 2 and subroutine ends
		BL 		put 
		MOV		R1, #2						;return 2 for isSuccessfulTurn
		B		turnE
turn5	CMP		R0, #0x31					;if R0 < '1' or R0 > 7, then print "invalid" message
		BLT		turnI
		CMP		R0, #0x37
		BGT		turnI
turn4	SUB		R5, R0, #0x31				;subs 0x31 from ASCII char to get row number in hex form
		BL		put							;prints the number the player entered back into the console
		MOV		R0, R4						;places current piece in R0 for "place" subroutine
		MOV		R1, R5						;places column selected in R0 for "place" subroutine
		BL		place
		CMP		R1, #1						;"place" returns binary successfulTurn, if true then continues, else asks the player for another attempt
		BEQ		turnE						;if turn was not successful, then invalid message is printed and branches to turn3 to ask player to go again
turnI	LDR		R0, =invalid
		BL		puts
		B		turn3
turn1	MOV		R0, R4
		MOV		R1, R6
		BL		place
turnE	LDMFD 	sp!, {R4-R6, PC}	

;place - places the piece in BOARD. Places the piece in the top row of the selected column, then prints the board, then does the same for each
; 									row until the piece is at the lowest possible spot (either bottom of the board or on top of another piece).
;									This is done so the board is animated.
;parameters:
;	R0 - current piece (Y or R)
;	R1 - column selected
;return:
;	R0 - player's piece (Y or R)
;	R1 - sucessfulTurn
;	R2 - final row value of piece
;	R3 - final column value of piece

place	STMFD 	sp!, {R4-R7, LR}
		LDR 	R2, =BOARD		;first index of board
		MOV		R4, R1			;puts column selected in R4
		MOV		R5, #0			;starts with top row
		MOV		R3, #7			;R3 = row size
		MUL		R6, R5, R3		;row*rowsize
		ADD		R6, R6, R4		;index=(row*rowsize)+column
		ADD		R2, R2, R6		;index of piece is BOARD+index
		LDRB	R3, [R2]		;puts piece (Y, R or O) from BOARD index into R3
		CMP		R3, #'O'		;if index of BOARD is empty, stores (Y or R) in R3
		BNE		placeF			;if piece of top row is not 'O', then return false for successful turn
placeN	STRB	R0, [R2]		;stores piece into BOARD index
		ADD		R5, R5, #1		;row++
		MOV		R7, R2			;R7 is the adress of current piece
		PUSH	{R0}			;pushes current piece (Y or R) onto stack to protect it
		BL		disp			;displays the board on console
		LDMFD 	sp!, {R0}			;pops current piece (Y or R) from stack
								;in order to check that the next piece below this is also empty, we do the following:
		CMP		R5, #5			;if piece has reached the lowest possible row(row 6), end subroutine
		BHI		placeF
		LDR 	R2, =BOARD		;first index of board
		MOV		R3, #7			;R3 = row size
		MUL		R6, R5, R3		;row*rowsize
		ADD		R6, R6, R4		;index=(row*rowsize)+column
		ADD		R2, R2, R6		;index of piece is BOARD+index
		LDRB	R3, [R2]		;puts piece (Y, R or O) from BOARD index into R3
		CMP		R3, #'O'		;if index of BOARD is empty, stores (Y or R) in R3
		BNE		placeF			;else the piece is currently in the lowest possible valid row
		MOV		R3, #'O'		;gets empty piece
		STRB	R3, [R7]		;stores 'O' where piece was, to create a falling effect (will not print the board this time)
		B		placeN			;branch to placeN
placeF	CMP		R5, #0			;if row has not been incremented at all, then placing a piece has failed
		BHI		placeF1
		MOV		R1, #0			;successfulTurn set to 0
		B		placeF2
placeF1	SUB		R5, R5, #1		;have to decrease row by 1 to find row of current piece
		MOV		R2, R5			;moves row to R2 for return value from subroutine
		MOV		R3,	R4			;moves column to R3 for return value from subroutine
		MOV		R1, #1			;successfulTurn set to 1
placeF2	LDMFD 	sp!, {R4-R7, PC}



;check - checks if the last piece placed was a winning move
;parameters:
;	R0 - player's piece (Y or R)
;	R1 - computersTurn (binary)
;	R2 - row of last piece placed
;	R3 - column of last piece placed
;return:
;	R1 - winning move
;	R2 - suggeted column

chck	STMFD 	sp!, {R4-R11, LR}
		MOV		R8, R1				;if R8 is set, then the computer is looking for a valid move
		MOV		R9, #0				;(computer) highest streak possible on current board
		MOV		R1, #0				;winning streak initialized as 0
		
;check the line consisting of the top-left slot and the bottom right slot

		MOV		R4, R2				;copy position of piece (row) to R4
		MOV		R5, R3				;copy position of piece (column) to R5
		MOV		R6, #-1				;xaxis: -1
		MOV		R7, #-1				;yaxis: -1
		MOV		R11, #0				;reset streak for line
chckL1	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm				;chckCm branch checks the spot being checked is valid, and if there is a winning streak of pieces
		CMP		R1, #0				;if curChk returns out of bounds, or if slot being checked does not match current player, then check next line
		BGT		chckL1
		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #1				;xaxis:+1
		MOV		R7, #1				;yaxis:+1
chckL2	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm	
		CMP		R1, #0				;if next adjacent piece is a matching piece, find next adjacent piece
		BGT		chckL2				;else examine next line
		CMP		R11, #3				;if there are 3 of the same color piece beside the last piece dropped, then player has won, skip to chckE
		BEQ		chckE
		MOV		R11, #0
		
;check the line consisting of the left-middle slot and the right middle slot

		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #0				;xaxis: +0
		MOV		R7, #-1				;yaxis: -1
		MOV		R11, #0				;reset streak for line
chckL3	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm				;chckCm branch checks the spot being checked is valid, and if there is a winning streak of pieces
		CMP		R1, #0				;if curChk returns out of bounds, or if slot being checked does not match current player, then check next line
		BGT		chckL3
		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #0				;xaxis:+1
		MOV		R7, #1				;yaxis:+1
chckL4	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm	
		CMP		R1, #0				;if next adjacent piece is a matching piece, find next adjacent piece
		BGT		chckL4				;else examine next line
		CMP		R11, #3				;if there are 3 of the same color piece beside the last piece dropped, then player has won, skip to chckE
		BEQ		chckE
		MOV		R11, #0

;check the line consisting of the bottom-left slot and the top-left slot

		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #-1				;xaxis: -1
		MOV		R7, #1				;yaxis: -1
		MOV		R11, #0				;reset streak for line
chckL5	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm				;chckCm branch checks the spot being checked is valid, and if there is a winning streak of pieces
		CMP		R1, #0				;if curChk returns out of bounds, or if slot being checked does not match current player, then check next line
		BGT		chckL5
		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #1				;xaxis:+1
		MOV		R7, #-1				;yaxis:-1
chckL6	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm	
		CMP		R1, #0				;if next adjacent piece is a matching piece, find next adjacent piece
		BGT		chckL6				;else examine next line
		CMP		R11, #3				;if there are 3 of the same color piece beside the last piece dropped, then player has won, skip to chckE
		BEQ		chckE
		MOV		R11, #0

;check the line consisting of the top-middle slot and bottom-middle slot

		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #1				;xaxis: 1
		MOV		R7, #0				;yaxis: 0
		MOV		R11, #0				;reset streak for line
chckL7	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm				;curChk leaf subroutine checks the spot being checked is valid, and if there is a winning streak of pieces
		CMP		R1, #0				;if curChk returns out of bounds, or if slot being checked does not match current player, then check next Pos (Pos 2)
		BGT		chckL7
		MOV		R2, R4				;reset to base row
		MOV		R3, R5				;reset to base column
		MOV		R6, #-1				;xaxis:-1
		MOV		R7, #0				;yaxis:0
chckL8	ADD		R2, R2, R6			;temp row = row+yaxis
		ADD		R3, R3, R7			;temp column = column + xaxis
		BL		chckCm				
		CMP		R1, #0				;if curChk returns out of bounds, or if slot being checked does not match current player, then check next Pos (Pos 2)
		BGT		chckL8
		B		chckE				;else end the subroutine

chckCm	STMFD   sp!, {R14}			
		CMP		R8, #1			;check the binary isComputerTurn; if this is set, then we need to find an eligible column
		BNE		chckCm1
		CMP		R11, R9			;compare current winning streak with highest possible winning streak
		BLT		chckCm1			;if current winning streak is lower than max, then we don't need to find a column here, since it doesn't benefit the computer
		STMFD   sp!, {R3}		;puts the column on stack
		BL		curChk			;tests the column
		CMP		R1, #-1			;if the next piece is vacant, i.e. 'O', then pop the column back into R10, which will be returned at the end of the chck subroutine
		BNE		chckCm2
		LDMFD 	sp!, {R10}
		LDMFD 	sp!, {R15}		;if current winning streak is greater than max winning streak, then max winning streak is now current winning streak
		B		chckCmE
chckCm2	LDMFD 	sp!, {R3}		;addresses currently on stack are useless to us, so we can get rid of them in R2, R3
		B		chckCmE
chckCm1	BL		curChk			;branch to curChk to test adjacent piece
		CMP		R1, #1			;if piece matches, return to chck loop for this xaxis/yaxis line...
		BNE		chckCmE
		ADD		R11, R11, #1	;...and increment the streak by 1
chckCmE	LDMFD 	sp!, {PC}

chckE	MOV		R1, R11			;places the highest possible stream in R1, and returns to main
		MOV		R2, R10			;place suggested column in R2 for the return values
		LDMFD 	sp!, {R4-R11, PC}




;curChck - used for chck subroutine, checks the given x-axis/y-axis of the column and sees if it matches the current player piece (Y or R)
;			also makes sure that we are not going outside the boundary of the BOARD
;		Note: LEAF subroutine, but pushes some registers to stack for protection
;parameters:
;		R0 - current player (Y or R)
;		R1 - running total of same pieces in a row
;		R2 - x-axis (or row of index currently being checked)
;		R3 - y-axis (or column of index currently being checked)
;return:
;		R0 - current player (Y or R) [UNCHANGED]
;		R1 - (winning streak) running total of same pieces in a row (incremented if next piece is the same, or 0 if out-of-bounds/next piece 
;				is not the same as current player piece)
;		R2 - x-axis (or row of index currently being checked) [UNCHANGED]
;		R3 - y-axis (or column of index currently being checked) [UNCHANGED]

curChk	STMFD 	sp!, {R4-R6}
		MOV		R4, #7    		;row size
		CMP		R2, #0			;if row being checked is <0 or >5, then out-of-bounds, end subroutine with failure
		BLT		curChkF
		CMP		R2, #5
		BGT		curChkF
		CMP		R3, #0			;if column being checked is <0 or >6, then out-of-bounds, end subroutine with failure
		BLT		curChkF
		CMP		R3, #6
		BGT		curChkF
		LDR		R5, =BOARD
		MUL		R4, R2, R4		;(row*rowsize)
		ADD		R4, R4, R3		;(row*rowsize)+column
		ADD		R5, R5, R4		;checked piece = BOARD+index
		LDRB	R4, [R5]		;load from BOARD index into R4
		CMP		R4, R0			;if adjacent piece is equal to current players piece, then increment winning streak by 1
		BNE		curChkF			;else failure
		MOV		R1, #1			;increment winning streak
		B		curChkE			;branch to end
curChkF	CMP		R4, #'O'
		BNE		curChkF1
		MOV		R1, #-1
		B		curChkE
curChkF1 MOV	R1, #0			;winning streak = 0 (if out of bounds or if next piece does not match)
curChkE	LDMFD 	sp!, {R4-R6}
		BX		LR
		
		
		
		
;flBrd - LEAF subroutine, checks that the board is full, and if not, returns the lowest possible valid space (for the computer player)
;parameters:
;		none
;return:
;		R1 - available space (column)
;		R2 - binary, isBoardFull
flBrd	STMFD 	sp!, {R4, R5}
		MOV		R1, #0			;column
		MOV		R2, #5			;row
		MOV		R3, #7			;rowSize		
flBrd1	MUL		R4, R2, R3
		ADD		R4, R4, R1
		LDR		R5, =BOARD
		ADD		R5, R5, R4
		LDRB	R4, [R5]
		CMP		R4, #'O'
		BEQ		flBrdS			;there are spaces left on the board
		ADD		R1, R1, #1
		CMP		R1, #6
		BLS		flBrd1
		MOV		R1, #0
		SUB 	R2, R2, #1
		CMP		R2, #0
		BLT		flBrdF			;board is completely full
		B		flBrd1
flBrdS	MOV		R2, #0			;isBoardFull = false
		LDMFD 	sp!, {R4, R5}
		BX		LR
flBrdF	MOV		R2, #1			;isBoardFull = true
		LDMFD 	sp!, {R4, R5}
		BX		LR
		
		

;
; inithw subroutines
; performs hardware initialisation, including console
; parameters:
;	none
; return value:
;	none
;
inithw
		LDR		R0, =PINSEL0		; enable UART0 TxD and RxD signals
		MOV		R1, #0x50
		STRB	R1, [R0]
		LDR		R0, =U0LCR		; 7 data bits + parity
		LDR		R1, =0x02
		STRB	R1, [R0]
		BX		LR

;
; get subroutine
; returns the ASCII code of the next character read on the console
; parameters:
;	none
; return value:
;	R0 - ASCII code of the character read on the console (byte)
;
get		LDR		R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
get0	LDR		R0, [R1]		; wait until
		ANDS	R0, #0x01		; receiver data
		BEQ		get0			; ready
		LDR		R1, =U0RBR		; R1 -> U0RBR (Receiver Buffer Register)
		LDRB	R0, [R1]		; get received data
		BX		LR			; return

;
; put subroutine
; writes a character to the console
; parameters:
;	R0 - ASCII code of the character to write
; return value:
;	none
;
put		LDR		R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
		LDRB	R1, [R1]		; wait until transmit
		ANDS	R1, R1, #0x20		; holding register
		BEQ		put			; empty
		LDR		R1, =U0THR		; R1 -> U0THR
		STRB	R0, [R1]		; output charcter
put0	LDR		R1, =U0LSR		; R1 -> U0LSR
		LDRB	R1, [R1]		; wait until
		ANDS	R1, R1, #0x40		; transmitter
		BEQ		put0			; empty (data flushed)
		BX		LR			; return

;
; puts subroutine
; writes the sequence of characters in a NULL-terminated string to the console
; parameters:
;	R0 - address of NULL-terminated ASCII string
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
puts	STMFD	SP!, {R4, LR} 		; push R4 and LR
		MOV		R4, R0			; copy R0
puts0	LDRB	R0, [R4], #1		; get character + increment R4
		CMP		R0, #0			; 0?
		BEQ		puts1			; return
		BL		put			; put character
		B		puts0			; next character
puts1	LDMFD	SP!, {R4, PC} 		; pop R4 and PC


;
; hint! put the strings used by your program here ...
;

str_go
	DCB	0xA, 0xD, "Let's play Connect 4!",0xA, 0xD, 0x0

str_new
	DCB	0xA, 0xD, 0x0
	
new_board
	DCB 0xA, 0xD, ' ', ' ', '1', ' ', '2', ' ', '3', ' ', '4', ' ', '5', ' ', '6', ' ', '7', 0xA, 0xD, 0x0
	
choose_yellow
	DCB "YELLOW: Choose a column for your next move (1-7, 'q' to restart): ", 0x0
		
choose_red
	DCB "RED: Choose a column for your next move (1-7, 'q' to restart): ", 0x0
	
invalid
	DCB 0xA, 0xD, "Invalid move, try again.", 0xA, 0xD, 0x0
	
win_yellow
	DCB	0xA, 0xD, "YELLOW wins!", 0xA, 0xD, 0x0

win_red
	DCB	0xA, 0xD, "RED wins!", 0xA, 0xD, 0x0

draw
	DCB	0xa, 0xD, "It's a draw!", 0xA, 0xD, 0x0
	
	END
	
