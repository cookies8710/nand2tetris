// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

//@21845 // 0x5555, 0101010101010101
// main loop - if no key, fill with white; otherwise fill with black
(main)
@KBD
D=M
@R1
M=0
//M=-1
@nokey
D;JEQ 
@R1
//M=0
M=-1
(nokey)
@main
D=A
@R15
M=D
@fill
0;JMP

(fill) // fills with R1, goes back to R15
	// R0 = 0
	@0
	D=A
	@R0
	M=D 
	(draw)
		// SCREEN[R0] = R1:
		@R0
		D=M
		@SCREEN
		D=A+D
		@R2
		M=D
		// R2 = SCREEN + R0

		@R1
		D=M
		@R2
		A=M
		M=D

		// R0++
		@R0
		M=M+1
		// if R0 < 8192 goto draw // 512 * 256 / 16 = 2**(9+8-4) = 8192
		@8191
		D=A
	@R0
	D=M-D
	@draw
	D;JLE
	// ret R15
	@R15
	A=M
	0;JMP

// end (loop)
(endless)
@endless
0;JMP

(fillline) // fills line R0 with R1, goes back to R15
        // base of the line = SCREEN + 32 * y (each line is 512 bits = 16 * 32)
	@R0
	D=M
	@line
	M=D

	@R1
	D=M
	@pattern
	M=D

	@R15
	D=M
	@flret
	M=D

	@32
	D=A
	@R0
	M=D

	@line
	D=M
	@R1
	M=D

	@filllineafter
	D=A
	@R15
	M=D
	@mul
	0;JMP
	(filllineafter)

	@base
	M=D
	@pattern
	D=M
	@base
	A=M
	M=D

	A=A+1
	M=D




	// ret
	@flret
	A=M
	0;JMP

// mul test
(here)
@here
D=A
@R4
M=D

@7
D=A
@R0
M=D
@4
D=A
@R1
M=D
@mul
0;JMP

(mul) // D <- R0 * R1, return to R15
// R3=0
@R3
M=0
(j)
// if R1 > 0
@R1
D=M
@me
D;JEQ
// R3 += R0
@R0
D=M
@R3
M=M+D
// R1--
@R1
M=M-1
// goto j
@j
0;JMP
(me)
@R3
D=M
// jump back
@R15
A=M
0;JMP
