// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)


// Put your code here.

// check if exactly on of R0, R1 is negative, write 1 to negate and transform both to positive

// negate = 0
@negate
M=0

// if (R0 < 0) { negate = !negate; R0 = -R0; }
@R0
D=M
@a
D;JGE
@negate
M=!M
@R0
M=-M
(a)

// if (R1 < 0) { negate = !negate; R1 = -R1; }
@R1
D=M
@b
D;JGE
@negate
M=!M
@R1
M=-M
(b)

// multiply

// R2=0
@R2
M=0
(j)
// if R1 > 0
@R1
D=M
@finalize
D;JEQ
// R2 += R0
@R0
D=M
@R2
M=M+D
// R1--
@R1
M=M-1
// goto j
@j
0;JMP
(finalize)

// if negate, negate the output
@negate
D=M
@end
D;JEQ 
@R2
M=-M

(end)
@end
0;JMP
	
