USE WORK.ALL;
LIBRARY IEEE;
USE IEEE.std_logic_1164.all;

PACKAGE package_MicroAssemblyCode IS
	CONSTANT Size : INTEGER := 8;
	CONSTANT ASize : INTEGER := 3;
	SUBTYPE OpCode_Type IS STD_LOGIC_VECTOR(2 DOWNTO 0);
	SUBTYPE Regs_Type IS STD_LOGIC_VECTOR(ASize DOWNTO 0);
	CONSTANT OpX : OpCode_Type :=   "001"; -- "Empty" operator
	CONSTANT OpInv : OpCode_Type := "000"; -- Complement A
	CONSTANT OpAnd : OpCode_Type := "001"; -- A and B
	CONSTANT OpXor : OpCode_Type := "010"; -- A xor B
	CONSTANT OpOr : OpCode_Type := "011"; -- A or B
	CONSTANT OpDec : OpCode_Type := "100"; -- Decrement A
	CONSTANT OpAdd : OpCode_Type := "101"; -- A add B
	CONSTANT OpSub : OpCode_Type := "110"; -- A sub B
	CONSTANT OpInc : OpCode_Type := "111"; -- Increment A
	-- shift instructions
	CONSTANT OpPass : OpCode_Type := "000";
	CONSTANT OpShiftL : OpCode_Type := "100";
	CONSTANT OpShiftR : OpCode_Type := "110";
	CONSTANT OpRotL : OpCode_Type := "101";
	CONSTANT OpRotR : OpCode_Type := "111";
	-- register macros
	CONSTANT Rx : Regs_Type := "0000";
	CONSTANT R0 : Regs_Type := "0001";
	CONSTANT R1 : Regs_Type := "0011";
	CONSTANT R2 : Regs_Type := "0101";
	CONSTANT R3 : Regs_Type := "0111";
	CONSTANT R4 : Regs_Type := "1001";
	CONSTANT R5 : Regs_Type := "1011";
	CONSTANT R6 : Regs_Type := "1101";
	CONSTANT R7 : Regs_Type := "1111";
	TYPE Instruction_Type IS RECORD
		IE : STD_LOGIC;
		Dest : Regs_Type;
		Src1 : Regs_Type;
		Src2 : Regs_Type;
		Alu : OpCode_Type;
		Shift : OpCode_Type;
		OE : STD_LOGIC;
	END RECORD;
	TYPE Program_Type IS ARRAY(NATURAL RANGE <>) OF Instruction_Type;
	TYPE RGBData_Type IS ARRAY(NATURAL RANGE <>) OF STD_LOGIC_VECTOR(Size*3-1 DOWNTO 0);
END package_MicroAssemblyCode;

