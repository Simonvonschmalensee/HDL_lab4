Library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_signed.ALL;

Library WORK;
use WORK.ALL;
use WORK.package_microassemblycode.ALL;

ENTITY dataPath IS
	GENERIC (
		Size  : INTEGER := 8; -- # bits in word
		ASize : INTEGER := 3  -- # bits in address
	);
	PORT (
		InPort      : IN  STD_LOGIC_VECTOR(Size-1 DOWNTO 0);
		OutPort     : OUT STD_LOGIC_VECTOR(Size-1 DOWNTO 0);
		Clk         : IN  STD_LOGIC;
		Instr       : IN Instruction_Type    
	);
END dataPath;

architecture behavioral of dataPath is
	component Selector
		generic(width :integer:=4);
		port(
			a	:in  std_logic_vector(width-1 downto 0);
			b	:in  std_logic_vector(width-1 downto 0);
			s	:in	 std_logic;
			q	:out std_logic_vector(width-1 downto 0));   
	end component;
	
	component RegFile
		generic (width :integer:=4);
		port(
			data_in		:in	 std_logic_vector(width-1 downto 0);
			WR			:in  std_logic_vector(3 downto 0);
			RA			:in  std_logic_vector(3 downto 0);
			RB 	    	:in  std_logic_vector(3 downto 0);
			clk			:in  std_logic;
			reset_n		:in  std_logic;	
			data_outA   :out std_logic_vector(width-1 downto 0);
			data_outB	:out std_logic_vector(width-1 downto 0));
	end component;
	
	component ALU
		generic (width:integer:=4);
		port(
			a		:in  std_logic_vector(width-1 downto 0);
			b		:in  std_logic_vector(width-1 downto 0);
			aluop	:in  std_logic_vector(2 downto 0);
			aluout	:out std_logic_vector(width-1 downto 0));
	end component;
	
	component Shifter
		generic(width:integer:=4);
		port(
			shift_in	:in  std_logic_vector(width-1 downto 0);
			shift_op	:in  std_logic_vector(2 downto 0);
			shift_out	:out std_logic_vector(width-1 downto 0));
	end component;
	
	signal result		:std_logic_vector(Size-1 downto 0);
	signal selector_out :std_logic_vector(Size-1 downto 0);
	signal reg_outA		:std_logic_vector(Size-1 downto 0);
	signal reg_outB		:std_logic_vector(Size-1 downto 0);
	signal aluout		:std_logic_vector(Size-1 downto 0);
begin
	selector1: Selector 
			   generic map(Size) 
			   port    map(
							a => result,
							b => InPort,
							s => Instr.IE,
							q => selector_out);
			   
	regFile1 : RegFile
			   generic map(Size)
			   port    map(selector_out,Instr.Dest(ASize downto 0),Instr.Src1(ASize downto 0),
			   			   Instr.Src2(ASize downto 0),clk,'1',
			   			   reg_outA,reg_outB);
			   			   
	alu1	 : ALU
			   generic map(Size)
			   port	   map(reg_outA,reg_outB,Instr.Alu,aluout);
			   
	shifter1 : Shifter
			   generic map(Size)
			   port    map(aluout,Instr.Shift,result);
			   
	OutPort <= result when Instr.OE = '1' else (others => 'Z');
	
end behavioral;
