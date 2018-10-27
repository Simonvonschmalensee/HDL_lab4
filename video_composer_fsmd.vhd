LIBRARY WORK;
USE WORK.ALL;
USE WORK.package_MicroAssemblyCode.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_signed.all;
USE IEEE.std_logic_arith.all;

ENTITY VideoComposer_FSMD IS
	PORT (
		Clk          : IN  STD_LOGIC;
		Reset        : IN  STD_LOGIC;

		Start		 : IN STD_LOGIC;
		Ready		 : OUT STD_LOGIC;

		ROM_address : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
		DataIn	: IN STD_LOGIC_VECTOR(7 DOWNTO 0);

		RAM_address : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
		RAM_WE	: OUT STD_LOGIC;
		DataOut      : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);
END VideoComposer_fsmd;

ARCHITECTURE behaviour OF videoComposer_FSMD IS

	CONSTANT ROM : Program_Type := (
		--| IE  | Dest | Src1 | Src2 | OpAlu | OpShift | OE  |
		  ('0',	R0, R0, R0,  OpXor,  OpPass, 	'0'), -- Reset_State (This instruction does not set the value of r0 to 0)
		  ('1',	R1,	Rx,	Rx,	 OpXor,  OpPass,	'0'), -- S_Read_Red
		  ('1',	R2,	R1,	R1,	 OpAnd,  OpPass,	'1'), -- S_ReadGreen_WriteRed
		  ('1',	R3,	R2,	R2,	 OpAnd,  OpPass,	'1'), -- S_ReadBlue_WriteGreen
      -----------------------------------------------------------
      	  ('0',	R7,	R1,	R1,	 OpXor,  OpPass,	'0'),   -- Setting register 7 to 0.
     	  ('0',	R4,	R7,	R7,	 OpInc,  OpRotR,	'0'),   -- Creating a bitmask R4 = 1000 0000 
      	  ('0',	R5,	R3,	R4,	 OpAnd,  OpRotL,	'0'),   -- Check for overflow R5 = 0000 0001(If overflow) or 0000 0000(Not overflow) 
      	  ('0',	R3,	R3,	R3,	 OpAnd,  OpShiftL,	'0'),   -- Multiply blue by 2. R3 = R3 << 1 
      	  ('0',	R6,	R3,	R4,	 OpAnd,  OpRotL,	'0'),   -- Check for overflow  R6 = 0000 0001(If overflow) or 0000 0000(Not overflow)
      	  ('0',	R6,	R5,	R6,	 OpOr,   OpPass,	'0'),   -- R6 = 0000 0001  or 0000 0000 
      	  ('0',	R5,	R6,	R6,	 OpDec,  OpPass,	'0'),   -- R5 = 0000 0000 or 1111 1111
      	  ('0',	R6,	R5,	R5,	 OpInv,  OpPass,	'0'),   -- R6 = 1111 1111 or 0000 0000
      	  ('0',	R3,	R3,	R3,	 OpAnd,  OpShiftL,	'0'), -- R3 = R3 << 2
      	  ('0',	R3,	R3,	R6,	 OpOr,   OpPass,	'0'),   -- R3 = R3 << 2 or 1111 1111 / 0000 0000
      -----------------------------------------------------------
		  ('0',	Rx,	R3,	R3,	 OpAnd,  OpPass,	'1'),   -- S_WriteBlue
		  ('0',	Rx,	Rx,	Rx,	 OpAnd,  OpPass,	'0')    -- S _Idle
		);

	COMPONENT dataPath
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
	END COMPONENT;

	-- Datapath signals
	SIGNAL InPort      : STD_LOGIC_VECTOR(Size-1 DOWNTO 0);
	SIGNAL OutPort     : STD_LOGIC_VECTOR(Size-1 DOWNTO 0);
	SIGNAL instr : Instruction_type := ( '0' , Rx   , Rx   , Rx   , OpX   , OpX     , '0' );

	TYPE   State_Type IS (reset_state,S_ReadRed, S_ReadGreenWriteRed, S_ReadBlueWriteGreen, S_ProcessBlue, S_WriteBlue, S_Idle);

	SIGNAL current_state,   next_state   : State_Type;
	-- Instr counter for the datapath
	SIGNAL current_counter, next_counter : INTEGER RANGE 0 to ROM'High:= 0;
	SIGNAL read_address,next_read_address,write_address,next_write_address: STD_LOGIC_VECTOR(15 DOWNTO 0);

	SIGNAL next_WE,WE:STD_LOGIC:='0';

BEGIN

	instr   <= ROM(current_counter); -- Moore Decoding of instr...

	COMB: PROCESS(current_state, current_counter, read_address, write_address, InPort,OutPort,DataIn)
	BEGIN
		InPort <= DataIn;
		next_state   <= current_state;
		next_counter <= current_counter;
		Ready <= '0';
		next_read_address<=read_address;
		next_write_address<=write_address;
		next_WE<='0';
		CASE current_state IS
			WHEN reset_state => -- ROM Instr 0
				next_read_address<=(others=>'0');
				next_write_address<=(others=>'0');
				next_WE<='0';
				next_state<=S_ReadRed;
				next_counter   <= 1;
			WHEN S_ReadRed  => -- ROM Instr 1
				next_state<=S_ReadGreenWriteRed;
				next_counter   <= 2;
				next_read_address<=read_address+1;
				next_WE<='1'; -- Write during next state...
			WHEN S_ReadGreenWriteRed => -- ROM Instr 2
				next_counter <= 3;
				next_state   <= S_ReadBlueWriteGreen;
				next_read_address<=read_address+1;
				next_write_address<=write_address+1;
				next_WE<='1'; 
			WHEN S_ReadBlueWriteGreen => -- ROM Instr 3
				next_WE<='0'; --<='0'; if you add states for processing the blue color
				next_counter <= 4;
				next_state   <= S_ProcessBlue; --S_ProcessBlue
				next_read_address<=read_address+1;
				next_write_address<=write_address+1;
			-- ...
			-- states to manage blue color...
			-- ...
      WHEN S_ProcessBlue => -- ROM Instr 4?
				if (current_counter < 13) then
				  report "Inside Process Blue";
				  next_state   <= S_ProcessBlue;
					next_counter <= current_counter + 1;
					
				else
					next_state <= S_WriteBlue;
			    next_counter <= current_counter +1;
			   	next_WE <='1';
				end if;
			-- ...
			-- states to manage blue color...
			-- ...
			WHEN S_WriteBlue  => -- ROM Instr 13
				next_WE<='0';
				next_write_address<=write_address+1;
				next_state     <= S_Idle;
				--next_counter <= current_counter +1;
			WHEN S_Idle  =>
				if (read_address=57600) then
					Ready   <= '1';
				else
					next_state<=S_ReadRed;
					next_counter<=1;
				end if;
			WHEN OTHERS => 
				ASSERT false 
					report "illegal FSM state, testbench error"
					severity error;
		END CASE;
	END PROCESS;

P_SYNCH: PROCESS(Clk,reset)
	BEGIN
		IF (reset='0') then
			current_state<=reset_state;
			current_counter<=0;
			WE<='0';
		ELSIF rising_edge(Clk) THEN
			WE<=next_WE;
			read_address <= next_read_address;
			write_address <= next_write_address;
			current_state   <= next_state;
			current_counter <= next_counter;
		END IF;
	END PROCESS;

U_dataPath : dataPath
		GENERIC MAP(Size  => Size, ASize => ASize)
		PORT    MAP(				InPort      => InPort,
								OutPort     => OutPort,    
								Clk         => Clk,       
								Instr     => instr);

	-- Ensure an late write in the first Write state when addresses are stable
	RAM_WE<=WE AND not(clk);
	RAM_ADDRESS<=write_address;
	ROM_ADDRESS<=read_address;
	DataOut<=OutPort;

END behaviour;