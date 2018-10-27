USE WORK.ALL;
USE WORK.ppm_file_handler.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.all; 
ENTITY test IS
 END test;

ARCHITECTURE tb_VideoComposer OF test IS

  component video_composer IS
  PORT (
    Clk          : IN  STD_LOGIC;
    Reset        : IN  STD_LOGIC;

    Start		 : IN STD_LOGIC;
    Ready		 : OUT STD_LOGIC;
    q		 : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END COMPONENT;

   
	SIGNAL clk         : STD_LOGIC := '0';
	SIGNAL reset       : STD_LOGIC;
	SIGNAL Start	 : STD_LOGIC;
	SIGNAL Ready	 : STD_LOGIC;
	SIGNAL q		 : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL ramAdr   : STD_LOGIC_VECTOR(11 DOWNTO 0);
	--romAdr   : STD_LOGIC_VECTOR(11 DOWNTO 0);

	CONSTANT read_from_file : BOOLEAN := TRUE;
BEGIN

	clk<=NOT(clk) AFTER 10 ns;
	reset<='0', '1' AFTER 25 ns; -- active low reset
	start<='0', '1' after 100 ns;

	DUT: video_composer 
			PORT MAP(	Clk          => clk,
					Reset        => reset, 
					Start		 => Start,
					Ready		 => Ready,
					q		 => q
					
				);
				
	 process(Ready)
    begin
             if rising_edge(Ready) then
            work.ppm_file_handler.dump_ppm<=true;
             end if;
          end process;
	


END tb_VideoComposer;
