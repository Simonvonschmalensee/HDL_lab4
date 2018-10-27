Library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_signed.ALL;
use IEEE.Numeric_Std.all;

entity RegFile is
	generic (width :integer:=4);
	port(
		data_in		:in	 std_logic_vector(width-1 downto 0);
		WR			:in  std_logic_vector(3 downto 0);
		RA			:in  std_logic_vector(3 downto 0);
		RB 		    :in  std_logic_vector(3 downto 0);
		clk			:in  std_logic;
		reset_n		:in  std_logic;	
		data_outA   :out std_logic_vector(width-1 downto 0);
		data_outB	:out std_logic_vector(width-1 downto 0));
end RegFile;

architecture behavioral of RegFile is
	type   reg_file_type is array(0 to 7) of std_logic_vector(width-1 downto 0);
	signal R:reg_file_type;
	--signal R:array(0 to 7) of std_logic_vector(width-1 downto 0);
begin


	process(clk,reset_n)
	begin
		if reset_n = '0' then
			R(0 to 7) <= (x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00");
		elsif clk = '1' and clk'event then
			if WR(0) = '1' then
				R(to_integer(unsigned(WR(3 downto 1)))) <= data_in;
			end if;
		end if;
	end process;
		data_outA <= R(to_integer(unsigned(RA(3 downto 1)))) when RA(0) = '1' else x"00";
	   data_outB <= R(to_integer(unsigned(RB(3 downto 1)))) when RB(0) = '1' else x"00";
end behavioral;
