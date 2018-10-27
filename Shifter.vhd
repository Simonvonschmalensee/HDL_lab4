Library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_signed.ALL;

entity Shifter is
	generic(width:integer:=4);
	port(
		shift_in	:in  std_logic_vector(width-1 downto 0);
		shift_op	:in  std_logic_vector(2 downto 0);
		shift_out	:out std_logic_vector(width-1 downto 0));
end Shifter;
architecture behavioral of Shifter is
begin
	process(shift_in,shift_op)
	begin
		case shift_op is
			when "000" | "001" => shift_out <= shift_in;
			when "100"		   => shift_out <= shift_in(width-2 downto 0) & '0';
			when "101"		   => shift_out <= shift_in(width-2 downto 0) & shift_in(width-1);
			when "110"		   => shift_out <= '0' & shift_in(width-1 downto 1);
			when "111"		   => shift_out <= shift_in(0) & shift_in(width-1 downto 1);
			when others		   => shift_out <= shift_in;
		end case;
	end process;
end behavioral;
