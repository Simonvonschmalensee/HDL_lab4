Library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_signed.ALL;

entity ALU is 
	generic (width:integer:=4);
	port(
		a		:in  std_logic_vector(width-1 downto 0);
		b		:in  std_logic_vector(width-1 downto 0);
		aluop	:in  std_logic_vector(2 downto 0);
		aluout	:out std_logic_vector(width-1 downto 0));
	
end ALU;
architecture behavioral of ALU is
begin
	process(a,b,aluop)
	begin
		case aluop is
			when "000"  => aluout <= (not a);
			when "001"  => aluout <= (a and b);
			when "010"  => aluout <= (a xor b);
			when "011"  => aluout <= (a or b);
			when "100"  => aluout <= (a - '1');
			when "101"  => aluout <= (a + b);
			when "110"  => aluout <= (a - b);
			when "111"  => aluout <= (a + '1');
			when others => aluout <= x"00";
		end case;
	end process;
end behavioral;
