LIBRARY IEEE;
USE IEEE.std_logic_1164.all;

entity Selector is
	generic(width :integer:=4);
	port(
		a	:in  std_logic_vector(width-1 downto 0);
		b	:in  std_logic_vector(width-1 downto 0);
		s	:in	 std_logic;
		q	:out std_logic_vector(width-1 downto 0));   
end Selector;
architecture behavioral of Selector is
begin
	q <= a when s = '0' else b;
end behavioral;
