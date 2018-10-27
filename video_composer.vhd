USE WORK.ALL;
LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_signed.all;
USE IEEE.std_logic_arith.all;

ENTITY video_composer IS
	PORT (
		Clk          : IN  STD_LOGIC;
		Reset        : IN  STD_LOGIC;

		Start		 : IN STD_LOGIC;
		Ready		 : OUT STD_LOGIC;
		q		 : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
		);
END video_composer;

Architecture structure of video_composer is
   COMPONENT VideoComposer_FSMD
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
   END COMPONENT;
   COMPONENT single_port_rom
	PORT(
		address	: IN STD_LOGIC_VECTOR (15 DOWNTO 0); -- 57600 Byte needed for image (120*160 RGB)
		q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
		);
   END COMPONENT;
   COMPONENT single_port_ram
	PORT(
		address	: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		data		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		we		: IN STD_LOGIC  := '1';
		q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
		);
   END COMPONENT;

   SIGNAL	ROM_address : STD_LOGIC_VECTOR(15 DOWNTO 0);
   SIGNAL 	DataIn	: STD_LOGIC_VECTOR(7 DOWNTO 0);
   SIGNAL 	RAM_address : STD_LOGIC_VECTOR(15 DOWNTO 0);
   SIGNAL 	RAM_WE	: STD_LOGIC;
   SIGNAL 	DataOut     : STD_LOGIC_VECTOR(7 DOWNTO 0);
begin
   U0: VideoComposer_FSMD
	port map(
		Clk          => Clk,
		Reset        => Reset,

		Start		 => Start,
		Ready		=> Ready,

		ROM_address => ROM_address,
		DataIn	=> DataIn,

		RAM_address => RAM_address,
		RAM_WE	=> RAM_WE,
		DataOut     => DataOut
	);
   U1: single_port_rom
	port map(
		address 	=> ROM_address,
		q 		=> DataIn
	);
   U2: single_port_ram
	port map(
		address 	=> RAM_address,
		data		=> DataOut,
		we	 	=> RAM_WE,
		q		=> q
	);
end structure;
