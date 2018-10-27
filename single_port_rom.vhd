LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
USE work.ppm_file_handler.all;
USE STD.TEXTIO.all;
ENTITY single_port_rom IS
  PORT(
		address	: IN STD_LOGIC_VECTOR (15 DOWNTO 0); -- 57600 Byte needed for image (120*160 RGB)
		q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
	);
END single_port_rom;

architecture read_ppm of single_port_rom is

subtype uint8 is integer range 0 to 255; -- use integers to speed up simulation
type mem_type is ARRAY (0 to 2**(Address'high+1)-1) of uint8;
signal mem_sig : mem_type:=(others=>0); --DEBUG

begin
	PROCESS(address)
		VARIABLE hash_table : HashTable_Type; 
		VARIABLE init:boolean:=FALSE;
		VARIABLE pointer:INTEGER:=0;
   	VARIABLE mem:mem_type:=(others=>255);
		VARIABLE header_length:header_length_type;
		VARIABLE header:header_type;
		FILE FileIn:TEXT OPEN Read_Mode  IS  "DATAIN.PPM";
		variable data_in:uint8;
	
	BEGIN
	  IF not(init) THEN
		CreateHashTable(hash_table);
		ReadHeader(FileIn, hash_table, header, header_length);
		work.ppm_file_handler.ppm_header<=header;
		work.ppm_file_handler.ppm_header_length<=header_length;
		WHILE NOT Endfile(FileIn) LOOP
			ReadData(data_in, FileIn, hash_table);
			mem(pointer):=data_in;
			pointer:=pointer+1;
		END LOOP;
		init:=true;
	  ELSE
		  pointer:=conv_integer(unsigned(Address));
		  q<=conv_std_logic_vector(mem(pointer),8);
	  END IF;
	  
	  mem_sig <= mem; --DEBUG
	  
	END PROCESS;

END read_ppm;
