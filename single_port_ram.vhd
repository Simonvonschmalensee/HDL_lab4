LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
USE work.ppm_file_handler.all;
USE STD.TEXTIO.all;
ENTITY single_port_ram IS
	PORT
	(
		address	: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		data		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		we		: IN STD_LOGIC  := '1';
		q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
	);
END single_port_ram;

architecture write_ppm of single_port_ram is
	subtype uint8 is integer range 0 to 255; -- use integers to speed up simulation
	type mem_type is ARRAY (0 to 2**(Address'high+1)-1) of uint8;

	SHARED VARIABLE mem:mem_type:=(others=>255);
      SIGNAL int_address:integer range 0 to mem_type'high;
	SIGNAL dump_ppm:boolean:=false;
begin
	int_address<=conv_integer(unsigned(address));
	PROCESS(int_address,data,we)
	   VARIABLE data_in:uint8;
	BEGIN
	   data_in:=conv_integer(unsigned(data));
	   IF (WE='1') THEN
	      mem(int_address):=data_in;
	   END IF;
	END PROCESS;
	q<=conv_std_logic_vector(mem(int_address),8);

	dump_ppm<=work.ppm_file_handler.dump_ppm;
	PROCESS(dump_ppm)
		FILE FileOut:PPM_FILE_TYPE OPEN Write_Mode IS  "dataout.ppm";
		variable data_in:uint8;
	BEGIN
	  IF dump_ppm'event and dump_ppm THEN
		WriteHeader(FileOut, work.ppm_file_handler.ppm_header, work.ppm_file_handler.ppm_header_length);
		FOR i in 0 to 57599 LOOP -- Picture is 120*160*3=57600 Bytes long...
			data_in:=mem(i);
			WriteData(data_in, FileOut);
		END LOOP;
	  END IF;
	END PROCESS;

end write_ppm;