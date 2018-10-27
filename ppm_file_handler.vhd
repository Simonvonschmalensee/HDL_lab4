USE Std.TextIO.ALL;
LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
PACKAGE ppm_file_handler IS
	TYPE HashTable_Type IS ARRAY(CHARACTER) OF INTEGER; 
	TYPE Header_Type IS ARRAY(0 to 255) OF CHARACTER;
	TYPE PPM_FILE_TYPE is FILE of CHARACTER;
	SUBTYPE header_length_type is INTEGER RANGE header_type'range;
      SIGNAL dump_ppm:boolean:=FALSE;
	SIGNAL ppm_header:header_Type;
      SIGNAL ppm_header_length:INTEGER RANGE ppm_header'range;
	PROCEDURE CreateHashTable(
		VARIABLE hash_table : OUT HashTable_Type
	);
	PROCEDURE ReadHeader(
			FILE FileIn : Std.TextIO.Text; 
			VARIABLE HashTable : IN HashTable_Type;
			-- FILE FileOut : Std.TextIO.Text;
			VARIABLE Header : OUT Header_Type;
			VARIABLE Header_length : OUT header_length_type
	);
	PROCEDURE WriteHeader(
			FILE FileOut : PPM_FILE_TYPE; -- Std.TextIO.Text; 
			SIGNAL Header : IN Header_Type;
			SIGNAL Header_length : IN header_length_type
	);
	PROCEDURE ReadData(
			VARIABLE data_in  : OUT INTEGER RANGE 0 to 255;
			FILE   FileIn   : Std.TextIO.Text;
			VARIABLE HashTable : HashTable_Type
	);
	PROCEDURE WriteData(
			VARIABLE data_out   : IN INTEGER RANGE 0 to 255;
			FILE   FileOut    : PPM_FILE_TYPE -- Std.TextIO.Text
	);
end ppm_file_handler;
package body ppm_file_handler is
	PROCEDURE CreateHashTable(
		VARIABLE hash_table : OUT HashTable_Type
	) IS
		VARIABLE tmp  : INTEGER := 0;  -- Temp. var. for creation of hash table
	BEGIN
		FOR i IN CHARACTER LOOP
			hash_table(i) := tmp;
			tmp := tmp+1;
		END LOOP;       
	END CreateHashTable;

	PROCEDURE ReadHeader(
			FILE FileIn : Std.TextIO.Text; 
			VARIABLE HashTable : IN HashTable_Type;
			-- FILE FileOut : Std.TextIO.Text;
			VARIABLE Header : OUT Header_Type;
			VARIABLE Header_length : OUT header_length_type
	) IS
		VARIABLE buf  : STRING(1 DOWNTO 1); 
				-- The read character, i.e., one char of the string.
		VARIABLE len  : INTEGER;
				-- A “dummy” var. for getting the READ syntax right
		VARIABLE char : CHARACTER;              -- A temporary character
		VARIABLE int  : INTEGER RANGE 0 TO 255;
				-- The integer value of the read ASCII character
		VARIABLE char_count : INTEGER RANGE 0 to ppm_header'high;
		VARIABLE state    : INTEGER := 0;
	BEGIN
    		LOOP
			READ(FileIn, buf, len);           -- Read 1 character from the input file
			--WRITE(FileOut, buf);              -- Write the character char to the output file
			Header(char_count):=buf(1);
			char_count:=char_count+1;
			char := buf(1);                   -- Extraction of curr. read character
			int := HashTable(char);           -- Make the char integer val. survive
			CASE state IS
				WHEN 0 => 
					IF int = 50 THEN 
						state := 1;
					ELSE
						state := 0;
					END IF;
				WHEN 1 =>
					IF int = 53 THEN
						state := 2;
					ELSIF int = 2 THEN
						state := 1;
					ELSE
						state := 0;
					END IF;
				WHEN 2 =>
					IF int = 53 THEN
						state := 3;
					ELSIF int = 2 THEN
						state := 1;
					ELSE
						state := 0;
					END IF;
				WHEN 3 =>
					IF int = 10 THEN
						EXIT;
					ELSIF int = 2 THEN
						state := 1;
					ELSE
						state := 0;
					END IF;
				WHEN OTHERS => 
					null;
			END CASE;
		END LOOP;
		header_length:=char_count;
	END ReadHeader;
	PROCEDURE WriteHeader(
			FILE FileOut : PPM_FILE_TYPE; -- Std.TextIO.Text; 
			SIGNAL Header : IN Header_Type;
			SIGNAL Header_length : IN header_length_type
	) IS
		VARIABLE buf  : STRING(1 DOWNTO 1);     -- The read character i.e. one char of the string.
	BEGIN
	   For i in 0 to Header_length-1 loop
		-- buf(1) := Header(i);
		WRITE(FileOut,Header(i));
	   end loop;
	END WriteHeader;

	PROCEDURE ReadData(
			VARIABLE data_in  : OUT INTEGER RANGE 0 to 255;
			FILE   FileIn   : Std.TextIO.Text;
			VARIABLE HashTable : HashTable_Type
	) IS

		VARIABLE buf  : STRING(1 DOWNTO 1);     -- The read character i.e. one char of the string.
		VARIABLE len  : INTEGER;                -- A “dummy” var. for getting the READ syntax right
		VARIABLE char : CHARACTER;              -- A temporary character
		VARIABLE int  : INTEGER RANGE 0 TO 255; -- The integer value of a the read ASCII character

	BEGIN
			READ(FileIn, buf, len);           -- Read 1 character from the input file
			char := buf(1);                   -- Extraction of curr. read character
			int := HashTable(char);           -- Make the char integer val. survive
			data_in:=int;
	END ReadData;


	PROCEDURE WriteData(
			VARIABLE data_out   : IN INTEGER RANGE 0 to 255;
			FILE   FileOut    : PPM_FILE_TYPE -- Std.TextIO.Text
	) IS
		VARIABLE buf  : STRING(1 DOWNTO 1);     -- The read character i.e. one char of the string.
		VARIABLE char : CHARACTER;              -- A temporary character
	BEGIN
			char := character'val(data_out);
			-- buf(1) := char;
			WRITE(FileOut, char);                -- Write the character char to the output file
	END WriteData;
END ppm_file_handler;