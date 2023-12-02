--DLatch aclr 1bit

library ieee;
use ieee.std_logic_1164.all;

entity DLatch is
	port(
	ACLR	: in std_logic;
	DATA	: in std_logic;
	L	: in std_logic;
	Q	: out std_logic
	);
end DLatch;

architecture RTV of DLatch is
	
begin
	process(ACLR, DATA, L)
	begin
		if(ACLR = '1') then
			Q <= '0';
		elsif(L = '1') then
			Q <= DATA;
		end if;
	end process;
end RTV;
