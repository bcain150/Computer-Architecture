
-- Entity: inverter_test 
-- Architecture : vhdl 
-- Author: cpatel2
-- Created On: 10/20/00 at 01:55
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use STD.textio.all;

entity inverter_test is

end inverter_test;

architecture test of inverter_test is

component inverter
    port (
    input       : in  std_logic;
    output      : out std_logic);
end component;

for i1 : inverter use entity work.inverter(structural);
   signal ip,op : std_logic;
   signal clock : std_logic;

begin

i1 : inverter port map (ip,op);

clk : process
   begin  -- process clk

    clock<='0','1' after 5 ns;
    wait for 10 ns;

  end process clk;

io_process: process

  file infile  : text is in "inverter_in.txt";
  file outfile : text is out "inverter_out.txt";
  variable ip1,op1 : std_logic;
  variable buf : line;

begin

  while not (endfile(infile)) loop


    readline(infile,buf);
    read (buf,ip1);
    ip<=ip1;

    wait until falling_edge(clock);

    op1:=op;

    write(buf,op1);
    writeline(outfile,buf);

  end loop;

end process io_process;

end test;
