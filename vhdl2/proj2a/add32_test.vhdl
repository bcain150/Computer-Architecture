-- add32_test.vhdl    this could be three or more files

library IEEE;
use IEEE.std_logic_1164.all;

entity fadd is               -- full adder stage, interface
  port(a    : in  std_logic;
       b    : in  std_logic;
       cin  : in  std_logic;
       s    : out std_logic;
       cout : out std_logic);
end entity fadd;

architecture circuits of fadd is  -- full adder stage, body
begin  -- circuits of fadd
  s <= a xor b xor cin after 1 ns;
  cout <= (a and b) or (a and cin) or (b and cin) after 1 ns;
end architecture circuits; -- of fadd


library IEEE;
use IEEE.std_logic_1164.all;
entity add32 is             -- simple 32 bit ripple carry adder
  port(a    : in  std_logic_vector(31 downto 0);
       b    : in  std_logic_vector(31 downto 0);
       cin  : in  std_logic; 
       sum  : out std_logic_vector(31 downto 0);
       cout : out std_logic);
end entity add32;

architecture circuits of add32 is
  signal c : std_logic_vector(0 to 30); -- internal carry signals
begin  -- circuits of add32
  a0: entity WORK.fadd port map(a(0), b(0), cin, sum(0), c(0));
  stage: for I in 1 to 30 generate
             as: entity WORK.fadd port map(a(I), b(I), c(I-1) , sum(I), c(I));
         end generate stage;
  a31: entity WORK.fadd port map(a(31), b(31), c(30) , sum(31), cout);
end architecture circuits;  -- of add32


library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_arith.all;

entity add32_test is
end add32_test;

architecture circuits of add32_test is
  signal cntr: std_logic_vector(3 downto 0) := b"0000";
  signal a:    std_logic_vector(31 downto 0) := x"00000000";
                                      -- initial value of 32 bits of zero
  signal b:    std_logic_vector(31 downto 0) := x"FFFFFFFF";
                                      -- initial 32 bit hexadecimal value    
  signal cin:  std_logic := '1';
  signal cout: std_logic;            
  signal sum:  std_logic_vector(31 downto 0);
  signal co2:  std_logic;             -- test with a and b interchanged
  signal s2:   std_logic_vector(31 downto 0);
                                      -- to be sure circuit is symmetric
  procedure my_printout is              -- format output
    variable my_line : LINE;
  begin
    write(my_line, string'("    a="));
    hwrite(my_line, a);
    write(my_line, string'(",    b="));
    hwrite(my_line, b);
    write(my_line, string'(",  cin="));
    write(my_line, cin);
    writeline(output, my_line);
    write(my_line, string'("  sum="));
    hwrite(my_line, sum);
    write(my_line, string'(", cout="));
    write(my_line, cout);
    write(my_line, string'(",   s2="));
    hwrite(my_line, s2);
    write(my_line, string'(",  co2="));
    write(my_line, co2);
    write(my_line, string'(", cntr="));
    write(my_line, cntr);
    write(my_line, string'(",  at="));
    write(my_line, now);
    writeline(output, my_line);
    writeline(output, my_line); -- blank line
  end my_printout;
  
begin  -- circuits of add32_test
  adder: entity WORK.add32 port map(a, b, cin, sum, cout); -- parallel circuit
  addrv: entity WORK.add32 port map(b, a, cin, s2,  co2);  -- parallel circuit
  cntr <= unsigned(cntr) + unsigned'(b"0001") after 40 ns; -- increment counter
  driver: process                               -- serial code
            variable my_line : LINE;
          begin  -- process driver
            write(my_line, string'("Driver starting."));
            writeline(output, my_line);
            
            for i in 0 to 4 loop  -- 4 test cases
              cin   <= cntr(0) after 1 ns;
              a( 3 downto  0)  <= cntr after 1 ns;
              a( 7 downto  4)  <= cntr after 1 ns;
              a(11 downto  8)  <= cntr after 1 ns;
              a(31 downto 28)  <= cntr after 1 ns;
              wait for 38 ns;           -- adders propagating signals
              my_printout;              -- write output
              wait for 2 ns;            -- rest of 40 ns test cycle
            end loop;  -- i
          end process driver;
end architecture circuits; -- of add32_test
