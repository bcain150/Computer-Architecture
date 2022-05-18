-- pmul16_test.vhdl   test entity pmul16

--signal a[msb];  multiplier   msb=3, 7, 11, 15, ... ,31, 63
--signal b[msb];  multiplicand
--signal p[msb2];  product

library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_arith.all;

entity pmul16_test is
end pmul16_test;

architecture circuits of pmul16_test is
  constant msb  : integer := 15;        -- most significant bit
  constant msb2 : integer := 2*msb+1;   -- double length msb
  constant prop : time := (2*msb+3)*(1 ns);   -- circuit propegation delay
  signal cntr   : std_logic_vector(7 downto 0) := X"00";
  signal one    : std_logic_vector(7 downto 0) := X"01";
  signal a      : std_logic_vector(msb downto 0);
  signal b      : std_logic_vector(msb  downto 0);
  signal p      : std_logic_vector(msb2 downto 0);

  procedure my_printout(a   : std_logic_vector(msb  downto 0);
                        b   : std_logic_vector(msb  downto 0);
                        p   : std_logic_vector(msb2 downto 0)) is
    variable my_line : line;
  begin
    write(my_line, string'("a="));
    write(my_line, a);
    write(my_line, string'(", b="));
    write(my_line, b);
    write(my_line, string'(",  p="));
    write(my_line, p);
    write(my_line, string'(", cntr="));
    write(my_line, cntr);
    write(my_line, string'(",  at="));
    write(my_line, now);
    writeline(output, my_line);
  end my_printout;
  
begin  -- circuits of pmul16_test
  mult32: entity WORK.pmul16 port map(a, b, p); -- parallel circuit
  driver: process                      -- serial code
            variable my_line : LINE;
          begin  -- process driver
            write(my_line, string'("Driver starting."));
            writeline(output, my_line);
            
            for i in 0 to 255 loop
              a( 3 downto  0) <= cntr(3 downto 0);
              a( 7 downto  4) <= cntr(3 downto 0);
              a(11 downto  8) <= cntr(3 downto 0);
              a(msb downto msb-3) <= cntr(3 downto 0); -- as many as needed
              b( 3 downto  0) <= cntr(7 downto 4);
              b( 7 downto  4) <= cntr(7 downto 4);
              b(11 downto  8) <= cntr(7 downto 4);
              b(msb downto msb-3) <= cntr(7 downto 4); -- as many as needed
              wait for prop;  -- pseudo clock wait for propogation
              my_printout(a, b, p);  -- write output
              cntr <= unsigned(cntr) + unsigned(one);
              wait for 1 ns;
            end loop;  -- i total run time = (2*msb+4)*256 ns
          end process driver;
end architecture circuits; -- of pmul16_test

