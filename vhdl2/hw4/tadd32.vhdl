-- tadd32.vhdl   test add32.vhdl

library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_arith.all;

entity tadd32 is                 -- test bench for add32c
end tadd32;

architecture circuits of tadd32 is
  signal a:    std_logic_vector(31 downto 0) := x"00000000";
                                      -- initial value of 32 bits of zero
  signal b:    std_logic_vector(31 downto 0) := x"FFFFFFFF";
                                      -- initial 32 bit hexadecimal value    
  signal cin:  std_logic := '1';
  signal cout: std_logic;            
  signal sum:  std_logic_vector(31 downto 0);

  procedure my_printout(pa:    std_logic_vector(31 downto 0);
                        pb:    std_logic_vector(31 downto 0);
                        pcin:  std_logic;
                        psum:  std_logic_vector(31 downto 0);
                        pcout: std_logic;
                        pcntr: integer) is
    variable my_line : LINE;
  begin

    write(my_line, string'("  a="));
    hwrite(my_line, pa);
    write(my_line, string'(",  b="));
    hwrite(my_line, pb);
    write(my_line, string'(",  cin="));
    write(my_line, pcin);
    writeline(output, my_line);
    write(my_line, string'("sum="));
    hwrite(my_line, psum);
    write(my_line, string'(",  cout="));
    write(my_line, pcout);
    write(my_line, string'(", cntr="));
    write(my_line, pcntr);
    write(my_line, string'(",  at="));
    write(my_line, now);
    writeline(output, my_line);
    writeline(output, my_line); -- blank line
  end my_printout;
  
begin  -- circuits of tadd32
  adder: entity WORK.add32 port map(a, b, cin, sum, cout); -- parallel circuit

  driver: process                               -- serial code
            variable my_line : line;
            subtype word is std_logic_vector(31 downto 0);
            type vector16 is array(0 to 15) of word;
            variable ta : vector16 :=
              (x"00000000", x"FFFFFFFF", x"88888888", x"20202020",
               x"33333333", x"44444444", x"55555555", x"66666666",
               x"77777777", x"88888888", x"99999999", x"AAAAAAAA",
               x"BBBBBBBB", x"CCCCCCCC", x"DDDDDDDD", x"EEEEEEEE");
            variable tb : vector16 :=
              (x"FFFFFFFF", x"00000000", x"88888888", x"DDDDDDDD",
               x"0C0C0C0C", x"BBBBABBB", x"A9AAAAAA", x"99899999",
               x"88878888", x"77776777", x"66666566", x"55555545",
               x"44444443", x"33333333", x"22222222", x"11111111");
          begin  -- process driver
            write(my_line, string'("Driver starting."));
            writeline(output, my_line);
            
            for i in 0 to 31 loop  -- 32 test cases
              if i mod 2 = 0 then
                cin <= '0';
              else
                cin <= '1';
              end if;
              a   <= ta(i/2);
              b   <= tb(i/2);
              wait for 1 ns;           -- adders propagating signals
              my_printout(a, b, cin, sum, cout, i);  -- write output
              wait for 1 ns;           -- other half of clock cycle
            end loop;  -- i
            wait;  -- never restart this process
          end process driver;
end architecture circuits; -- of tadd32



