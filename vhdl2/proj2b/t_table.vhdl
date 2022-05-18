-- t_table.vhdl   try:  and, or, nand, nor, xor, xnor, not  for type std_logic

library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_arith.all;

entity t_table is
end t_table;

architecture test of t_table is

  procedure e_print(a : std_logic;
                    b : std_logic;
                    c : std_logic) is
    variable my_line : line;
  begin
    write(my_line, a);
    write(my_line, string'(" "));
    write(my_line, b);
    write(my_line, string'(" | "));
    write(my_line, c);
    writeline(output, my_line);
  end e_print;

  procedure t_print( title : string ) is
    variable my_line : line;
  begin
    writeline(output, my_line); -- blank line
    write(my_line, "a b | c   " & title);
    writeline(output, my_line);
    write(my_line, string'("----+--"));
    writeline(output, my_line);
  end t_print;

  procedure s_print(a : std_logic;
                    b : std_logic;
                    c : std_logic;
                    s : std_logic;
                    co : std_logic) is
    variable my_line : line;
  begin
    write(my_line, a);
    write(my_line, string'(" "));
    write(my_line, b);
    write(my_line, string'(" "));
    write(my_line, c);
    write(my_line, string'("   | "));
    write(my_line, s);
    write(my_line, string'("  "));
    write(my_line, co);
    writeline(output, my_line);
  end s_print;

  procedure v_print( title : string ) is
    variable my_line : line;
  begin
    writeline(output, my_line); -- blank line
    write(my_line, "a b cin | s  co   " & title);
    writeline(output, my_line);
    write(my_line, string'("--------+------"));
    writeline(output, my_line);
  end v_print;
  
begin  -- test of t_table
  driver: process                      -- serial code
            variable  c, s, co : std_logic;
            variable my_line : LINE;
          begin  -- process driver
            write(my_line, string'("Truth tables for type std_logic."));
            write(my_line, string'("test input signals  U X 0 1 Z W L H - "));
            writeline(output, my_line);
            t_print("c <= a and b;");
            for a in std_logic loop
              for b in std_logic loop
                c := a and b;
                e_print(a, b, c);
              end loop;
            end loop;
            t_print("c <= a or b;");
            for a in std_logic loop
              for b in std_logic loop
                c := a or b;
                e_print(a, b, c);
              end loop;
            end loop;
            t_print("c <= a nand b;");
            for a in std_logic loop
              for b in std_logic loop
                c := a nand b;
                e_print(a, b, c);
              end loop;
            end loop;
            t_print("c <= a nor b;");
            for a in std_logic loop
              for b in std_logic loop
                c := a nor b;
                e_print(a, b, c);
              end loop;
            end loop;
            t_print("c <= a xor b;");
            for a in std_logic loop
              for b in std_logic loop
                c := a xor b;
                e_print(a, b, c);
              end loop;
            end loop;
            -- t_print("c <= a xnor b;");
            -- for a in std_logic loop
            --   for b in std_logic loop
            --     c := a xnor b;
            --     e_print(a, b, c);
            --   end loop;
            -- end loop;
            v_print("s is sum a b cin,  co is carry out");
            for a in std_logic loop
              for b in std_logic loop
                for c in std_logic loop
                  s := (    a  and  not b  and  not c) or
                       (not a  and      b  and  not c) or
                       (not a  and  not b  and      c) or
                       (    a  and      b  and      c) ;
                  co := (a and b) or (b and c) or (a and c);
                  if ((a='1' or a='0') and (b='1' or b='0') and
                      (c='1' or c='0')) then  -- print normal result
                    s_print(a, b, c, s, co);
                  end if;
                end loop;
              end loop;
            end loop;

            wait for 1 ns;
            -- report  "normal termination" severity ERROR;
          end process driver;
end test;   -- of t_table
