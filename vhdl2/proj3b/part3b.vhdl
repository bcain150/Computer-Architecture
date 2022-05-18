-- Created by Brendan Cain
-- CMSC 411 Final Proj
-- Fall 2020
--  part3b.vhdl   VHDL '93 version using entities from WORK library
--              basic five stage pipeline of MIPS architecture
--              The 411 course pipeline has the same five stages
--              IF Instruction Fetch includes PC and instruction memory
--              ID Instruction Decode and registers
--              EX Execution including the ALU Arithmetic Logic Unit
--              MEM data Memory
--              WB Write Back into registers
--
--              The signal naming convention uses the stage as a prefix
--              WORK library needs entities and architectures for
--              add32  and  bshift and pmul16 and divcas16 .
--
-- This self contained VHDL file defines:
--     a package declaration and body that defines functions and memory
--     a 32 bit and a 5 bit register entity with clock and clear inputs
--     an instruction memory entity and behavioral architecture
--     a data memory entity and behavioral architecture
--     a general register entity and behavioral architecture
--     multiplexer entities and behavioral architectures
--     equal comparator entities and circuit architectures
--     an incomplete ALU entity and schematic architecture

--     a top level entity, part3b, test bench
--     the architecture, schematic layout of the top level entity
--     the signals for interconnecting the entities
--     a clock generator process
--     the entities connected with signals (port maps)
--     a memory read process that reads "part3b.abs"
--     a print process that shows the registers in the pipeline each clock

library IEEE;
use IEEE.std_logic_1164.all;

package util_pkg is
  function to_integer(sig : std_logic_vector) return integer;

  -- main memory, a process reads a file to load memory
  subtype word is std_logic_vector(31 downto 0);
  type mem_array is array(integer range <>) of word;
  shared variable memory: mem_array(0 to 4095);  -- max 12 bit addresses

  -- general register memory
  type reg_mem_type is array (natural range <>) of word;
  shared variable reg_mem : reg_mem_type(0 to 31) := (others =>(others =>'0'));
end package util_pkg;

package body util_pkg is
  function to_integer(sig : std_logic_vector) return integer is
    variable num : integer := 0;  -- descending sig as integer
  begin
    for i in sig'range loop
      if sig(i)='1' then
        num := num*2+1;
      else
        num := num*2;
      end if;
    end loop;  -- i
    return num;
  end function to_integer;
end package body util_pkg;

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity register_32 is
  port(clk    : in  std_logic;
       clear  : in  std_logic;
       input  : in  std_logic_vector (31 downto 0);
       output : out std_logic_vector (31 downto 0) );
end entity register_32;

architecture behavior of register_32 is
begin  -- behavior
  reg_32: process(clk, clear)
          begin
            if clear='1' then  -- only once
              output <= (others=>'0');
            elsif clk'event and clk='1' then
              output <= input after 250 ps;
            end if;
          end process reg_32;
end architecture behavior;  -- of register_32

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity register_5 is
  port(clk    : in  std_logic;
       clear  : in  std_logic;
       input  : in  std_logic_vector (4 downto 0);
       output : out std_logic_vector (4 downto 0) );
end entity register_5;

architecture behavior of register_5 is
begin  -- behavior
  reg_5: process(clk, clear)
         begin
           if clear='1' then  -- only once
             output <= (others=>'0');
           elsif clk'event and clk='1' then
             output <= input after 250 ps;
           end if;
         end process reg_5;
end architecture behavior;  -- of register_5

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use WORK.util_pkg.all;
use IEEE.std_logic_textio.all;

entity instruction_memory is
  port(clear : in  std_logic;
       addr  : in  std_logic_vector (31 downto 0);
       inst  : out std_logic_vector (31 downto 0);
       miss  : out std_logic );
end entity instruction_memory;

architecture behavior of instruction_memory is
  subtype block_type is std_logic_vector(154 downto 0);
  type cache_type is array (0 to 3) of block_type;
  signal cache : cache_type := (others=>(others=>'0'));
  signal local_miss : std_logic := '0'; -- needed between process calls
  -- now we have a cache memory initialized to zero

begin  -- behavior
  inst_mem : process(addr, clear, local_miss)       --compute same as miss
    variable quad_word_address : natural;  -- for memory fetch
    variable cblock : block_type;-- the shaded block in the cache
    variable index : natural;   -- index into cache to get a block
    variable word : natural;    -- select a word
    variable my_line : line;    -- for debug printout
    alias tag   : std_logic_vector(25 downto 0) is cblock(153 downto 128);
    alias w0    : std_logic_vector(31 downto 0) is cblock(127 downto 96);
    alias w1    : std_logic_vector(31 downto 0) is cblock(95 downto 64);
    alias w2    : std_logic_vector(31 downto 0) is cblock(63 downto 32);
    alias w3    : std_logic_vector(31 downto 0) is cblock(31 downto 0);
    alias valid : std_logic is cblock(154); -- other alias allowed
  begin

    if clear = '1' then
      miss <= '0';
      inst <= x"00000000";
    end if;
    if clear = '0' then
      index := to_integer(addr(5 downto 4));
      word  := to_integer(addr(3 downto 2));
      cblock := cache(index);   -- has valid (154), tag (153 downto 128),
      --W0 (127 downto 96), W1(95 downto 64) W2(63 downto 32), W3 (31 downto 0), cblock is the shaded block in handout
      if (valid = '1') and (tag = addr(31 downto 6)) then
          miss <= '0';
        if word = 0 then
          inst <= w0 after 250 ps;
        end if;
        if word = 1 then
          inst <= w1 after 250 ps;
        end if;
        if word = 2 then
          inst <= w2 after 250 ps;
        end if;
        if word = 3 then
          inst <= w3 after 250 ps;
        end if;
      else -- miss
        quad_word_address := to_integer(addr(13 downto 4));
        w0   := memory(quad_word_address*4+0);
        w1   := memory(quad_word_address*4+1);
        w2   := memory(quad_word_address*4+2);
        w3   := memory(quad_word_address*4+3);
        tag  := addr(31 downto 6); -- update tag
        valid := '1';
        cache(index) <= cblock after 30 ns; -- 3 clock delay
        miss <= '1', '0' after 30 ns;       -- miss is '1' for 30 ns
        local_miss <= '1', '0' after 30 ns; -- to get process to run
        -- this "miss" signal gets ored into part2b "stall" signal
        if word = 0 then
          inst <= x"00000000", w0 after 30 ns;
        end if;
        if word = 1 then
          inst <= x"00000000", w1 after 30 ns;
        end if;
        if word = 2 then
          inst <= x"00000000", w2 after 30 ns;
        end if;
        if word = 3 then
          inst <= x"00000000", w3 after 30 ns;
        end if;
      end if;
    end if; -- clear = '0'
  end process inst_mem;

end architecture behavior;  -- of instruction_memory

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use WORK.util_pkg.all;
use IEEE.std_logic_textio.all;

entity data_memory is
  port(address      : in  std_logic_vector (31 downto 0);
       write_data   : in  std_logic_vector (31 downto 0);
       read_enable  : in  std_logic;  -- from address
       write_enable : in  std_logic;  -- rising clock and enable
       write_clk    : in  std_logic;  -- required to write
       clear        : in  std_logic;  -- clears memory
       read_data    : out std_logic_vector (31 downto 0);
       miss         : out std_logic); -- miss line (Dmiss)
end entity data_memory;

architecture behavior of data_memory is
  subtype block_type is std_logic_vector(154 downto 0);
  type cache_type is array (0 to 3) of block_type;
  signal cache : cache_type := (others=>(others=>'0'));
  signal local_miss : std_logic := '0'; -- needed between process calls
  -- now we have a cache memory initialized to zero
begin  -- behavior

  data_mem : process(address, clear, read_enable, write_enable, write_data, local_miss)       --compute same as miss
    variable quad_word_address : natural;  -- for memory fetch
    variable cblock : block_type;-- the shaded block in the cache
    variable index : natural;   -- index into cache to get a block
    variable word : natural;    -- select a word
    variable my_line : line;    -- for debug printout
    variable word_addr : natural;  -- byte addr/4
    alias tag   : std_logic_vector(25 downto 0) is cblock(153 downto 128);
    alias w0    : std_logic_vector(31 downto 0) is cblock(127 downto 96);
    alias w1    : std_logic_vector(31 downto 0) is cblock(95 downto 64);
    alias w2    : std_logic_vector(31 downto 0) is cblock(63 downto 32);
    alias w3    : std_logic_vector(31 downto 0) is cblock(31 downto 0);
    alias valid : std_logic is cblock(154); -- other alias allowed
  begin
    if clear='1' then
	    miss <= '0';
	  end if;
    if clear='0' and local miss='0' and (read_enable='1' or (write_enable='1' and write_clk'event and write_clk='1')) then
      word_addr := to_integer(address(13 downto 2));  -- 12 bits
      index := to_integer(address(5 downto 4));
      word  := to_integer(address(3 downto 2));
      cblock := cache(index);   -- has valid (154), tag (153 downto 128),
      if write_enable = '1' then
        if (valid = '1') and (tag = address(31 downto 6)) then -- hit
            miss <= '0';
          if word = 0 then
            w0 := write_data;
          end if;
          if word = 1 then
            w1 := write_data;
          end if;
          if word = 2 then
            w2 := write_data;
          end if;
          if word = 3 then
            w3 := write_data;
          end if;
          cache(index) <= cblock after 250 ps;
        else -- miss
          quad_word_address := to_integer(address(13 downto 4));
          tag  := address(31 downto 6); -- update tag
          valid := '1';
          miss <= '1', '0' after 30 ns;       -- miss is '1' for 30 ns
          local_miss <= '1', '0' after 30 ns; -- to get process to run
          -- this "miss" signal gets ored into part2b "stall"  and creates dclk signal
          if word = 0 then
            w0 := write_data;
            w1 := memory(quad_word_address*4+1);
            w2 := memory(quad_word_address*4+2);
            w3 := memory(quad_word_address*4+3);
          end if;
          if word = 1 then
            w0 := memory(quad_word_address*4+0);
            w1 := write_data;
            w2 := memory(quad_word_address*4+2);
            w3 := memory(quad_word_address*4+3);
          end if;
          if word = 2 then
            w0 := memory(quad_word_address*4+0);
            w1 := memory(quad_word_address*4+1);
            w2 := write_data;
            w3 := memory(quad_word_address*4+3);
          end if;
          if word = 3 then
            w0 := memory(quad_word_address*4+0);
            w1 := memory(quad_word_address*4+1);
            w2 := memory(quad_word_address*4+2);
            w3 := write_data;
          end if;
          cache(index) <= cblock after 30 ns;
          read_data <= write_data;
        end if;
        memory(word_addr) := write_data;  -- write main memory
      elsif read_enable = '1' then
        --W0 (127 downto 96), W1(95 downto 64) W2(63 downto 32), W3 (31 downto 0), cblock is the shaded block in handout
        if (valid = '1') and (tag = address(31 downto 6)) then -- hit
            miss <= '0';
          if word = 0 then
            read_data <= w0 after 250 ps;
          end if;
          if word = 1 then
            read_data <= w1 after 250 ps;
          end if;
          if word = 2 then
            read_data <= w2 after 250 ps;
          end if;
          if word = 3 then
            read_data <= w3 after 250 ps;
          end if;
        else -- miss
          quad_word_address := to_integer(address(13 downto 4));
          w0   := memory(quad_word_address*4+0);
          w1   := memory(quad_word_address*4+1);
          w2   := memory(quad_word_address*4+2);
          w3   := memory(quad_word_address*4+3);
          tag  := address(31 downto 6); -- update tag
          valid := '1';
          cache(index) <= cblock after 30 ns; -- 3 clock delay
          miss <= '1', '0' after 30 ns;       -- miss is '1' for 30 ns
          local_miss <= '1', '0' after 30 ns; -- to get process to run
          -- this "miss" signal gets ored into part2b "stall" signal
          if word = 0 then
            read_data <= x"00000000", w0 after 30 ns;
          end if;
          if word = 1 then
            read_data <= x"00000000", w1 after 30 ns;
          end if;
          if word = 2 then
            read_data <= x"00000000", w2 after 30 ns;
          end if;
          if word = 3 then
            read_data <= x"00000000", w3 after 30 ns;
          end if;
        end if;
      else
        read_data <= x"00000000";
      end if; -- write_enable = '1', read_enable = '1'
    end if; -- clear local miss if statement etc
  end process data_mem;

  debug:  process -- used to print contents of D cache, use part3b_print.chk
            variable my_line : LINE;   -- not part of working circuit
          begin
            wait for 9.5 ns;         -- just before rising clock
            for I in 0 to 3 loop
               write(my_line, string'("line="));
               write(my_line, I);
               write(my_line, string'("  V="));
               write(my_line, cache(I)(154));
               write(my_line, string'("  tag="));
               hwrite(my_line, cache(I)(151 downto 128));  -- ignore top bits
               write(my_line, string'("  w0="));
               hwrite(my_line, cache(I)(127 downto 96));
               write(my_line, string'("  w1="));
               hwrite(my_line, cache(I)(95 downto 64));
               write(my_line, string'("  w2="));
               hwrite(my_line, cache(I)(63 downto 32));
               write(my_line, string'("  w3="));
               hwrite(my_line, cache(I)(31 downto 0));
               writeline(output, my_line);
            end loop;
            wait for 0.5 ns;         -- rest of clock
          end process debug;
     -- end architecture behavior;  -- of data_memory

end architecture behavior;  -- of data_memory

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use WORK.util_pkg.all;

entity registers is
  port(read_reg_1   : in  std_logic_vector (4 downto 0); -- address
       read_reg_2   : in  std_logic_vector (4 downto 0); -- address
       write_reg    : in  std_logic_vector (4 downto 0); -- address
       write_data   : in  std_logic_vector (31 downto 0);
       write_enable : in  std_logic;  -- rising clock and enable
       write_clk    : in  std_logic;  -- required to write
       read_data_1  : out std_logic_vector (31 downto 0);
       read_data_2  : out std_logic_vector (31 downto 0));
end entity registers;

architecture behavior of registers is
begin  -- behavior
  reg: process(read_reg_1, read_reg_2, write_clk)
         variable reg_addr : natural;
       begin
         if write_enable='1' and write_clk'active and write_clk='1' then
           reg_addr := to_integer(write_reg);
           if reg_addr/=0 then           -- can not change register zero
             reg_mem(reg_addr) := write_data;
           end if;
         end if;
         read_data_1 <= reg_mem(to_integer(read_reg_1));
         read_data_2 <= reg_mem(to_integer(read_reg_2));
         -- signals updated after process exits
       end process reg;
end architecture behavior;  -- of registers

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity mux_32 is
  port(in0    : in  std_logic_vector (31 downto 0);
       in1    : in  std_logic_vector (31 downto 0);
       ctl    : in  std_logic;
       result : out std_logic_vector (31 downto 0));
end entity mux_32;

architecture behavior of mux_32 is
begin  -- behavior -- no process needed with concurrent statements
  result <= in1 when ctl='1' else in0 after 250 ps;
end architecture behavior;  -- of mux_32

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
entity mux32_3 is
  port(in0    : in  std_logic_vector (31 downto 0);
       in1    : in  std_logic_vector (31 downto 0);
       in2    : in  std_logic_vector (31 downto 0);
       ct1    : in  std_logic;          -- pass in1(has priority)
       ct2    : in  std_logic;          -- pass in2
       result : out std_logic_vector (31 downto 0));
end entity mux32_3;

architecture behavior of mux32_3 is
begin  -- behavior -- no process needed with concurrent statements
  result <= in1 when ct1='1' else in2 when ct2='1' else in0 after 50 ps;
end architecture behavior;  -- of mux32_3

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity mux32_6 is
  port(in0    : in  std_logic_vector (31 downto 0);
       in1    : in  std_logic_vector (31 downto 0);
       in2    : in  std_logic_vector (31 downto 0);
       in3    : in  std_logic_vector (31 downto 0);
       in4    : in  std_logic_vector (31 downto 0);
       in5    : in  std_logic_vector (31 downto 0);
       ct1   : in  std_logic;
       ct2   : in  std_logic;
       ct3   : in  std_logic;
       ct4   : in  std_logic;
       ct5   : in  std_logic;
       result : out std_logic_vector (31 downto 0));
end entity mux32_6;

architecture behavior of mux32_6 is
begin  -- behavior -- no process needed with concurrent statements
  result <= in1 when ct1='1' else in2 when ct2='1' else
            in3 when ct3='1' else in4 when ct4='1' else
            in5 when ct5='1' else in0 after 100 ps;
end architecture behavior;  -- of mux32_6

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity mux_5 is
  port(in0    : in  std_logic_vector (4 downto 0);
       in1    : in  std_logic_vector (4 downto 0);
       ctl    : in  std_logic;
       result : out std_logic_vector (4 downto 0));
end entity mux_5;

architecture behavior of mux_5 is
begin  -- behavior -- no process needed with concurrent statements
  result <= in1 when ctl='1' else in0 after 250 ps;
end architecture behavior;  -- of mux_5

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity equal32 is --  a 32-bit compare
  port(inst  : in  std_logic_vector(31 downto 0);
       test  : in  std_logic_vector(31 downto 0);
       equal : out std_logic);
end entity equal32;

architecture circuits of equal32 is
  signal temp : std_logic_vector(0 to 32) := (others=>'1');
begin  -- circuits
  t1: for I in 0 to 31 generate
        temp(I+1) <= (inst(I) xnor test(I)) and temp(I);
      end generate t1;
  equal <= temp(32);
end architecture circuits;  -- of equal32

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity equal6 is -- basically a 6-bit op code compare
  port(inst  : in  std_logic_vector(5 downto 0);
       test  : in  std_logic_vector(5 downto 0);
       equal : out std_logic);
end entity equal6;

architecture circuits of equal6 is
  signal temp : std_logic_vector(0 to 6) := (others=>'1');
begin  -- circuits
  t1: for I in 0 to 5 generate
        temp(I+1) <= (inst(I) xnor test(I)) and temp(I);
      end generate t1;
  equal <= temp(6);
end architecture circuits;  -- of equal6

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity equal5 is -- basically a 5-bit register number compare
  port(inst  : in  std_logic_vector(4 downto 0);
       test  : in  std_logic_vector(4 downto 0);
       equal : out std_logic);
end entity equal5;

architecture circuits of equal5 is
  signal temp : std_logic_vector(0 to 5) := (others=>'1');
begin  -- circuits
  t1: for I in 0 to 4 generate
        temp(I+1) <= (inst(I) xnor test(I)) and temp(I);
      end generate t1;
  equal <= temp(5);
end architecture circuits;  -- of equal5

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity alu_32 is
  port(inA    : in  std_logic_vector (31 downto 0);
       inB    : in  std_logic_vector (31 downto 0);
       inst   : in  std_logic_vector (31 downto 0);
       result : out std_logic_vector (31 downto 0));
end entity alu_32;


architecture schematic of alu_32 is
  signal cout    : std_logic; --- unsure if needed, probably a no connect
  signal mulresult : std_logic_vector (31 downto 0);
  signal divresult : std_logic_vector (31 downto 0);
  signal divrem    : std_logic_vector (31 downto 0); -- unsure what to do with this
  signal orresult  : std_logic_vector (31 downto 0);
  signal andresult : std_logic_vector (31 downto 0);
  signal bresult   : std_logic_vector (31 downto 0);
  signal aresult   : std_logic_vector (31 downto 0);

  --instruction decoding
  signal reg_op, sub_op, sll_op, srl_op, mul_op, div_op, and_op, or_op, cmp_op : std_logic;
  signal subop_and, sllop_and, srlop_and, mulop_and, divop_and, andop_and, orop_and, cmpop_and : std_logic;

  signal b_notb_mux : std_logic_vector(31 downto 0); -- muxed b output with b not
  signal sub_or_cmp, s_sel : std_logic; --input to b mux
  signal not_inB : std_logic_vector(31 downto 0);


begin  -- schematic
  --
  --   ADD TO THIS SECTION FOR PROJECT PART 1
  --   (add the signals you need above "begin"
  --
  --instruction decoding logic
  eq_r: entity work.equal6 port map(inst => inst(31 downto 26), test => "000000", equal => reg_op);
  eq_sub: entity work.equal6 port map(inst => inst(5 downto 0), test => "100010", equal => sub_op);
  eq_sll: entity work.equal6 port map(inst => inst(5 downto 0), test => "000100", equal => sll_op);
  eq_srl: entity work.equal6 port map(inst => inst(5 downto 0), test => "000010", equal => srl_op);
  eq_mul: entity work.equal6 port map(inst => inst(5 downto 0), test => "011000", equal => mul_op);
  eq_div: entity work.equal6 port map(inst => inst(5 downto 0), test => "011011", equal => div_op);
  eq_and: entity work.equal6 port map(inst => inst(5 downto 0), test => "001101", equal => and_op);
  eq_or:  entity work.equal6 port map(inst => inst(5 downto 0), test => "001111", equal => or_op);
  eq_cmp: entity work.equal6 port map(inst => inst(5 downto 0), test => "001011", equal => cmp_op);
  --and with reg op equal output to ensure that its a valid alu operation
  subop_and <= reg_op and sub_op;
  sllop_and <= reg_op and sll_op;
  srlop_and <= reg_op and srl_op;
  mulop_and <= reg_op and mul_op;
  divop_and <= reg_op and div_op;
  andop_and <= reg_op and and_op;
  orop_and  <= reg_op and or_op;
  cmpop_and <= reg_op and cmp_op;

  --other cntrl signals
  sub_or_cmp <= subop_and or cmpop_and; --input to Bnot multiplexer
  s_sel <= sllop_and or srlop_and; --input to output mux to control shift output

  --B multiplexer
  not_inB <= not(inB);
  muxB: entity WORK.mux_32 port map(in0 => inB, in1 => not_inB, ctl => sub_or_cmp, result => b_notb_mux);


  --setting operation outputs to mux32_6
  orresult <= inA or inB; -- bitwise or operation
  andresult <= inA and inB; -- bitwise and operation

  adder: entity WORK.add32 port map(a    => inA,
                                    b    => b_notb_mux,  -- fixed for subtract
                                    cin  => subop_and,  -- fixed for subtract
                                    sum  => aresult,    --this was just result beforehand fixed now
                                    cout => cout);      -- unsure if this should change

  Mul: entity WORK.pmul16 port map(a => inA(15 downto 0),
                                   b => inB(15 downto 0),
                                   p => mulresult(31 downto 0));

  Div16: entity WORK.divcas16 port map(dividend => inA,
                                       divisor => inB(15 downto 0),
                                       quotient => divresult(15 downto 0),
                                       remainder => divrem(15 downto 0));

  shifter: entity WORK.bshift port map(left => sllop_and,
                                       logical => '1',
                                       shift => inst(10 downto 6),
                                       input => inB,
                                       output => bresult);

  -- output multiplexer
  Res_mux: entity WORK.mux32_6 port map (in0 => aresult,
                                         in1 => orresult,
                                         in2 => andresult,
                                         in3 => mulresult,
                                         in4 => divresult,
                                         in5 => bresult,
                                         ct1 => orop_and,
                                         ct2 => andop_and,
                                         ct3 => mulop_and,
                                         ct4 => divop_and,
                                         ct5 => s_sel,
                                         result => result);

end architecture schematic;  -- of alu_32

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Entity:      staller
--Created by:  Brendan Cain (added for part3b)
--Description: Computes whether there is a stall using the 4 stall cases
--             stall_lw, stall_lwlw, stall_mem, stall_beq
library IEEE;
use IEEE.std_logic_1164.all;

entity staller is
  port(exop    : in std_logic_vector(5 downto 0);
       exrd    : in std_logic_vector(4 downto 0);
       idreg_1 : in std_logic_vector(4 downto 0);
       idreg_2 : in std_logic_vector(4 downto 0);
       idop    : in std_logic_vector(5 downto 0);
       memrd   : in std_logic_vector(4 downto 0);
       memop   : in std_logic_vector(5 downto 0);
       stall   : out std_logic);
end entity staller;

architecture schematic of staller is
  signal zero_5   : std_logic_vector(4 downto 0) := (others=>'0');   -- 5 bits of zero
  signal lw       : std_logic_vector(5 downto 0) := "100011";        -- lw op code
  signal addi     : std_logic_vector(5 downto 0) := "001100";        -- addi op code
  signal jmp      : std_logic_vector(5 downto 0) := "000010";        -- jmp op code
  signal beq      : std_logic_vector(5 downto 0) := "011101";        -- beq op code

  signal stall_lw, stall_lwlw, stall_mem, stall_beq : std_logic := '0';  --used to store logical output from each case
  signal exop_lw, exrd_0, memrd_0, reg1_exrd, reg2_exrd, reg1_memrd   : std_logic; --equality outputs
  signal idop_lw, idop_addi, idop_jmp, idop_beq, memop_lw, reg2_memrd : std_logic; --equality outputs

begin
  --equalities used in logical equations to compute stall cases
  exop_eq_lw:     entity WORK.equal6 port map(exop, lw, exop_lw);
  exrd_eq_0:      entity WORK.equal5 port map(exrd, zero_5, exrd_0);
  reg1_eq_exrd:   entity WORK.equal5 port map(idreg_1, exrd, reg1_exrd);
  reg2_eq_exrd:   entity WORK.equal5 port map(idreg_2, exrd, reg2_exrd);
  idop_eq_lw:     entity WORK.equal6 port map(idop, lw, idop_lw);
  idop_eq_addi:   entity WORK.equal6 port map(idop, addi, idop_addi);
  idop_eq_jmp:    entity WORK.equal6 port map(idop, jmp, idop_jmp);
  idop_eq_beq:    entity WORK.equal6 port map(idop,  beq, idop_beq);
  memrd_eq_0 :    entity WORK.equal5 port map(memrd, zero_5, memrd_0);
  memop_eq_lw:    entity WORK.equal6 port map(memop, lw, memop_lw);
  reg1_eq_memrd:  entity WORK.equal5 port map(idreg_1, memrd, reg1_memrd);
  reg2_eq_memrd:  entity WORK.equal5 port map(idreg_2, memrd, reg2_memrd);

  --stall cases
  stall_lw    <= exop_lw and not exrd_0 and (reg1_exrd or reg2_exrd) and not idop_lw and not idop_addi and not idop_jmp;
  stall_lwlw  <= exop_lw and not exrd_0 and (idop_lw or idop_addi) and reg1_exrd;
  stall_mem   <= idop_beq and not memrd_0 and memop_lw and (reg1_memrd or reg2_memrd);
  stall_beq   <= idop_beq and not exrd_0 and (reg1_exrd or reg2_exrd);
  --stall decision
  stall <= stall_lw or stall_lwlw or stall_mem or stall_beq;

end architecture schematic;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

entity part3b is  -- test bench
end part3b;

library STD;
use STD.textio.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use WORK.util_pkg.all;

architecture schematic of part3b is -- top level connection of entities

  -- signals used in top level architecture (the interconnections)

  subtype word_32 is std_logic_vector(31 downto 0);  -- data and inst
  subtype word_6 is std_logic_vector(5 downto 0);    -- op codes
  subtype word_5 is std_logic_vector(4 downto 0);    -- register numbers

  signal zero_32 : word_32 := (others=>'0');     -- 32 bits of zero
  signal zero_5  : word_5  := (others=>'0');     -- 5 bits of zero
  signal zero    : std_logic := '0';             -- one bit zero
  signal four_32 : word_32 := x"00000004";       -- four

  -- from  cs411_opcodes.txt
  signal lwop    : word_6 := "100011";     -- lw op code
  signal swop    : word_6 := "101011";     -- sw op code
  signal RRop    : word_6 := "000000";     -- RR op code
  signal addiop  : word_6 := "001100";     -- addi op code
  signal lwimop  : word_6 := "001111";     -- lwim op code


  signal clear   : std_logic := '1';       -- one shot clear
  signal clk     : std_logic := '1';       -- master clock
  signal clk_bar : std_logic := '0';       -- complement of master clock
  signal counter : integer := 0;           -- master clock counter,raising edge

  signal PC_next        : word_32;            -- next value of PC
  signal PC             : word_32;            -- Program Counter
  signal inst           : word_32;            -- instruction fetched
  --signals used for forwarding during beq or jmp
  signal PCP            : word_32;            -- PCP register (used for forwarding during beq jmp)
  signal old_PC_next    : word_32;            -- next value of PC
  signal jump_addr      : word_32;            -- input address for a jmp command
  signal beq_addr       : word_32;            -- input address for a beq command
  signal do_beq         : std_logic := '0';   -- anded value of IDbeq_op & IDbeq_true
  --signals used for stalling stages
  signal sclk           : std_logic;          -- clk or'd with stall to stall certain stages
  signal need_stall     : std_logic := '0';   -- indicates a stall is needed from staller ored with Imiss
  signal stall_EX       : word_32;            -- output of stall mux
  --signals used for handling cache in instruction memory
  signal stall          : std_logic := '0';   -- indicates stall
  signal IF_miss        : std_logic := '0';   -- instruction fetch miss signal from Memory (stall for 30ns)

  signal ID_IR          : word_32;            -- ID Instruction Register
  alias  ID_reg1 : word_5 is ID_IR(25 downto 21);  -- a register
  alias  ID_reg2 : word_5 is ID_IR(20 downto 16);  -- b register
  alias  ID_OP   : word_6 is ID_IR(31 downto 26);  -- opcode
  signal ID_read_data_1 : word_32;            -- ID Register read data 1
  signal ID_read_data_2 : word_32;            -- ID Register read data 2
  signal ID_sign_ext    : word_32;            -- ID sign extension
  signal RegDst         : std_logic := '0';   -- ID register destination ctl
  signal ID_rd          : word_5;             -- ID register destination
  signal old_ID_rd      : word_5;             -- used for forwarding
  alias  ID_addr        : std_logic_vector(15 downto 0) is ID_IR(15 downto 0);
  signal IDjmp_op       : std_logic := '0';    -- jmp control signal
  signal IDbeq_op       : std_logic := '0';    -- beq control signal
  signal IDsw_op        : std_logic := '0';    -- sw control signal
  signal shifted2       : word_32;             -- shifted address input for beq_addr
  --Data forwarding control signals
  signal idFWD1_1, idFWD1_2, idFWD1_3 : std_logic;  --inputs to and fwd1
  signal idFWD1         : std_logic := '0';          -- and gate output from above signals, fwd data 1 control signal
  signal ID_fwd_data1   : word_32;            --fwd 1 data output
  signal idFWD2_1, idFWD2_2, idFWD2_3 : std_logic;  --inputs to and fwd2
  signal idFWD2         : std_logic;          -- and gate output from above signals, fwd data 2 control signal
  signal ID_fwd_data2   : word_32;            -- fwd 2 data output
  signal ID_fwd_cntrl   : std_logic := '0';          -- used to control ID_rd during a fwd
  signal IDbeq_true     : std_logic;          -- 1 if beq comparison is true otherwise 0

  signal EX_IR          : word_32;            -- EX Instruction Register
  alias  EX_reg1 : word_5 is EX_IR(25 downto 21);  -- a register
  alias  EX_reg2 : word_5 is EX_IR(20 downto 16);  -- b register
  alias  EX_OP   : word_6 is EX_IR(31 downto 26);  -- opcode
  signal EX_A           : word_32;            -- EX data A
  signal EX_B           : word_32;            -- EX data B
  signal EX_C           : word_32;            -- EX data C
  signal EX_rd          : word_5;             -- EX register destination
  signal EX_aluB        : word_32;            -- EX into ALU B
  signal ALUSrc         : std_logic := '0';   -- EX ALU B side source control
  signal ALUSrc_not     : std_logic;          -- above's compliment
  signal EX_result      : word_32;            -- EX ALU output
  --signals used for handling cache in data memory
  signal D_miss         : std_logic := '0';   -- data miss signal (stall 30ns)
  signal clk200         : std_logic := '1';   -- delayed clk (by 200ps)
  signal dsclk          : std_logic := '1';   -- data stalled clk for
  --Data forwarding control signals
  signal exAMFA_1A, exAMFA_2A, exAMFA_3A : std_logic;    --input to and A1
  signal exAMFA_A       : std_logic := '0';          -- and gate output from above signals, mem fwd control signal (A1)
  signal exAFWBR_1A, exAFWBR_2A          : std_logic;    --input to and A2
  signal exAFWBR_A      : std_logic := '0';          -- and gate output from above signals, wb_result fwd control signal (A2)
  signal EX_Afwd        : word_32;            -- data forwarded from A
  signal exAMFA_1B, exAMFA_2B, exAMFA_3B : std_logic;    --input to and B1
  signal exAMFA_B       : std_logic := '0';          -- and gate output from above signals, mem fwd control signal (B1)
  signal exAFWBR_1B, exAFWBR_2B          : std_logic;    --input to and B2
  signal exAFWBR_B      : std_logic := '0';          -- and gate output from above signals, wb_result fwd control signal (B2)
  signal EX_Bfwd        : word_32;            -- data forwarded from B

  signal MEM_IR         : word_32;            -- MEM Inst Register
  signal MEM_addr       : word_32;            -- MEM address
  signal MEM_data       : word_32;            -- MEM write data
  signal MEM_read_data  : word_32;            -- MEM read data
  signal MEM_rd         : word_5;             -- MEM register destination
  signal MEMRead        : std_logic := '0';   -- MEM enable read
  signal MEMWrite       : std_logic := '0';   -- MEM enable write
  alias  MEM_OP  : word_6 is MEM_IR(31 downto 26);

  signal WB_IR          : word_32;            -- WB Instruction Register
  signal WB_read        : word_32;            -- WB read data
  signal WB_pass        : word_32;            -- WB pass data
  signal WB_rd          : word_5;             -- WB register destination
  signal WB_rd_zero     : std_logic;          -- WB_rd is zero, no value
  signal MemtoReg       : std_logic := '1';   -- WB mux control
  signal WB_result      : word_32;            -- WB mux output
  signal WB_write_enb   : std_logic := '1';   -- WB enable register write
  signal WB_lwop        : std_logic;          -- Have a lw in WB stage
  signal WB_lwimop      : std_logic;          -- Have a lwim in WB stage
  signal WB_RRop        : std_logic;          -- WB RR operation
  signal WB_addiop      : std_logic;          -- WB add immediate operation

begin  -- schematic of part3b, top level architecture and test bench

  clock_gen: process(clk, clear)  -- clock generator and one shot clear signal
             begin
               if clear='1' then -- happens only once
                 clear <= '0' after 200 ps;
               elsif clear='0' then     -- avoid time zero glitch
                 clk <= not clk after 5 ns;  -- 10 ns period
               end if;
             end process clock_gen;

             clk_bar <= not clk;               -- for split phase registers

-- IF, Instruction Fetch pipeline stage
  PC_reg:      entity WORK.register_32 port map(sclk, clear, PC_next, PC);
  PC_incr:     entity WORK.add32 port map(PC, four_32, zero, old_PC_next, open);
  inst_mem:    entity WORK.instruction_memory port map(clear, PC, inst, IF_miss);
  --Forwarding logic used during jmp or beq
  PCP_reg:     entity WORK.register_32 port map(sclk, clear, old_PC_next, PCP);
  PCP_incr:    entity WORK.add32 port map(PCP, shifted2, zero, beq_addr, open);
  PCnext_mux:  entity WORK.mux32_3 port map(old_PC_next, jump_addr, beq_addr, IDjmp_op, do_beq, PC_next);
  -- rearrages inputed branch address
  jump_addr <= PCP(31 downto 28) & ID_IR(25 downto 0) & "00";
  do_beq <= IDbeq_op and IDbeq_true;
  --handles sclk which can pause PC_reg, PCP_reg, and ID_IR_reg if a stall is needed
  sclk <= clk or stall;

  HZRD_Stall: entity WORK.staller port map(exop => EX_OP,
                                           exrd => EX_rd,
                                           idreg_1 => ID_reg1,
                                           idreg_2 => ID_reg2,
                                           idop => ID_OP,
                                           memrd => MEM_rd,
                                           memop => MEM_OP,
                                           stall => need_stall);
-- handling miss from cache
  stall <= need_stall or IF_miss or D_miss;

-- ID, Instruction Decode and register stack pipeline stage
  ID_IR_reg: entity WORK.register_32 port map(sclk, clear, inst, ID_IR);
  ID_regs:   entity WORK.registers port map(
                                read_reg_1   => ID_IR(25 downto 21),
                                read_reg_2   => ID_IR(20 downto 16),
                                write_reg    => WB_rd,
                                write_data   => WB_result,
                                write_enable => WB_write_enb,
                                write_clk    => clk_bar,
                                read_data_1  => ID_read_data_1,
                                read_data_2  => ID_read_data_2);
  ID_jmp_op: entity WORK.equal6 port map(ID_IR(31 downto 26), "000010", IDjmp_op);
  ID_beq_op: entity WORK.equal6 port map(ID_IR(31 downto 26), "011101", IDbeq_op);
  ID_sw_op:  entity WORK.equal6 port map(ID_IR(31 downto 26), "101011", IDsw_op);
             -- RegDst <=   must compute
  REG_Dst: entity WORK.equal6 port map(ID_IR(31 downto 26), RRop, RegDst);

  ID_mux_rd: entity WORK.mux_5 port map(in0    => ID_IR(20 downto 16),
                                        in1    => ID_IR(15 downto 11),
                                        ctl    => RegDst,
                                        result => old_ID_rd);
  -- handle ID_rd when forwarding or stalling

  ID_mux_fwd: entity WORK.mux_5 port map(in0    => old_ID_rd,
                                        in1    => zero_5,
                                        ctl    => ID_fwd_cntrl,
                                        result => ID_rd);
  ID_fwd_cntrl <= IDjmp_op or IDbeq_op or IDsw_op or stall;

  ID_sign_ext(15 downto 0) <= ID_addr;  -- just wiring
  ID_sign_ext(31 downto 16) <= (others => ID_IR(15));
  -- shift over 2 for part3b
  shifted2 <= ID_sign_ext(29 downto 0) & "00";
  --compute control signals that will determine fwd for branch if equal
  ID_FWD1_1: entity WORK.equal5 port map(ID_reg1, MEM_rd, idFWD1_1);
  ID_FWD1_2: entity WORK.equal5 port map(MEM_rd, zero_5, idFWD1_2);
  ID_FWD1_3: entity WORK.equal6 port map(MEM_OP, lwop, idFWD1_3);
  idFWD1 <= idFWD1_1 and not idFWD1_2 and not idFWD1_3;
  --forward data1
  ID_FWD_1: entity WORK.mux_32 port map(ID_read_data_1, MEM_addr, idFWD1, ID_fwd_data1);
  --compute control signals that will determine fwd
  ID_FWD2_1: entity WORK.equal5 port map(ID_reg2, MEM_rd, idFWD2_1);
  ID_FWD2_2: entity WORK.equal5 port map(MEM_rd, zero_5, idFWD2_2);
  ID_FWD2_3: entity WORK.equal6 port map(MEM_OP, lwop, idFWD2_3);
  idFWD2 <= idFWD2_1 and not idFWD2_2 and not idFWD2_3;
  --forward data2
  ID_FWD_2: entity WORK.mux_32 port map(ID_read_data_2, MEM_addr, idFWD2, ID_fwd_data2);
  --see if equal and if so then branch (see IF stage)
  ID_FWD_beq: entity WORK.equal32 port map(ID_fwd_data1, ID_fwd_data2, IDbeq_true);
  -- the below entities handle stalling
  ID_STL_mux: entity WORK.mux_32 port map(ID_IR, zero_32, stall, stall_EX); -- gives EX stage nop if stall otherwise ID_IR

-- EX, Execute pipeline stage
  clk200 <= clk after 200 ps; -- slight delay
  dsclk  <= clk200 or D_miss;  -- dmiss out of data_memory

  EX_IR_reg: entity WORK.register_32 port map(dsclk, clear, stall_EX, EX_IR);
  EX_A_reg : entity WORK.register_32 port map(dsclk, clear, ID_read_data_1,EX_A);
  EX_B_reg : entity WORK.register_32 port map(dsclk, clear, ID_read_data_2,EX_B);
  EX_C_reg : entity WORK.register_32 port map(dsclk, clear, ID_sign_ext, EX_C);
  EX_rd_reg: entity WORK.register_5  port map(dsclk, clear, ID_rd, EX_rd);

  ALU_src: entity WORK.equal6 port map(EX_IR(31 downto 26), RRop, ALUSrc_not);
  ALUSrc <= not ALUSrc_not;

  --calculate A forward mem address control
  EX_AFMA_1A: entity WORK.equal5 port map(EX_reg1, MEM_rd, exAMFA_1A);
  EX_AFMA_2A: entity WORK.equal5 port map(MEM_rd, zero_5, exAMFA_2A);
  EX_AFMA_3A: entity WORK.equal6 port map(MEM_OP, lwop, exAMFA_3A);
  exAMFA_A <= exAMFA_1A and not exAMFA_2A and not exAMFA_3A;
  --calculate A wb_result forward control
  EX_AFWBR_1A: entity WORK.equal5 port map(EX_reg1, WB_rd, exAFWBR_1A);
  EX_AFWBR_2A: entity WORK.equal5 port map(WB_rd, zero_5, exAFWBR_2A);
  exAFWBR_A <= exAFWBR_1A and not exAFWBR_2A;
  --forward EX_A Based on above control signals
  EX_A_fwd: entity WORK.mux32_3 port map(EX_A, MEM_addr, WB_result, exAMFA_A, exAFWBR_A, EX_Afwd);

  --calculate A forward mem address control
  EX_AFMA_1B: entity WORK.equal5 port map(EX_reg2, MEM_rd, exAMFA_1B);
  EX_AFMA_2B: entity WORK.equal5 port map(MEM_rd, zero_5, exAMFA_2B);
  EX_AFMA_3B: entity WORK.equal6 port map(MEM_OP, lwop, exAMFA_3B);
  exAMFA_B <= exAMFA_1B and not exAMFA_2B and not exAMFA_3B;
  --calculate A wb_result forward control
  EX_AFWBR_1B: entity WORK.equal5 port map(EX_reg2, WB_rd, exAFWBR_1B);
  EX_AFWBR_2B: entity WORK.equal5 port map(WB_rd, zero_5, exAFWBR_2B);
  exAFWBR_B <= exAFWBR_1B and not exAFWBR_2B;
  --forward EX_A Based on above control signals
  EX_B_fwd: entity WORK.mux32_3 port map(EX_B, MEM_addr, WB_result, exAMFA_B, exAFWBR_B, EX_Bfwd);

  EX_mux1  : entity WORK.mux_32 port map(in0    => EX_Bfwd,
                                         in1    => EX_C,
                                         ctl    => ALUSrc,
                                         result => EX_aluB );
  ALU      : entity WORK.alu_32 port map(inA   => EX_Afwd,
                                         inB   => EX_aluB,
                                         inst  => EX_IR,
                                         result=> EX_result);

-- MEM Data Memory pipeline stage
  MEM_IR_reg  : entity WORK.register_32 port map(dsclk, clear, EX_IR, MEM_IR);
  MEM_addr_reg: entity WORK.register_32 port map(dsclk, clear, EX_result,
                                                 MEM_addr);
  MEM_data_reg: entity WORK.register_32 port map(dsclk, clear, EX_Bfwd, MEM_data);
  MEM_rd_reg  : entity WORK.register_5  port map(dsclk, clear, EX_rd, MEM_rd);

  MEM_lw      : entity WORK.equal6 port map(MEM_IR(31 downto 26), lwop, MEMRead);
  MEM_sw      : entity work.equal6 port map(MEM_IR(31 downto 26), swop, MEMWrite);

  data_mem    : entity WORK.data_memory port map(address     => MEM_addr,
                                                 write_data  => MEM_data,
                                                 read_enable => MEMRead,
                                                 write_enable=> MEMWrite,
                                                 write_clk   => clk_bar,
                                                 clear       => clear,
                                                 read_data   => MEM_read_data,
                                                 miss        => D_miss);

-- WB, Write Back pipeline stage
  WB_IR_reg  : entity WORK.register_32 port map(dsclk, clear, MEM_IR, WB_IR);
  WB_read_reg: entity WORK.register_32 port map(dsclk, clear, MEM_read_data,
                                                WB_read);
  WB_pass_reg: entity WORK.register_32 port map(dsclk, clear, MEM_addr, WB_pass);
  WB_rd_reg  : entity WORK.register_5  port map(dsclk, clear, MEM_rd, WB_rd);
  wblwop     : entity WORK.equal6 port map(WB_IR(31 downto 26), lwop, WB_lwop);
  wblwimop   : entity WORK.equal6 port map(WB_IR(31 downto 26), lwimop, WB_lwimop);
             MemtoReg <= WB_lwop;
  compare_rd : entity WORK.equal5 port map(WB_rd, "00000", WB_rd_zero);
               WB_write_enb <= (not WB_rd_zero) and
                               (WB_lwop or WB_lwimop or WB_RRop or WB_addiop);

  WBrrop: entity WORK.equal6 port map( WB_IR(31 downto 26), RRop, WB_RRop);
  WBaddiop: entity WORK.equal6 port map( WB_IR(31 downto 26), addiop, WB_addiop);
                                  -- and WB_RRop and WB_addiop computed

  WB_mux     : entity WORK.mux_32 port map(in0    => WB_pass,
                                           in1    => WB_read,
                                           ctl    => MemtoReg,
                                           result => WB_result );

 prtstall: process (stall) -- print the time of when a stall
                variable my_line : LINE; -- my_line needs to be defined
              begin
                write(my_line, string'("stall="));
                write(my_line, stall);         -- or hwrite for long signals
                write(my_line, string'("clk="));
                write(my_line, counter);         -- or hwrite for long signals
                write(my_line, string'(" at="));
                write(my_line, now);         -- "now" is simulation time
                writeline(output, my_line);  -- outputs line
              end process prtstall;

  loadmem: process    -- read part3b.abs into shared memory array
             file my_input : TEXT open READ_MODE is "part3b.abs";  -- hex data
             variable good : boolean := true;
             variable my_line : LINE;
             variable my_input_line : LINE;
             variable loc : std_logic_vector(31 downto 0);  -- read from file
             variable val : std_logic_vector(31 downto 0);  -- read from file
             variable word_addr : natural;  -- byte addr/4
           begin
             write(my_line, string'
                   ("---PC--- --inst--  loadmem process input .abs file"));
             writeline(output, my_line);
             while good loop
               exit when endfile(my_input);
               readline(my_input, my_input_line);
               my_line := new string'(my_input_line.all);  -- for printing
               writeline(output, my_line); -- writing clears my_line
               hread(my_input_line, loc, good);
               exit when not good;
               hread(my_input_line, val, good);
               exit when not good;
               word_addr := to_integer(loc(13 downto 2)); -- crop to 12 bits
               memory(word_addr) := val;  -- write main memory
             end loop;
             write(my_line, string'("loadmem ended. memory loaded"));
             writeline(output, my_line);
             wait; -- run once. do not keep restarting process
           end process loadmem;

  printout:  process -- used to show pipeline, registers and memory
               variable my_line : LINE;   -- not part of working circuit
             begin
               wait for 9.5 ns;         -- just before rising clock
               write(my_line, string'("clock "));
               write(my_line, counter);
               write(my_line, string'("  inst="));
               hwrite(my_line, inst);
               write(my_line, string'("  PC   ="));
               hwrite(my_line, PC);
               write(my_line, string'(" PCnext="));
               hwrite(my_line, PC_next);
               writeline(output, my_line);
               write(my_line, string'("ID  stage  IR="));
               hwrite(my_line, ID_IR);
               if (WB_write_enb='1') and (WB_rd/="00000") then
                 write(my_line, string'("  write="));
                 hwrite(my_line, WB_result);
                 write(my_line, string'("  into ="));
                 hwrite(my_line, "000000000000000000000000000"&WB_rd);
               else
                 write(my_line, string'("                "));
                 write(my_line, string'("                "));
               end if;
               write(my_line, string'("                "));
               write(my_line, string'(" rd="));
               write(my_line, ID_rd);
               writeline(output, my_line);

               write(my_line, string'("EX  stage  IR="));
               hwrite(my_line, EX_IR);
               write(my_line, string'("  EX_A ="));
               hwrite(my_line, EX_A);
               write(my_line, string'("  EX_B ="));
               hwrite(my_line, EX_B);
               write(my_line, string'("  EX_C ="));
               hwrite(my_line, EX_C);
               write(my_line, string'(" rd="));
               write(my_line, EX_rd);
               writeline(output, my_line);
               write(my_line, string'("EX  stage"));
               write(my_line, string'("             "));
               write(my_line, string'("EX_aluB="));
               hwrite(my_line, EX_aluB);
               write(my_line, string'(" EX_res="));
               hwrite(my_line, EX_result);
               writeline(output, my_line);
               write(my_line, string'("MEM stage  IR="));
               hwrite(my_line, MEM_IR);
               write(my_line, string'("  addr ="));
               hwrite(my_line, MEM_addr);
               write(my_line, string'("  data ="));
               hwrite(my_line, MEM_data);
               if MEMread='1' then
                 write(my_line, string'("  read ="));
                 hwrite(my_line, MEM_read_data);
               elsif MEMWrite='1' then
                 write(my_line, string'("  wrote="));
                 hwrite(my_line, MEM_data);
               else
                 write(my_line, string'("                "));
               end if;
               write(my_line, string'(" rd="));
               write(my_line, MEM_rd);
               writeline(output, my_line);
               write(my_line, string'("WB  stage  IR="));
               hwrite(my_line, WB_IR);
               write(my_line, string'("  read ="));
               hwrite(my_line, WB_read);
               write(my_line, string'("  pass ="));
               hwrite(my_line, WB_pass);
               write(my_line, string'(" result="));
               hwrite(my_line, WB_result);
               write(my_line, string'(" rd="));
               write(my_line, WB_rd);
               writeline(output, my_line);
               write(my_line, string'("control RegDst="));
               write(my_line, RegDst);
               write(my_line, string'("  ALUSrc="));
               write(my_line, ALUSrc);
               write(my_line, string'("  MemtoReg="));
               write(my_line, MemtoReg);
               write(my_line, string'("  MEMRead="));
               write(my_line, MEMRead);
               write(my_line, string'("  MEMWrite="));
               write(my_line, MEMWrite);
               write(my_line, string'("  WB_write_enb="));
               write(my_line, WB_write_enb);
               writeline(output, my_line);

               -- registers
               write(my_line, string'("reg 0-7 "));
               for I in 0 to 7 loop
                 hwrite(my_line, reg_mem(I));
                 write(my_line, string'(" "));
               end loop;  -- I
               writeline(output, my_line);
               write(my_line, string'("   8-15 "));
               for I in 8 to 15 loop
                 hwrite(my_line, reg_mem(I));
                 write(my_line, string'(" "));
               end loop;  -- I
               writeline(output, my_line);
               write(my_line, string'("  16-23 "));
               for I in 16 to 23 loop
                 hwrite(my_line, reg_mem(I));
                 write(my_line, string'(" "));
               end loop;  -- I
               writeline(output, my_line);

               -- RAM memory
               write(my_line, string'("RAM 70- "));
               for I in 28 to 35 loop  -- word at hex 70 byte address
                 hwrite(my_line, memory(I));
                 write(my_line, string'(" "));
               end loop;
               writeline(output, my_line);

               writeline(output, my_line);  -- blank line
               counter <= counter+1;
               wait for 0.5 ns;         -- rest of 10 ns clock period
             end process printout;

end architecture schematic; -- of part3b
