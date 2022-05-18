--HW5 cmsc 411
-- created by brendan cain

-- HW5 part 1
library IEEE;
use IEEE.std_logic_1164.all;
entity hw5_1 is 
  port(a : in std_logic;
       b : in std_logic;
       c : in std_logic;
       x : out std_logic;
       y : out std_logic);
end entity hw5_1;

architecture circuits of hw5_1 is
begin 
  x <= (not(a) and b and not(c)) or
       (not(a) and b and c) or
       (a and b and not(c)) after 1 ns;

  y <= (not(a) and b and c) or
       (a and not(b) and not(c)) or
       (a and not(b) and c) after 1 ns;
end architecture circuits;


-- HW5 part 2
library IEEE;
use IEEE.std_logic_1164.all;
entity hw5_2 is 
  port(a : in std_logic;
       b : in std_logic;
       c : in std_logic;
       d : in std_logic;
       e : in std_logic;
       f : in std_logic;
       g : out std_logic);
end entity hw5_2;

architecture circuits of hw5_2 is
begin
  
  g <= ((a or b) xor (c and d) xor not(e)) or f after 1 ns;
  
end architecture circuits;

-- HW5 Part 3
-- See PNG file titled HW5_3.png

--HW5 Part 4
-- a) the sum should be 111100 with s5 being the msb and s0 being the lsb
-- b) the time for the longest path from the input to the output would be 14T
-- this is because each adder has to wait for the adder before it (who's
-- outputs are going into the second adder's inputs) to complete before the
-- current adder can begin processing its inputs. ANSWER: 14T

--HW5 Part 5
-- a) the 6 bit result is still 111100 with s5 being the msb and s0 being the lsb
-- b) the time for the longest path from input to output would be 12T this time
-- only because each level of the adder is happening at the same time (the
-- input time only for each level only depends on the ouput time for the level
-- above it) There is one exception to this however. The last level must
-- combine all carries. Therefore, the input start time is depended on the
-- completion of the adder in the lsb location. level 1 and 2 complete in 2T
-- time and 4T time respectively but then level 3 is responsible for rippling
-- the carry and so that will add 2T time per adder leaving the output for the
-- longest path at 12T time. ANSWER: 12T
