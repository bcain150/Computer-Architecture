-- add32.vhdl    for section 0301

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
  s <= a xor b xor cin after 2 ns;
  cout <= (a and b) or (a and cin) or (b and cin) after 2 ns;
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

