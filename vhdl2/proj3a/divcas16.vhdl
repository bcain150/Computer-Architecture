-- divcas16.vhdl  parallel division  divident32/divsor16 computes
--                                   quotient16  remainder16

library IEEE;
use IEEE.std_logic_1164.all;

entity cas is  -- Controlled Add/Subtract cell
  port (
    divisor       : in  std_logic;
    T             : in  std_logic;
    remainder_in  : in  std_logic;
    cin           : in  std_logic;
    remainder_out : out std_logic;
    cout          : out std_logic);
end entity cas;

architecture circuits of cas is
  signal tt : std_logic;
begin  -- circuits of cas
  tt            <= T xor divisor after 1 ps;
  remainder_out <= tt xor remainder_in xor cin after 1 ps;
  cout          <= (tt and remainder_in) or (tt and cin) or
                   (remainder_in and cin) after 1 ps;
end architecture circuits;  -- of cas

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_1164.all;

entity divcas16 is   -- 32 bit dividend, 16 bit divisor
  port (
    dividend  : in  std_logic_vector(31 downto 0);
    divisor   : in  std_logic_vector(15 downto 0);
    quotient  : out std_logic_vector(15 downto 0);
    remainder : out std_logic_vector(15 downto 0));
end entity divcas16;

architecture circuits of divcas16 is
  constant N : integer := 15; -- high index
  constant NP : integer := N+1; -- number of bits
  constant NM : integer := N-1; -- loop counts
  
  subtype row is std_logic_vector(N downto 0);
  type mat is array(N downto 0) of row;
  signal T : std_logic_vector(N downto 0);
  signal r, c : mat;
  signal rr, cr : std_logic_vector(N downto 0); -- for remainder correction
begin  -- circuits of divcas16
  -- dividend(31) assumed zero and unused
  -- "T" is sub_add in non-restoring divide
  
  idiv: for I in N downto N generate
      jdiv: for J in N downto 1 generate
         casN: entity WORK.cas port map(
          divisor(J), T(I), dividend(I+J), c(I)(J-1), r(I)(J), c(I)(J)); 
      end generate jdiv; 
      casNN: entity WORK.cas port map(
             divisor(0), T(I), dividend(I), T(I), r(I)(0), c(I)(0)); 
  end generate idiv;

  iidiv: for I in NM downto 1 generate
      ijdiv: for J in N downto 1 generate
         casij: entity WORK.cas port map(
               divisor(J), T(I), r(I+1)(J-1), c(I)(J-1), r(I)(J), c(I)(J)); 
      end generate ijdiv; 
      casi0: entity WORK.cas port map(
             divisor(0), T(I), dividend(I), T(I), r(I)(0), c(I)(0)); 
  end generate iidiv;

  i0div: for I in 0 downto 0 generate
      j0div: for J in N downto 1 generate
         cas0: entity WORK.cas port map(
               divisor(J), T(I), r(I+1)(J-1), c(I)(J-1), r(I)(J), c(I)(J)); 
      end generate j0div; 
      cas00: entity WORK.cas port map(
             divisor(0), T(I), dividend(I), T(I),     r(0)(0), c(0)(0));
  end generate i0div;

  T(N) <= '1';
  itdiv: for I in NM downto 0 generate
    T(I) <= not r(I+1)(N) after 1 ps;
    quotient(I+1)  <= T(I);
  end generate itdiv;
  quotient(0)  <= not r(0)(N) after 1 ps;
  -- correct remainder
  jrdiv: for J in N downto 1 generate
    casrj: entity WORK.cas port map(
           divisor(J), '0', r(0)(J), cr(J-1), rr(J), cr(J)); 
  end generate jrdiv;
  casrr0: entity WORK.cas port map(
          divisor(0), '0', r(0)(0), '0', rr(0), cr(0));
  remainder <= rr after 1 ps when r(0)(N)='1' else r(0) after 1 ps;
end architecture circuits; -- of divcas16
