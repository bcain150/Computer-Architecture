-- add32pg.vhdl    start of HW4
-- pg4.vhdl    entity and architecture
--             Carry-Lookahead unit p246 of textbook
--             pg4 is driven by four add4pg entities
--      EDITED BY: BRENDAN CAIN

library IEEE;
use IEEE.std_logic_1164.all;
entity pg4 is 
  port(p0   : in  std_logic;
       p1   : in  std_logic;
       p2   : in  std_logic; 
       p3   : in  std_logic;
       g0   : in  std_logic;
       g1   : in  std_logic;
       g2   : in  std_logic; 
       g3   : in  std_logic;
       cin  : in  std_logic;
       c1   : out std_logic;
       c2   : out std_logic;
       c3   : out std_logic;
       c4   : out std_logic);
end entity pg4 ;

architecture circuits of pg4 is
begin  -- circuits of pg4
  c1   <= g0 or (p0 and cin) after 2 ps;
  c2   <= g1 or (p1 and g0) or (p1 and p0 and cin) after 2 ps;
  c3   <= g2 or (p2 and g1) or (p2 and p1 and g0) or
          (p2 and p1 and p0 and cin) after 2 ps;
  c4   <= g3 or
          (p3 and g2) or
          (p3 and p2 and g1) or
          (p3 and p2 and p1 and g0) or
          (p3 and p2 and p1 and p0 and cin) after 2 ps;
end architecture circuits;  -- of pg4

library IEEE;
use IEEE.std_logic_1164.all;
entity add4pg is
  port(a    : in  std_logic_vector(3 downto 0);
       b    : in  std_logic_vector(3 downto 0);
       cin  : in  std_logic; 
       sum  : out std_logic_vector(3 downto 0);
       p    : out std_logic;
       g    : out std_logic );
end entity add4pg ;

architecture circuits of add4pg is
  signal c : std_logic_vector(2 downto 0);
begin  -- circuits of add4pg
  sum(0) <= a(0) xor b(0) xor cin after 2 ps;
  c(0)   <= (a(0) and b(0)) or (a(0) and cin) or (b(0) and cin) after 2 ps;
  sum(1) <= a(1) xor b(1) xor c(0) after 2 ps;
  c(1)   <= (a(1) and b(1)) or
            (a(1) and a(0) and b(0)) or
            (a(1) and a(0) and cin)  or
            (a(1) and b(0) and cin)  or
            (b(1) and a(0) and b(0)) or
            (b(1) and a(0) and cin)  or
            (b(1) and b(0) and cin) after 2 ps;
  sum(2) <= a(2) xor b(2) xor c(1) after 2 ps;
  c(2)   <= (a(2) and b(2)) or (a(2) and c(1)) or (b(2) and c(1)) after 2 ps;
  sum(3) <= a(3) xor b(3) xor c(2) after 2 ps;
  p      <= (a(0) or b(0)) and (a(1) or b(1)) and
            (a(2) or b(2)) and (a(3) or b(3)) after 2 ps;
  g      <= (a(3) and b(3)) or ((a(3) or b(3)) and
            ((a(2) and b(2)) or ((a(2) or b(2)) and
            ((a(1) and b(1)) or ((a(1) or b(1)) and
            ((a(0) and b(0)))))))) after 2 ps;
end architecture circuits;  -- of add4pg


-- ENTITY: add32
-- uses add4pg and pg4

library IEEE;
use IEEE.std_logic_1164.all;
entity add32 is
  port(a    : in  std_logic_vector(31 downto 0);
       b    : in  std_logic_vector(31 downto 0);
       cin  : in  std_logic; 
       sum  : out std_logic_vector(31 downto 0);
       cout : out std_logic);
end entity add32;

architecture circuits of add32 is
  signal P0, P1, P2, P3, P4, P5, P6, P7: std_logic;
  
  signal C1, C2, C3, C4, C5, C6, C7: std_logic;
  signal G0, G1, G2, G3, G4, G5, G6, G7: std_logic;

begin
  a01: entity WORK.add4pg port map(a(3 downto 0),
                                   b(3 downto 0),
                                   cin,
                                   sum(3 downto 0),
                                   P0,
                                   G0);

  a02: entity WORK.add4pg port map(a(7 downto 4),
                                   b(7 downto 4),
                                   C1,
                                   sum(7 downto 4),
                                   P1,
                                   G1);

  a03: entity WORK.add4pg port map(a(11 downto 8), b(11 downto 8), C2, sum(11 downto 8), P2, G2);
  a04: entity WORK.add4pg port map(a(15 downto 12), b(15 downto 12), C3, sum(15 downto 12), P3, G3);
    
  pg1: entity WORK.pg4 port map(P0, P1, P2, P3,
                                G0, G1, G2, G3,
                                cin,
                                C1, C2, C3, C4);

  a05: entity WORK.add4pg port map(a(19 downto 16), b(19 downto 16), C4, sum(19 downto 16), P4, G4);
  a06: entity WORK.add4pg port map(a(23 downto 20), b(23 downto 20), C5, sum(23 downto 20), P5, G5);
  a07: entity WORK.add4pg port map(a(27 downto 24), b(27 downto 24), C6, sum(27 downto 24), P6, G6);
  a08: entity WORK.add4pg port map(a(31 downto 28), b(31 downto 28), C7, sum(31 downto 28), P7, G7);

  pg2: entity WORK.pg4 port map(P4, P5, P6, P7,
                                G4, G5, G6, G7,
                                C4,
                                C5, C6, C7, cout);
  
end architecture circuits; -- of add32
