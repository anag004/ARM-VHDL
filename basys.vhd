library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity basys_display is
    PORT(a_in: IN std_logic_vector(3 downto 0);
         A, B, C, D, E, F, G: OUT std_logic);
end basys_display;

architecture Behavioral of basys_display is
    signal tmp: std_logic_vector(15 downto 0);
begin
    dec:
        entity WORK.dec4_16 (Behavioral)
        PORT MAP(
            s => a_in, 
            y => tmp
        );
        
     A <= tmp(1) or tmp(4) or tmp(11) or tmp(13);
     B <= tmp(5) or tmp(6) or tmp(11) or tmp(12) or tmp(14) or tmp(15);
     C <= tmp(2) or tmp(12) or tmp(14) or tmp(15);
     D <= tmp(15) or tmp(10) or tmp(7) or tmp(4) or tmp(1);
     E <= tmp(1) or tmp(3) or tmp(4) or tmp(5) or tmp(7) or tmp(9);
     F <= tmp(1) or tmp(2) or tmp(3) or tmp(7) or tmp(13);
     G <= tmp(1) or tmp(0) or tmp(7) or tmp(12);
end Behavioral;
         