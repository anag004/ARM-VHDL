----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04/23/2019 08:06:25 PM
-- Design Name: 
-- Module Name: multi_display - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity multi_display is
  Port (clock: in std_logic;
        d0, d1, d2, d3: in std_logic_vector(3 downto 0);
        enw0, enw1, enw2, enw3: in std_logic;
        anode: out std_logic_vector(3 downto 0);
        A, B, C, D, E, F, G: out std_logic
        );
end multi_display;

architecture Behavioral of multi_display is
    signal counter: integer := 0;
    signal disp_digit: std_logic_vector(3 downto 0) := "0000";
begin
    proc_counter: process(clock) begin
        if (rising_edge(clock)) then 
            if (counter = 3) then 
                counter <= 0;
            else
                counter <= counter +1;
            end if;
        end if;
    end process;
    
    basys:
        ENTITY WORK.basys_display (Behavioral)
        PORT MAP(
            a_in => disp_digit,
            A => A,
            B => B,
            C => C,
            D => D,
            E => E,
            F => F,
            G => G
        );
        
    process(counter, enw0, enw1, enw2, enw3, d0, d1, d2, d3) begin
        case counter is
            when 0 =>
                anode <= "111" & (not enw0);
                disp_digit <= d0;
            when 1 =>
                anode <= "11" & (not enw1) & '1';
                disp_digit <= d1;
            when 2 =>
                anode <= "1" & (not enw2) & "11";
                disp_digit <= d2;
            when 3 =>
                anode <= (not enw3) & "111";
                disp_digit <= d3;
            when others =>
                anode <= "0000";
                disp_digit <= "1111";
        end case;
    end process;
end Behavioral;
