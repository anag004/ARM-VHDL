----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09/27/2018 04:55:17 PM
-- Design Name: 
-- Module Name: debounce - Behavioral
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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity debounce is
  Port (insig, clk: in std_logic;
        outsig: out std_logic 
       );
end debounce;

architecture Behavioral of debounce is
    signal ctr: std_logic_vector(3 downto 0) := "0000"; 
begin
    process(clk)
    begin
        if rising_edge(clk) then
            ctr <= ctr + 1;
            if (ctr = "1111") then
                    outsig <= insig;
               end if;
        end if;
        
        
    end process;
end Behavioral;

