
-- Engineer: 
-- 
-- Create Date: 09/27/2018 05:04:53 PM
-- Design Name: 
-- Module Name: clockgen - Behavioral
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

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity clockgen is
  Port (reset, rawclk: in std_logic;
        outclk: out std_logic
     );
end clockgen;

architecture Behavioral of clockgen is
    signal tmpclk: std_logic := '0';
    signal counter, div: integer := 0;
begin
--    div <= 100000;
    div <= 1000;
    process(rawclk, reset) begin
        if (reset = '1') then 
            tmpclk <= '0'; 
            counter <= 0; 
        end if;
        
        if (rising_edge(rawclk)) then
            if (counter < div/2 - 1) then      
                counter <= counter + 1;
                tmpclk <= '0';
            elsif (counter < div - 1) then
                counter <= counter + 1;
                tmpclk <= '1';
            else 
                counter <= 0;
                tmpclk <= '0';
            end if;
        end if;
    end process;
    
    outclk <= tmpclk;
end Behavioral;

