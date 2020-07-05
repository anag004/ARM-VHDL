----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04/24/2019 11:34:43 PM
-- Design Name: 
-- Module Name: my_bus - Behavioral
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
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.ALL;
use work.common.all;

entity my_bus is
  Port (bus_addr: in std_logic_vector(31 downto 0);
        bus_value_in: in std_logic_vector(31 downto 0);
        bus_value_out: out std_logic_vector(31 downto 0);
        bus_anode_out, bus_digit_out: out std_logic_vector(3 downto 0);
        enw_bus, mode: in std_logic;
        enw_digit, enw_anode, enw_DM: out std_logic;
        bus_anode_in, bus_digit_in: in std_logic_vector(3 downto 0);
        bus_DM_in: in std_logic_vector(31 downto 0);
        bus_DM_addr: out std_logic_vector(9 downto 0);
        bus_DM_out: out std_logic_vector(31 downto 0);
        bus_cols_in: in std_logic_vector(3 downto 0);
        enw_cols: out std_logic;
        bus_cols_out: out std_logic_vector(3 downto 0);
        bus_rows_in: in std_logic_vector(3 downto 0)
       );
end my_bus;

architecture Behavioral of my_bus is

begin
    read_proc: process(bus_addr, bus_anode_in, bus_digit_in, bus_DM_in, mode, bus_value_in, enw_bus, bus_rows_in) begin
        bus_DM_addr <= bus_addr(11 downto 2);
        enw_anode <= '0'; enw_DM <= '0'; enw_digit <= '0'; enw_cols <= '0';
        bus_cols_out <= bus_value_in(3 downto 0);
        bus_digit_out <= bus_value_in(3 downto 0); bus_anode_out <= bus_value_in(3 downto 0);
        bus_DM_out <= bus_value_in;
        if (bus_addr = "00000000000000000000000000000000") then
            -- Read the values from anode, if mode is supervisor, enable writing
            if (mode = '1') then enw_anode <= enw_bus; end if;
            bus_value_out <= "0000000000000000000000000000" & bus_anode_in;
        elsif (bus_addr = "00000000000000000000000000000100") then
            -- Read the values from the digit
            if (mode = '1') then enw_digit <= enw_bus; end if;
            bus_value_out <= "0000000000000000000000000000" & bus_digit_in;
        elsif (bus_addr = "00000000000000000000000000011100") then 
            -- Column i/o addr
            if (mode = '1') then enw_cols <= enw_bus; end if;
            bus_value_out <= "0000000000000000000000000000" & bus_cols_in;
        elsif (bus_addr = "00000000000000000000000000100000") then
            bus_value_out <= "0000000000000000000000000000" & bus_rows_in;
        else
            if (to_integer(unsigned(bus_addr)) > 1023 or mode = '1') then enw_DM <= enw_bus; end if;
            bus_value_out <= bus_DM_in; 
        end if;
    end process;
end Behavioral;
