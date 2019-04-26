----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/25/2019 04:12:16 PM
-- Design Name: 
-- Module Name: shift_module - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity shift_module is
  Port (shft_type: in std_logic_vector(1 downto 0);
        shft_amt: integer;
        op_in: in std_logic_vector(31 downto 0);
        op_out: out std_logic_vector(31 downto 0);
        carry: out std_logic);
end shift_module;

architecture Behavioral of shift_module is
    
begin
    process(shft_amt, shft_type, op_in) begin
        if (shft_amt /= 0) then
            case shft_type is 
                when "00" =>
                    -- Logical left
                    op_out <= std_logic_vector(shift_left(unsigned(op_in), shft_amt));
                    carry <= op_in(31-shft_amt);
                when "01" =>
                    -- Logical right
                    op_out <= std_logic_vector(shift_right(unsigned(op_in), shft_amt));
                    carry <= op_in(shft_amt-1);
                when "10" =>
                    -- Arithmetic shift right
                    op_out <= std_logic_vector(shift_right(signed(op_in), shft_amt));
                    carry <= op_in(shft_amt-1);
                when "11" =>
                    -- rotate right
                    op_out <= std_logic_vector(rotate_right(unsigned(op_in), shft_amt));
                    carry <= op_in(shft_amt-1);
                when others =>  
                    op_out <= "00000000000000000000000000000000";
                    carry <= '0';
            end case;
        else 
            op_out <= op_in;
            carry <= '0';
        end if;
    end process;

end Behavioral;
