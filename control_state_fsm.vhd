----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/18/2019 02:36:11 PM
-- Design Name: 
-- Module Name: control_state_fsm - Behavioral
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
use work.common.all;    

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity control_state_fsm is
  Port (clock, reset, pred_bit: in std_logic; 
        execution_state_in: in execution_state;
        instr_class_in: in instr_class_type;
        i_decoded_in: in i_decoded_type;
        state_out: out control_state; 
        rst_signal, irq_signal, swi_signal, m, i: in std_logic
        );
end control_state_fsm;

architecture Behavioral of control_state_fsm is
    signal state: control_state := fetch;
    signal is_green: std_logic := '0';
begin
    process(clock, reset) begin
        if (reset = '1') then
            state <= fetch;
        elsif (rising_edge(clock)) and is_green = '1' then
            case state is
                when fetch =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    else state <= decode; end if;
                when decode =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (swi_signal = '1' and m = '0') then state <= swi_exc;
                    elsif (i_decoded_in = unknown) then state <= undef_exc;
		            elsif (pred_bit = '0') then
			             state <= skip;
                    elsif (instr_class_in = DP) then
                        state <= arith;
                    elsif (instr_class_in = DT or instr_class_in = DTSH) then
                        state <= addr;
                    elsif (instr_class_in = branch) then
                        state <= brn;
                    elsif (instr_class_in = halt) then
                        state <= halt;
                    elsif (instr_class_in = PSR) then
                        state <= psr;
		            elsif (instr_class_in = MULT) then
	                  state <= mult;
                    else 
                        state <= fetch;
                    end if;
                when arith => 
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    else state <= res2RF; end if;
                when addr =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (i_decoded_in = strp or i_decoded_in = strs or i_decoded_in = strs_reg or i_decoded_in = strp_reg or i_decoded_in = strsh_reg or i_decoded_in = strsh_imm) then
                        state <= mem_wr;
                    elsif (i_decoded_in = ldrp or i_decoded_in = ldrs or i_decoded_in = ldrs_reg or i_decoded_in = ldrp_reg or i_decoded_in = ldrsh_imm or i_decoded_in = ldrsh_reg)  then
                        state <= mem_rd;
                    end if;
                when mem_rd =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    else state <= mem2RF; end if;
                when brn =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (irq_signal = '1' and i = '0') then state <= irq_exc;
                    else state <= fetch; end if;
                when halt =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (irq_signal = '1' and i = '0') then state <= irq_exc;
                    else state <= fetch; end if;
                when mem_wr =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (irq_signal = '1' and i = '0') then state <= irq_exc;
                    else state <= fetch; end if;
                when mem2RF =>
                   if (rst_signal = '1' and m = '0') then state <= rst_exc;
                   elsif (irq_signal = '1' and i = '0') then state <= irq_exc;
                   else state <= fetch; end if;
                when res2RF =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (irq_signal = '1' and i = '0') then state <= irq_exc;
                    else state <= fetch; end if;
		        when mult =>
		            if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    else state <= add1; end if;
                when add1 =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    else state <= add2; end if;
                when add2 =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    else state <= mul_res2RF; end if;
                when rst_exc => state <= fetch;
                when irq_exc => state <= fetch;
                when undef_exc => state <= fetch;
                when swi_exc => state <= fetch;
                when others =>
                    if (rst_signal = '1' and m = '0') then state <= rst_exc;
                    elsif (irq_signal = '1' and i = '0') then state <= irq_exc;
                    else state <= fetch;
                    end if;
            end case;
        end if;
    end process;
    
    check_green: process(execution_state_in) begin
        if (execution_state_in = onestep or execution_state_in = oneinstr or execution_state_in = cont) then
            is_green <= '1';
        else
            is_green <= '0';
        end if;
    end process;
    
    state_out <= state;
end Behavioral;
