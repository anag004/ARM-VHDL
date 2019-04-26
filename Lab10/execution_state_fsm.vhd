
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.common.all;

entity execution_state_fsm is
    Port(
        clock, reset, step, go, instr: in std_logic;
        control_state_in: in control_state;
        state_out: out execution_state
    );
end execution_state_fsm;

architecture Behavioral of execution_state_fsm is
    signal state: execution_state := initial;
    signal is_red: std_logic := '0'; 
begin    
    process(clock, reset) begin
        if (reset = '1') then
            state <= initial;
        elsif rising_edge(clock) then
            case state is
                when initial =>
                    if (step = '0' and instr = '0' and go = '0') then
                        state <= initial;
                    elsif (step = '1') then
                        state <= onestep;
                    elsif (instr = '1') then
                        state <= oneinstr;
                    elsif (go = '1') then
                        state <= cont;
                    else
                        state <= initial;
                    end if;
                when onestep =>
                    state <= done;
                when oneinstr =>
                    if (is_red = '1') then
                        state <= done;
                    elsif (is_red = '0') then
                        state <= oneinstr;
                    else   
                        state <= initial;
                    end if;
                when cont =>
                    if (control_state_in = halt) then
                        state <= done;
                    else 
                        state <= cont;
                    end if;
                when done =>
                    if (step ='1' or instr = '1' or go = '1') then
                        state <= done;
                    else 
                        state <= initial;
                    end if;
            end case;
        end if;
    end process;
    
    red_check: process(control_state_in) begin
        case control_state_in is
            when brn => is_red <= '1';
            when halt => is_red <= '1';
            when mem_wr => is_red <= '1';
            when res2RF => is_red <= '1';
            when mem2RF => is_red <= '1';
	   when mul_res2RF => is_red <= '1';
            when others => is_red <= '0';
        end case;
    end process;
    
    state_out <= state;
end architecture;
