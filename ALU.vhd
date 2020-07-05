----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/18/2019 01:38:14 PM
-- Design Name: 
-- Module Name: ALU - Behavioral
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

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ALU is
  Port (op1, op2: in std_logic_vector(31 downto 0); 
        flag_write, clock, reset, shift_carry, cin: in std_logic;
        optype: in i_decoded_type;
        shft_amt: integer;
        result_out: out std_logic_vector(31 downto 0);
        z_out, c_out, v_out, n_out: out std_logic;
	    is_mult, Z_override: in std_logic;
	    -- Registers to load the saved status into the flags
         n_in, z_in, v_in, c_in, ld_status: in std_logic
       );
end ALU;

architecture Behavioral of ALU is
    signal is_logic: std_logic := '0';
    signal tmp_z, tmp_c, tmp_n, tmp_v, c31, c32, z, n, c, v: std_logic := '0';
    signal tmp_op1, tmp_op2, tmp_cin: integer := 0;
    signal result: integer := 0;
    signal result_tmp: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
begin
    clocked_process: process(clock, reset) begin
        if (ld_status = '1') then
                  -- Load registers
                  n <= n_in;
                  v <= v_in;
                  z <= z_in;
                  c <= c_in;
        elsif (reset = '1') then
            z <= '0';
        elsif rising_edge(clock) then
          if (flag_write = '1') then
                if (Z_override = '1') then z <= tmp_z; end if;
                if (is_logic = '1') then 
                    if shft_amt /= 0 then c <= shift_carry; 
                    else --do nothing
                    end if;
                else c <= tmp_c; end if;
                if (is_logic = '0' and is_mult = '0') then v <= tmp_v; end if;
                n <= tmp_n;
            end if;
        end if;
    end process;
    
    z_out <= z; c_out <= c; n_out <= n; v_out <= v;
    tmp_op1 <= to_integer(unsigned(op1));
    tmp_op2 <= to_integer(unsigned(op2));   
    tmp_cin <= 1 when (cin = '1') else 0;
    
    compute_result: process(op1, op2, optype, tmp_op1, tmp_op2, tmp_cin, c) begin
        is_logic <= '0';
        if (optype = pandi or optype = pandr) then
            result <= to_integer(unsigned(op1 and op2));
            is_logic <= '1';
        elsif (optype = eori or optype = eorr) then
            is_logic <= '1';
            result <= to_integer(unsigned(op1 xor op2));
        elsif (optype = subi or optype = subr) then
            result <= tmp_op1 - tmp_op2;
        elsif (optype = rsbi or optype = rsbr) then
            result <= to_integer(unsigned(not op1)) + tmp_op2 + 1;
        elsif (optype = addi or optype = addr) then
            result <= tmp_op1 + tmp_op2 + tmp_cin;
        elsif (optype = adci or optype = adcr) then
            if (c = '1') then
                result <= tmp_op1 + tmp_op2 + 1;
            else 
                result <= tmp_op1 + tmp_op2;
            end if;
        elsif (optype = sbci or optype = sbcr) then
            if (c = '1') then
                result <= tmp_op1 + to_integer(unsigned(not op2)) + 1;
            else 
                result <= tmp_op1 + to_integer(unsigned(not op2));
            end if;
        elsif (optype = rsci or optype = rscr) then
            if (c = '1') then
                result <= to_integer(unsigned(not op1)) + tmp_op2 + 1;
            else 
                result <= to_integer(unsigned(not op1)) + tmp_op2;
            end if;
        elsif (optype = tsti or optype = tstr) then
            is_logic <= '1';
            result <= to_integer(unsigned(op1 and op2));
        elsif (optype = teqi or optype = teqr) then
            is_logic <= '1';
            result <= to_integer(unsigned(op1 xor op2));
        elsif (optype = cmpi or optype = cmpr) then
            result <= tmp_op1 + to_integer(unsigned(not op2)) + 1;
         elsif (optype = cmni or optype = cmnr) then
            result <= tmp_op1 + tmp_op2;
        elsif (optype = orrr or optype = orri) then
            is_logic <= '1';
            result <= to_integer(unsigned(op1 or op2));
        elsif optype = movi or optype = movr then
            is_logic <= '1';
            result <= tmp_op2;
        elsif optype = bici or optype = bicr then
            is_logic <= '1';
            result <= to_integer(unsigned(op1 and (not op2)));
        elsif optype = mvni or optype = mvnr then
            is_logic <= '1';
            result <= to_integer(unsigned(not op2));
        else 
            result <= 0;
        end if;
    end process;
    
    result_tmp <= std_logic_vector(to_unsigned(result, 32));
    
    process(result_tmp, optype, op1, op2) begin
        if (optype = subi or optype = subr or optype = sbci or optype = sbcr or optype = cmpi or optype = cmpr) then
            c31 <= op1(31) xor (not op2(31))  xor result_tmp(31);
        elsif (optype = rsbi or optype = rsbr or optype = rsci or optype = rscr) then
            c31 <= (not op1(31)) xor op2(31) xor result_tmp(31);
        else
            c31 <= op1(31) xor op2(31) xor result_tmp(31);
        end if;
    end process;
    
    process(result_tmp, c31, optype, op1, op2) begin
        if (optype = subi or optype = subr or optype = sbci or optype = sbcr or optype = cmpi or optype = cmpr) then
            c32 <= (op1(31) and (not op2(31))) or (op1(31) and c31) or ((not op2(31)) and c31); 
        elsif (optype = rsbi or optype = rsbr or optype = rsci or optype = rscr) then
            c32 <= ((not op1(31)) and op2(31)) or ((not op1(31)) and c31) or (op2(31) and c31);
        else
            c32 <= (op1(31) and op2(31)) or (op1(31) and c31) or (op2(31) and c31);
        end if;
    end process;
    
    flag_assign: process(result_tmp, c31, c32) begin
        if (result_tmp = "00000000000000000000000000000000") then tmp_z <= '1'; else tmp_z <= '0'; end if;
        tmp_n <= result_tmp(31);
        tmp_v <= c31 xor c32;
        tmp_c <= c32;
    end process;
    result_out <= result_tmp;
end Behavioral;
