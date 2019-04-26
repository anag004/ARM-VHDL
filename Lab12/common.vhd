----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/18/2019 02:01:05 PM
-- Design Name: 
-- Module Name: common - Behavioral
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

package common is
    type instr_class_type is (DP, DT, branch, halt, unknown, DTSH, MULT, PSR); 
    type i_decoded_type is (addi, addr,subi, subr, cmpi, cmpr, movi, movr, ldrp, ldrs, strp, strs, beq , bne, b, halt, unknown,
                            pandi, pandr, eori, eorr, rsbi, rsbr, adci, adcr, sbci, sbcr, rsci, rscr, tsti, tstr, teqi, teqr, 
                            cmni, cmnr, orri, orrr, bici, bicr, mvni, mvnr, strp_reg, strs_reg, ldrp_reg, ldrs_reg, strsh_imm, strsh_reg,
                            ldrsh_imm, ldrsh_reg, mul, mla, umull, umlal, smull, smlal, mrs, msr);
    type execution_state is (initial, onestep, oneinstr, cont, done);
    type control_state is (fetch, decode, arith, addr, brn, halt, mem_rd, mem2RF, mem_wr, res2RF, mul_res2RF, mult, add1, add2, psr, skip, undef_exc, swi_exc, rst_exc, irq_exc); 
end common;

package body common is
    
end common;
