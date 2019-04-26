----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02/18/2019 01:52:38 PM
-- Design Name: 
-- Module Name: instr_decoder - Behavioral
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

entity instr_decoder is
  Port (ins_value: in std_logic_vector(31 downto 0);
        instr_class_out: out instr_class_type;
        i_decoded_out:  out i_decoded_type
        );
end instr_decoder;

Architecture Behavioral of instr_decoder is
    -- Signal storing the condition type
	signal cond : std_logic_vector (3 downto 0) := "0000"; 
	-- Storing the F value
	signal F_field, opc : std_logic_vector (1 downto 0) := "00"; 
	-- Store the I bit value
	signal I_bit, L_bit, U_bit, S_bit, P_bit, B_bit, W_bit: std_logic := '0';
	-- Store the shift specification
	signal shift_spec : std_logic_vector (7 downto 0) := "00000000";
	-- Store the rotation specification
	signal rot_spec: std_logic_vector(3 downto 0) := "0000";
	-- Store the immediates based on instruction types
	signal Immconv: std_logic_vector(31 downto 0) := "00000000000000000000000000000000"; -- Extending the immediate to 32 bits
	signal Imm24: std_logic_vector(23 downto 0) := "000000000000000000000000";
	signal Imm8: std_logic_vector(7 downto 0) := "00000000";
	signal Imm12: std_logic_vector(11 downto 0) := "000000000000";
	signal opcode: std_logic_vector(3 downto 0) := "0000";
	signal instr_class: instr_class_type := DP;
	signal i_decoded: i_decoded_type := addi;
	signal SH: std_logic_vector(1 downto 0);
begin
    -- Assign a value to instr_class
	instr_class_assign : process(ins_value, cond, S_bit, opcode, SH) begin
		case ins_value(27 downto 26) is
			when "00" => 
			    if (ins_value = "00000000000000000000000000000000") then
                    instr_class <= halt;
                elsif (cond = "1110") then
		    if (ins_value(25) = '0' and ins_value(7) = '1' and ins_value(4) = '1') then
	                        if (SH = "00") then 
	                            instr_class <= MULT;
	                        else
	                            instr_class <= DTSH;
	                        end if;
                    else
                        instr_class <= DP;
                    end if;
                else
                    instr_class <= unknown;
                end if;
			when "01" =>
			    if (cond = "1110") then
				    instr_class <= DT;
                else
                    instr_class <= unknown;
                end if;
			when "10" =>
				instr_class <= branch;
			when others =>
				instr_class <= unknown;
		end case;
	end process;
	
    -- Assign a value to the immediates
    signal_assign: process(ins_value) begin
        Imm8 <= ins_value(7 downto 0);
        Imm12 <= ins_value(11 downto 0);
        Imm24 <= ins_value(23 downto 0);

        I_bit <= ins_value(25);
        U_bit <= ins_value(23);
        L_bit <= ins_value(20);
        S_bit <= ins_value(20);
        B_bit <= ins_value(22);
        P_bit <= ins_value(24);
        W_bit <= ins_value(21);
        shift_spec <= ins_value(11 downto 4);
        rot_spec <= ins_value(11 downto 8);
        SH <= ins_value(6 downto 5);
        opc <= ins_value(25 downto 24);
        cond <= ins_value(31 downto 28);
        opcode <= ins_value(24 downto 21);
        F_field <= ins_value(27 downto 26);
    end process;
    
    -- Determine the instruction type from the stored variables
        i_decoded_assign: process(instr_class, ins_value, SH, W_bit, B_bit, P_bit, S_bit, U_bit, L_bit, cond, opcode, I_bit, rot_spec, shift_spec, opc) begin
            case instr_class is 
                when DP =>
                    case opcode is
                        when "0000"=>
                            if I_bit = '1' then i_decoded <= pandi;
                            elsif I_bit = '0' then i_decoded <= pandr;
                            else i_decoded <= unknown;
                            end if;
                        when "0001"=>
                            if I_bit = '1' then i_decoded <= eori;
                            elsif I_bit = '0' then i_decoded <= eorr;
                            else i_decoded <= unknown;
                            end if;
                        when "0010"=>
                            if I_bit = '1' then i_decoded <= subi;
                            elsif I_bit = '0' then i_decoded <= subr;
                            else i_decoded <= unknown;
                            end if;
                        when "0011"=>
                            if I_bit = '1' then i_decoded <= rsbi;
                            elsif I_bit = '0' then i_decoded <= rsbr;
                            else i_decoded <= unknown;
                            end if;
                        when "0100"=>
                            if I_bit = '1' then i_decoded <= addi;
                            elsif I_bit = '0' then i_decoded <= addr;
                            else i_decoded <= unknown;
                            end if;
                        when "0101"=>
                            if I_bit = '1' then i_decoded <= adci;
                            elsif I_bit = '0' then i_decoded <= adcr;
                            else i_decoded <= unknown;
                            end if;
                        when "0110"=>
                            if I_bit = '1' then i_decoded <= sbci;
                            elsif I_bit = '0' then i_decoded <= sbcr;
                            else i_decoded <= unknown;
                            end if;
                        when "0111"=>
                            if I_bit = '1' then i_decoded <= rsci;
                            elsif I_bit = '0' then i_decoded <= rscr;
                            else i_decoded <= unknown;
                            end if;
                        when "1000"=>
                            if I_bit = '1' and S_bit = '1' then i_decoded <= tsti;
                            elsif I_bit = '0' and S_bit = '1' then i_decoded <= tstr;
                            else i_decoded <= unknown;
                            end if;
                        when "1001"=>
                            if I_bit = '1' and  S_bit = '1' then i_decoded <= teqi;
                            elsif I_bit = '0' and S_bit = '1' then i_decoded <= teqr;
                            else i_decoded <= unknown;
                            end if;
                        when "1010"=>
                            if I_bit = '1' and S_bit = '1' then i_decoded <= cmpi;
                            elsif I_bit = '0' and S_bit = '1' then i_decoded <= cmpr;
                            else i_decoded <= unknown;
                            end if;
                        when "1011"=>
                            if I_bit = '1' and S_bit = '1' then i_decoded <= cmni;
                            elsif I_bit = '0' and S_bit = '1' then i_decoded <= cmnr;
                            else i_decoded <= unknown;
                            end if;
                        when "1100"=>
                            if I_bit = '1' then i_decoded <= orri;
                            elsif I_bit = '0' then i_decoded <= orrr;
                            else i_decoded <= unknown;
                            end if;
                        when "1101"=>
                            if I_bit = '1' then i_decoded <= movi;
                            elsif I_bit = '0' then i_decoded <= movr;
                            else i_decoded <= unknown;
                            end if;
                        when "1110"=>
                            if I_bit = '1' then i_decoded <= bici;
                            elsif I_bit = '0' then i_decoded <= bicr;
                            else i_decoded <= unknown;
                            end if;
                        when "1111"=>
                            if I_bit = '1' then i_decoded <= mvni;
                            elsif I_bit = '0' then i_decoded <= mvnr;
                            else i_decoded <= unknown;
                            end if;
                        when others =>
                            i_decoded <= unknown; 
                    end case;
                when DT =>
                    if (P_bit /= '0' or (P_bit = '0' and W_bit = '0')) then
                        if (L_bit = '0' and U_bit = '1' and cond="1110" and I_bit='0') then i_decoded <= strp;
                        elsif (L_bit = '1' and U_bit = '1' and cond="1110" and I_bit='0') then i_decoded <= ldrp;
                        elsif (L_bit = '0' and U_bit = '0' and cond="1110" and I_bit='0') then i_decoded <= strs;
                        elsif (L_bit = '1' and U_bit = '0' and cond="1110" and I_bit='0') then i_decoded <= ldrs;
                        elsif (L_bit = '0' and U_bit = '1' and cond="1110" and I_bit = '1') then i_decoded <= strp_reg;
                        elsif (L_bit = '0' and U_bit = '0' and cond="1110" and I_bit = '1') then i_decoded <= strs_reg;
                        elsif (L_bit = '1' and U_bit = '1' and cond="1110" and I_bit = '1') then i_decoded <= ldrp_reg;
                        elsif (L_bit = '1' and U_bit = '0' and cond="1110" and I_bit = '1') then i_decoded <= ldrs_reg;
                        else i_decoded <= unknown;
                        end if;
                    else
                        i_decoded <= unknown;
                    end if;
                when DTSH =>
                    if (L_bit /= '0' or (L_bit = '0' and SH="01")) then
                        -- When we are strsh type, the signed bit should be zero
                        if (ins_value(22) = '0' and L_bit = '0' and SH(1) = '0' and rot_spec = "0000") then i_decoded <= strsh_reg;
                        elsif (ins_value(22) = '0' and L_bit = '1' and rot_spec ="0000") then i_decoded <= ldrsh_reg;
                        elsif (ins_value(22) = '1' and L_bit = '0' and SH(1) = '0') then i_decoded <= strsh_imm;
                        elsif (ins_value(22) = '1' and L_bit = '1') then i_decoded <= ldrsh_imm;
                        else i_decoded <= unknown;
                        end if;
                    else
                        i_decoded <= unknown;
                    end if;
                when branch =>
                    case cond is
                        when "1110" => 
                           if(opc="10") then
                              i_decoded <= b;
                            else 
                              i_decoded<=unknown;
                            end if;
                        when "0000" =>
                            if(opc="10") then
                                  i_decoded <= beq;
                            else 
                                  i_decoded<=unknown;
                            end if;
                        when "0001" =>
                             if(opc="10") then
                                   i_decoded <= bne;
                            else 
                                i_decoded<=unknown;
                            end if;
                        when others =>
                            i_decoded <= unknown;
                    end case;
		 when MULT =>
	                    case opcode is 
	                        when "0001" => i_decoded <= mla;
	                        when "0000" => i_decoded <= mul;
	                        when "0100" => i_decoded <= umull;
	                        when "0101" => i_decoded <= umlal;
	                        when "0110" => i_decoded <= smull;
	                        when "0111" => i_decoded <= smlal;
	                        when others => i_decoded <= unknown;
	                    end case;
                when halt =>
                    i_decoded <= halt;
                when others =>
                    i_decoded <= unknown;
            end case;
        end process;

        instr_class_out <= instr_class;
        i_decoded_out <= i_decoded;
end Behavioral;
