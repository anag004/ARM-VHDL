library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.ALL;
use work.common.all;

library UNISIM;
use UNISIM.VComponents.all;

entity CPU is
	Port(
         clock, reset, step, go, instr: in std_logic;
	     rst_signal, irq_signal: in std_logic;
		 ins_addr_trunc: out std_logic_vector(9 downto 0); -- instruction address fed to the program memory (truncated to 8 bits)
		 ins_value: in std_logic_vector(31 downto 0); -- instruction value from program memory
		 ad_out: out std_logic_vector(31 downto 0); -- Memory address fed to the data memory (truncated to 8 bits)
		 wd_out, pc_out: out std_logic_vector(31 downto 0); -- Data to write to data memory
		 rd_in: in std_logic_vector(31 downto 0); -- Data fed from memory
		 enw_DM, iack: out std_logic; -- Write-enable to data memory
		 pselect: in std_logic_vector(2 downto 0);
		 rad3: in std_logic_vector(3 downto 0);
		 rd3: out std_logic_vector(31 downto 0);
		 state_out: out std_logic_vector(6 downto 0);
		 A_out, B_out, DR_out, RES_out, IR_out: out std_logic_vector(31 downto 0);
		 n_in,v_in,z_in,c_in,m_out, enw_m_out, enw_RF_out: out std_logic;
		 wad_RF_out: out std_logic_vector(3 downto 0);
		 wd_RF_out, r14svc_out: out std_logic_vector(31 downto 0)
		 
		);
end CPU;

Architecture Behavioral of CPU is
	-- Signal storing the instruction class
	signal instr_class: instr_class_type := DP;
	-- Signal storing instruction subclass
	signal i_decoded : i_decoded_type := addi;
	-- Store the immediates based on instruction types
	signal Immconv: std_logic_vector(31 downto 0) := "00000000000000000000000000000000"; -- Extending the immediate to 32 bits
	signal Imm24: std_logic_vector(23 downto 0) := "000000000000000000000000";
	signal Imm8: std_logic_vector(7 downto 0) := "00000000";
	signal Imm12: std_logic_vector(11 downto 0) := "000000000000";
	signal op1, op2: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal optype: i_decoded_type;
	signal swi_signal: std_logic;
	signal shift_carry: std_logic;
	signal shft_type: std_logic_vector(1 downto 0);
	signal shift_spec: std_logic_vector(7 downto 0);
	signal shft_op_in, shift_out: std_logic_vector(31 downto 0);
	signal shft_amt: integer;
	-- Store the register numbers
	signal Rn, Rd, Rm, Rs: std_logic_vector(3 downto 0) := "0000";
	signal rad1, rad2, wad_RF: std_logic_vector(3 downto 0) := "0000";
	signal rd1, rd2: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal wd_RF: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal pc, next_pc: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal z, n, c, v, enw_RF, flag_write, enw_PC: std_logic := '0';
	-- The ins_addr and ad_out_addr 32 bit values
	signal ins_addr: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal execution_state_sig: execution_state := initial;
	signal control_state_sig: control_state := fetch;
	signal is_green: std_logic := '0';
	signal IR_reg, DR_reg, A_reg, B_reg, RES_reg: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal result: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
	signal enw_IR, enw_DR, enw_A, enw_B, enw_RES, cin: std_logic := '0';
	signal x1, x2: std_logic;
	signal op1_33, op2_33: std_logic_vector(32 downto 0);
	signal result_66, result_66_tmp: std_logic_vector(65 downto 0);
	signal enw_result_66, is_mult, Z_override, pred_bit: std_logic;
	-- Signals to change the saved and current mode of processor
    signal m_in, m_svd, m_svd_in, m, enw_m: std_logic;
    signal change_mode, enw_change_mode, change_mode_in: std_logic;
    -- Signals to change the i bit in a processor
    signal i_in, i, i_svd_in, i_svd, enw_i: std_logic;
    -- Saved flag registers
    signal n_curr_in, z_curr_in, v_curr_in, c_curr_in: std_logic;
    signal n_svd_in, z_svd_in, c_svd_in, v_svd_in: std_logic;
    signal n_svd, z_svd, c_svd, v_svd: std_logic;
    signal ld_status, st_status: std_logic;
    signal r14svc_in, r14_svc: std_logic_vector(31 downto 0);
    signal enw_r14svc: std_logic;
begin
    -- By default, change mode is 1
    process(reset, clock) begin
        if (reset = '1') then
            change_mode <= '1';
        elsif (rising_edge(clock)) then
            if (enw_change_mode = '1') then
                change_mode <= change_mode_in;
            end if;
        end if;
    end process;
    
    -- Enable actions according to execution_state_fsm
    is_green_assign: process(execution_state_sig) begin
        if (execution_state_sig = onestep or execution_state_sig = cont or execution_state_sig = oneinstr) then
            is_green <= '1';
        else
            is_green <= '0';
        end if;
    end process;

	-- Declare the register file
	register_file:
		entity work.registers(Behavioral)
		PORT MAP(
			rad1 => rad1,
			rad2 => rad2,
			rad3 => rad3,
			wd => wd_RF,
			wad => wad_RF,
			clock => clock,
			reset => reset,
			rd1 => rd1,
			rd2 => rd2,
			rd3 => rd3,
			enw_RF => enw_RF,
			enw_PC => enw_PC,
			pselect => pselect,
			pc_in => next_pc,
			pc_out => pc,
			is_green => is_green,
			m_out => m,
			enw_m => enw_m,
			m_in => m_in
		);

		-- Define the ALU
    ALU:
        entity work.ALU(Behavioral)
            PORT MAP(
                op1 => op1,
                op2 => op2,
                result_out => result,
                z_out => z,
                c_out => c,
                v_out => v,
                n_out => n,
                clock => clock,
                reset => reset,
                flag_write => flag_write,
                optype => optype,
                shift_carry => shift_carry,
                cin => cin,
                shft_amt => shft_amt,
		        is_mult => is_mult, 
		        Z_override => Z_override,
		        n_in => n_curr_in,
		        z_in => z_curr_in,
		        c_in => c_curr_in,
		        v_in => v_curr_in,
		        ld_status => ld_status
            );
            
    shift_module:
        entity work.shift_module(Behavioral)
            PORT MAP(
                shft_type => shft_type,
                shft_amt => shft_amt,
                op_in => shft_op_in,
                op_out => shift_out,
                carry => shift_carry
            );   

    -- Replaces the commented code block above
    execution_state_fsm:
        entity work.execution_state_fsm(Behavioral)
        PORT MAP(
            clock => clock,
            reset => reset,
            step => step,
            go => go,
            instr => instr,
            control_state_in => control_state_sig,
            state_out => execution_state_sig
        );

    -- Store the state in saved registers when interrupt seen
    store_in_reg: process(clock, reset) begin
        if (reset = '1') then
            n_svd <= '0';
            z_svd <= '0';
            c_svd <= '0';
            v_svd <= '0';
        elsif (rising_edge(clock)) then
            if (st_status = '1') then
                n_svd <= n_svd_in;
                z_svd <= z_svd_in;
                c_svd <= c_svd_in;
                v_svd <= v_svd_in;
                m_svd <= m_svd_in;
                i_svd <= i_svd_in;
            end if;
        end if;
    end process;
    
    ld_from_reg: process(clock, reset) begin
        if (reset = '1') then
            i <= '0';
        elsif (enw_i = '1') then
            i <= i_in;
        end if;
    end process;
    
    r14_svc_assign: process(clock, reset) begin
        if (reset = '1') then
            r14_svc <= "00000000000000000000000000000000";
        elsif (rising_edge(clock) and enw_r14svc = '1') then
            r14_svc <= r14svc_in;
        end if;
    end process;
    
	-- Assign registers Rn, Rd, Rm their values
	process(IR_reg, instr_class) begin
	   if (instr_class = MULT) then
	       Rd <= IR_reg(19 downto 16);
	       Rn <= IR_reg(15 downto 12);
       else 
           Rd <= IR_reg(15 downto 12);
           Rn <= IR_reg(19 downto 16);
       end if;
            Rm <= IR_reg(3 downto 0);
            Rs <= IR_reg(11 downto 8);
            shift_spec <= IR_reg(11 downto 4);
	end process;

	-- Assign a value to the immediates
	signal_assign: process(IR_reg) begin
		Imm8 <= IR_reg(7 downto 0);
		Imm12 <= IR_reg(11 downto 0);
		Imm24 <= IR_reg(23 downto 0);
	end process;

    instr_decoder:
        entity work.instr_decoder(Behavioral)
            PORT MAP(
                ins_value => IR_reg,
                i_decoded_out => i_decoded,
                instr_class_out => instr_class,
                swi_signal => swi_signal,
                mode => m
            );

    control_state_fsm:
        entity work.control_state_fsm(Behavioral)
            PORT MAP(
		pred_bit => pred_bit,
                clock => clock,
                reset => reset,
                execution_state_in => execution_state_sig,
                state_out => control_state_sig,
                instr_class_in => instr_class,
                i_decoded_in => i_decoded,
                rst_signal => rst_signal,
                irq_signal => irq_signal,
                swi_signal => swi_signal,
                m => m,
                i => i
            );

	-- Get the value of Immconv (Imm24 extended to 32 bits)
	Immconv_assign: process(Imm24) begin
		-- Copy the leftmost bit of Imm24 six times
		Immconv <= Imm24(23) & Imm24(23) & Imm24(23) & Imm24(23) & Imm24(23) & Imm24(23) & Imm24 & "00";
	end process;

	    predication: process(z, n, c, v, IR_reg) begin
	        case IR_reg(31 downto 28) is
	            when "0000" => pred_bit <= z;
	            when "0001" => pred_bit <= (not z);
	            when "0010" => pred_bit <= c;
	            when "0011" => pred_bit <= (not c);
	            when "0100" => pred_bit <= n;
	            when "0101" => pred_bit <= (not n);
	            when "0110" => pred_bit <= v;
	            when "0111" => pred_bit <= (not v);
	            when "1000" => pred_bit <= c and (not z);
	            when "1001" => pred_bit <= (not c) or (z);
	            when "1010" => pred_bit <= (not (n xor v));
	            when "1011" => pred_bit <= (n xor v);
	            when "1100" => pred_bit <= (not z) and (not (n xor v)); 
	            when "1101" => pred_bit <= z or (n xor v);
	            when "1110" => pred_bit <= '1';
	            when others => pred_bit <= '0';
	        end case;
	    end process;


	-- Define the instructions
	exec_instr: process(z, n, c, v, shift_out, Imm12, z, rd_in, control_state_sig, instr_class, i_decoded, pc, result, Rn, Rm, Rd, RES_reg, IR_reg, B_reg, A_reg, Imm8, Immconv, Imm24, DR_reg, shift_spec, op1_33, op2_33, result_66, rst_signal, swi_signal, irq_signal, m, i) begin
		enw_IR <= '0';
		op1 <= "00000000000000000000000000000000";
		op2 <= "00000000000000000000000000000000";
		iack <= '0';
		cin <= '0';
		enw_change_mode <= '0'; change_mode_in <= '0';
		enw_PC <= '0'; is_mult <= '0';
		Z_override <= '1';
		enw_A <= '0'; enw_B <= '0';
		rad1 <= "0000"; rad2 <= "0000";
		enw_RES <= '0'; enw_DM <= '0';
		ad_out <= RES_reg; -- Address to the memory
		flag_write <= '0';
		wd_out <= A_reg; -- Data written to memory is contained in Rn
		wad_RF <= Rd; 
		wd_RF <= "00000000000000000000000000000000";
		next_pc <= "00000000000000000000000000000000";
		enw_DR <= '0';
		enw_RF <= '0';
		optype <= addi;
		enw_result_66 <= '0';
		st_status <= '0';
		ld_status <= '0';
		m_svd_in <= '1';
		i_svd_in <= '1';
		n_svd_in <= '0'; n_curr_in <= '0';
		z_svd_in <= '0'; z_curr_in <= '0';
		c_svd_in <= '0'; c_curr_in <= '0';
		v_svd_in <= '0'; v_curr_in <= '0';
		enw_r14svc <= '0'; r14svc_in <= "00000000000000000000000000000000";
		shft_type <= "00"; shft_amt <= 0; shft_op_in <= "00000000000000000000000000000000";
        enw_m <= '0'; enw_i <= '0'; m_in <= '0'; i_in <= '0';
        case control_state_sig is
            when fetch =>
                    -- Write ins_value into IR
                    enw_IR <= '1';
                    -- Add 4 to PC
                    op1 <= pc;
                    op2 <= "00000000000000000000000000000100";
                    optype <= addi;
                    next_pc <= result;
                    enw_PC <= '1';

            when decode =>
              
                    -- Assign values to the registers, A, B
                    enw_A <= '1'; enw_B <= '1';
                    -- Read values from the RF
                    if (instr_class = DP or instr_class = PSR) then
                        rad1 <= Rn; rad2 <= Rm;
                    elsif (instr_class = MULT) then
                            rad1 <= Rs; rad2 <= Rm;
                    elsif (instr_class = DT or instr_class = DTSH) then
                        rad1 <= Rm; rad2 <= Rn;
                    else
                        rad1 <= Rd; rad2 <= Rn;
                    end if;
              
                -- If the instrucition is of type strs_reg, strp_reg, etc. get the value of Rm in C_reg
            when arith =>
               
                    if (IR_reg(25) = '1') then
                        -- operand 2 is an immediate
                        op1 <= A_reg; op2 <= shift_out;
                        shft_type <= "11";
                        shft_amt <= 2*to_integer(unsigned(IR_reg(11 downto 8)));
                        shft_op_in <= "000000000000000000000000" & Imm8;
                        optype <= i_decoded;
                        enw_RES <= '1'; 
                        flag_write <= IR_reg(20);
                    else
                        op1 <= A_reg; shft_op_in <= B_reg;
                        optype <= i_decoded;
                        shft_type <= shift_spec(2 downto 1); 
                        shft_amt <= to_integer(unsigned(shift_spec(7 downto 3)));
                        op2 <= shift_out; flag_write <= IR_reg(20);
                        enw_RES <= '1';
                    end if;
         
            when addr =>
               
                    -- Do address calculation, include write_back functionality
                    case i_decoded is
                        when strp =>
                            -- RES = A + ex(Imm12)
                            op1 <= B_reg; op2 <= "00000000000000000000" & Imm12;
                            optype <= addi;
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when strs =>
                            -- RES = A - ex(Imm12)
                            op1 <= B_reg; op2 <= "00000000000000000000" & Imm12; 
                            optype <= subi;
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when ldrp =>
                            -- RES = A + ex(Imm12)
                            op1 <= B_reg; op2 <= "00000000000000000000" & Imm12; 
                            optype <= addi;
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when ldrs =>
                            -- RES = A - ex(Imm12)
                            op1 <= B_reg; op2 <= "00000000000000000000" & Imm12; 
                            optype <= subi;
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when strp_reg =>
                            op1 <= B_reg; op2 <= shift_out; optype <= addi;
                            shft_op_in <= A_reg; -- Stores Rm
                            shft_amt <= to_integer(unsigned(IR_reg(11 downto 7)));
                            shft_type <= IR_reg(6 downto 5);
                            enw_RES <= '1';
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when strs_reg =>
                            op1 <= B_reg; op2 <= shift_out; optype <= subi;
                            shft_op_in <= A_reg; -- Stores Rm
                            shft_type <= IR_reg(6 downto 5);
                            shft_amt <= to_integer(unsigned(IR_reg(11 downto 7)));
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if;        
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';                 
                        when ldrp_reg =>
                            op1 <= B_reg; op2 <= shift_out; optype <= addi;
                            shft_op_in <= A_reg; -- Stores Rm
                            shft_type <= IR_reg(6 downto 5);
                            shft_amt <= to_integer(unsigned(IR_reg(11 downto 7)));
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when ldrs_reg =>
                            shft_type <= IR_reg(6 downto 5);
                            shft_op_in <= A_reg; -- Stores Rm
                            op1 <= B_reg; op2 <= shift_out; optype <= subi;
                            shft_amt <= to_integer(unsigned(IR_reg(11 downto 7)));
                            enw_RES <= '1';
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        -- The SH instructions
                        when strsh_imm =>
                            -- RES = A + ex(ins[11..8] & ins[3..0])
                            op1 <= B_reg; op2 <= "000000000000000000000000" & IR_reg(11 downto 8) & IR_reg(3 downto 0);
                            enw_RES <= '1';
                            if (IR_reg(23) = '1') then optype <= addi; else optype <= subi; end if;
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when strsh_reg =>
                            -- RES = A + ex(shft(Rm))
                            op1 <= B_reg; op2 <= shift_out; 
                            shft_amt <= to_integer(unsigned(IR_reg(11 downto 7)));  
                            shft_type <= IR_reg(6 downto 5);
                            shft_op_in <= A_reg; -- stores Rm
                            enw_RES <= '1';
                            if (IR_reg(23) = '1') then optype <= addi; else optype <= subi; end if;
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when ldrsh_imm =>
                            -- RES = A + ex(ins[11..8] & ins[3..0])
                            op1 <= B_reg; op2 <= "000000000000000000000000" & IR_reg(11 downto 8) & IR_reg(3 downto 0);
                            optype <= addi;
                            enw_RES <= '1';
                            if (IR_reg(23) = '1') then optype <= addi; else optype <= subi; end if;
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if; 
                            
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when ldrsh_reg =>
                            -- RES = A + ex(shft(Rm))
                            op1 <= B_reg; op2 <= shift_out; 
                            shft_amt <= to_integer(unsigned(IR_reg(11 downto 7)));  
                            shft_type <= IR_reg(6 downto 5);
                            shft_op_in <= A_reg; -- stores Rm
                            enw_RES <= '1';
                            if (IR_reg(23) = '1') then optype <= addi; else optype <= subi; end if;
                            
                            -- Set the option for write_back
                            if (IR_reg(21) = '1' or IR_reg(24) = '0') then 
                                enw_RF <= '1';
                                -- Write back into Rn
                                wad_RF <= Rn; wd_RF <= result;
                            end if;
                             
                            -- Write the value of Rd into A_reg
                            rad1 <= Rd; enw_A <= '1';
                        when others =>
                            -- do nothing
                    end case;

                when res2RF =>
                        if (m = '0' and IR_reg = "11100001101000001111000000001110") then
                            next_pc <= RES_reg; enw_PC <= '1';
                        elsif (m = '1' and IR_reg = "11100001101100001111000000001110") then
                            -- Return
                            ld_status <= '1';
                            n_curr_in <= n_svd;
                            z_curr_in <= z_svd;
                            c_curr_in <= c_svd;
                            v_curr_in <= v_svd;
                            m_in <= '0'; enw_m <= change_mode;
                            i_in <= '0'; enw_i <= '1';
                            enw_change_mode <= '1'; change_mode_in <= '1';
                            -- Return to value in supervisor LR
                            enw_PC <= '1'; next_pc <= r14_svc;
                        else 
                            -- Store the value of RES in the appropriate registerc
                            if (i_decoded = cmpi or i_decoded = cmpr) then
                                enw_RF <= '0';
                            else
                                enw_RF <= '1';
                            end if;
                            wad_RF <= Rd; wd_RF <= RES_reg;
                        end if;
                when mem_wr =>

                        -- Write to memory, check the type of storage
                        enw_DM <= '1';
                        if (instr_class = DT) then
                            -- strs_imm/reg, strp_imm/reg
                            if (IR_reg(22) = '0') then
                                -- Transfer the whole word in A_reg
                                wd_out <= A_reg;
                                if (IR_reg(22) = '1') then
                                    ad_out <= RES_reg;
                                else
                                    ad_out <= B_reg;
                                end if;
                            elsif (IR_reg(22) = '1') then
                               -- Transfer the lower byte of A_reg repeated again and again as required; read the memory first
                               if (IR_reg(24) = '1') then
                                    -- Pre-indexing
                                    ad_out <= RES_reg; -- Fetch the value at this address, also write at this address
                                    case RES_reg(1 downto 0) is
                                        when "00" => wd_out <= rd_in(31 downto 8) & A_reg(7 downto 0);
                                        when "01" => wd_out <= rd_in(31 downto 16) & A_reg(7 downto 0) & rd_in(7 downto 0);
                                        when "10" => wd_out <= rd_in(31 downto 24) & A_reg(7 downto 0) & rd_in(15 downto 0);
                                        when "11" => wd_out <= A_reg(7 downto 0) & rd_in(23 downto 0); 
                                        when others => wd_out <= "00000000000000000000000000000000";
                                    end case;
                               elsif (IR_reg(24) = '0') then
                                    -- Post-indexing
                                    ad_out <= B_reg; -- B_reg stores Rd
                                    case B_reg(1 downto 0) is
                                        when "00" => wd_out <= rd_in(31 downto 8) & A_reg(7 downto 0);
                                        when "01" => wd_out <= rd_in(31 downto 16) & A_reg(7 downto 0) & rd_in(7 downto 0);
                                        when "10" => wd_out <= rd_in(31 downto 24) & A_reg(7 downto 0) & rd_in(15 downto 0);
                                        when "11" => wd_out <= A_reg(7 downto 0) & rd_in(23 downto 0); 
                                        when others => wd_out <= "00000000000000000000000000000000";
                                    end case;
                               else -- do nothing
                               end if;
                            else
                                -- Do nothing
                            end if;
                        elsif (instr_class = DTSH) then
                            -- strsh_imm/reg
                            -- Transfer a half word as necessary
                            if (IR_reg(24) = '1') then
                                -- Pre-indexing
                                ad_out <= RES_reg;
                                case RES_reg(1 downto 0) is
                                    when "00" => wd_out <= rd_in(31 downto 16) & A_reg(15 downto 0);
                                    when "10" => wd_out <= A_reg(15 downto 0) & rd_in(15 downto 0);
                                    when others => wd_out <= "00000000000000000000000000000000";
                                end case;
                            elsif (IR_reg(24) = '0') then
                                -- Post-indexing
                                ad_out <= B_reg;
                                case B_reg(1 downto 0) is
                                    when "00" => wd_out <= rd_in(31 downto 16) & A_reg(15 downto 0);
                                    when "10" => wd_out <= A_reg(15 downto 0) & rd_in(15 downto 0);
                                    when others => wd_out <= "00000000000000000000000000000000";
                                end case;
                            end if; 
                        else
                            -- do nothing
                        end if;

                when mem_rd =>
                     -- Read value from memory and store in the register DR
                        ad_out <= RES_reg; enw_DR <= '1';

                when mem2RF =>

                        -- Write data from DR to the RF
                        enw_RF <= '1';
                        wad_RF <= Rd; 
                        if (instr_class = DT) then
                            -- ldrs_imm/reg, lrsp_imm/reg
                            if (IR_reg(22) = '0') then
                                -- Load the entire word from DR_reg
                                wd_RF <= DR_reg; 
                            elsif (IR_reg(22) = '1') then
                                -- Load a single byte into the bottom 8 bits
                                wd_RF(31 downto 8) <= "000000000000000000000000";
                                if (IR_reg(24) = '1') then
                                    -- Look at the value of RES_reg's lower two bits
                                    case RES_reg(1 downto 0) is
                                        when "00" => wd_RF(7 downto 0) <= DR_reg(7 downto 0);
                                        when "01" => wd_RF(7 downto 0) <= DR_reg(15 downto 8);
                                        when "10" => wd_RF(7 downto 0) <= DR_reg(23 downto 16);
                                        when "11" => wd_RF(7 downto 0) <= DR_reg(31 downto 24);
                                        when others => wd_RF(7 downto 0) <= "00000000";
                                    end case;
                                else 
                                    -- Look at the value of B_reg's lower two bits
                                    case B_reg(1 downto 0) is
                                        when "00" => wd_RF(7 downto 0) <= DR_reg(7 downto 0);
                                        when "01" => wd_RF(7 downto 0) <= DR_reg(15 downto 8);
                                        when "10" => wd_RF(7 downto 0) <= DR_reg(23 downto 16);
                                        when "11" => wd_RF(7 downto 0) <= DR_reg(31 downto 24);
                                        when others => wd_RF(7 downto 0) <= "00000000";
                                    end case;
                                end if;
                            else
                                -- do nothing
                            end if;
                        elsif (instr_class = DTSH) then
                            --ldrsh_imm/reg
                            if (IR_reg(6) = '1') then
                                -- The value of S is set => signed
                                if (IR_reg(5) = '1') then
                                    -- Load signed half words
                                    if (IR_reg(24) = '1') then
                                        -- Pre-indexing, look at RES_reg
                                        case RES_reg(1 downto 0) is
                                            when "00" => 
                                                wd_RF(15 downto 0) <= DR_reg(15 downto 0);
                                                wd_RF(31 downto 16) <= DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15);
                                            when "10" =>
                                                wd_RF(31 downto 16) <= DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31);   
                                                wd_RF(15 downto 0) <= DR_reg(31 downto 16);
                                            when others => wd_RF <= DR_reg;
                                        end case;
                                    else  
                                        -- Post-indexing, look at B_reg
                                        case B_reg(1 downto 0) is
                                            when "00" => 
                                                wd_RF(15 downto 0) <= DR_reg(15 downto 0);
                                                wd_RF(31 downto 16) <= DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15) & DR_reg(15);
                                            when "10" => 
                                                wd_RF(31 downto 16) <= DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31) & DR_reg(31);   
                                                wd_RF(15 downto 0) <= DR_reg(31 downto 16);
                                            when others => wd_RF <= DR_reg;
                                        end case;
                                    end if;
                                else 
                                    -- Load signed bytes
                                    if (IR_reg(24) = '1') then
                                        -- Pre-indexing
                                        case RES_reg(1 downto 0) is
                                            when "00" => if (DR_reg(7) = '1') then wd_RF <= x"FFFFFF" & DR_reg(7 downto 0); else wd_RF <= x"000000" & DR_reg(7 downto 0); end if;
                                            when "01" => if (DR_reg(15) = '1') then wd_RF <= x"FFFFFF" & DR_reg(15 downto 8); else wd_RF <= x"000000" & DR_reg(15 downto 8); end if;
                                            when "10" => if (DR_reg(23) = '1') then wd_RF <= x"FFFFFF" & DR_reg(23 downto 16); else wd_RF <= x"000000" & DR_reg(23 downto 16); end if;
                                            when "11" => if (DR_reg(31) = '1') then wd_RF <= x"FFFFFF" & DR_reg(31 downto 24); else wd_RF <= x"000000" & DR_reg(31 downto 24); end if;
                                            when others => wd_RF(7 downto 0) <= "00000000";
                                        end case;
                                    else 
                                        -- Look at the value of B_reg's lower two bits
                                        case B_reg(1 downto 0) is
                                            when "00" => wd_RF(7 downto 0) <= DR_reg(7 downto 0);
                                            when "01" => wd_RF(7 downto 0) <= DR_reg(15 downto 8);
                                            when "10" => wd_RF(7 downto 0) <= DR_reg(23 downto 16);
                                            when "11" => wd_RF(7 downto 0) <= DR_reg(31 downto 24);
                                            when others => wd_RF(7 downto 0) <= "00000000";
                                        end case;
                                    end if;
                                end if;
                            else
                                -- Signed bit is not set => unsigned
                                -- Load a half word here
                                wd_RF(31 downto 16) <= "0000000000000000";
                                -- Check the pre/post indexing bit
                                 if (IR_reg(24) = '1') then
                                   -- Pre-indexing, look at RES_reg
                                   case RES_reg(1 downto 0) is
                                       when "00" => wd_RF(15 downto 0) <= DR_reg(15 downto 0);
                                       when "10" => wd_RF(15 downto 0) <= DR_reg(15 downto 0);
                                       when others => wd_RF(15 downto 0) <= "0000000000000000";
                                   end case;
                               else  
                                   -- Post-indexing, look at B_reg
                                   case B_reg(1 downto 0) is
                                       when "00" => wd_RF(15 downto 0) <= DR_reg(15 downto 0);
                                       when "10" => wd_RF(15 downto 0) <= DR_reg(15 downto 0);
                                       when others => wd_RF(15 downto 0) <= "0000000000000000";
                                   end case;
                               end if;
                            end if;
                        else
                            -- do nothing
                        end if; 
                when brn =>
                        case i_decoded is 
                            when b =>
                                -- Branch unconditionally
                                -- PC = PC + S2(Imm24) + 4
                                op1 <= "00" & pc(31 downto 2); op2 <= "00" & Immconv(31 downto 2); cin <= '1';
                                optype <= addi;
                                next_pc <= result(29 downto 0) & "00";
                                enw_PC <= '1';
                            wad_RF <= "1110"; wd_RF <= pc; enw_RF <= ins_value(24);
                            when others =>
                                -- do nothing
                        end case;
                when halt =>

                when mult =>
                        -- Multiply Rm, Rs; store in result_66
                        enw_result_66 <= '1';
                        result_66_tmp <= std_logic_vector(signed(op1_33)*signed(op2_33));
                        -- Simultaneously read Rn, Rd; latch into A_reg and B_reg
                        rad1 <= Rn; rad2 <= Rd; enw_A <= '1'; enw_B <= '1';
                    when add1 =>
                            if (i_decoded = mla or i_decoded = umlal or i_decoded = smlal) then
                                 -- Latch lower result_66 + RdLo(Rn) into RES_reg
                                enw_RES <= '1'; is_mult <= '1';
                            flag_write <= IR_reg(20);
                                optype <= addi;
                                op1 <= result_66(31 downto 0);
                                op2 <= A_reg;
                            elsif (i_decoded = mul or i_decoded = umull or i_decoded = smull) then
                                -- Latch lower of result_66 into RES_reg
                                enw_RES <= '1'; is_mult <= '1';
                                    flag_write <= IR_reg(20);
                                optype <= addi;
                                op1 <= result_66(31 downto 0);
                                op2 <= "00000000000000000000000000000000";
                            else  
                                -- do mnothing
                            end if;
                    when add2 =>

                        if (i_decoded = mla or i_decoded = mul) then
                            -- No addition required here, latch RES_reg into Rd
                            wad_RF <= Rd; wd_RF <= RES_reg;
                            enw_RF <= '1';
                        elsif (i_decoded = umlal or i_decoded = smlal) then
                            -- Add the higher bits of result_66, with Rd taking care of carry
                            -- Add RdHi to Rd with carry
                    Z_override <= z;
                    flag_write <= IR_reg(20);
                            optype <= addi; enw_RES <= '1';
                            op1 <= result_66(63 downto 32);
                            op2 <= B_reg;
                            cin <= ((A_reg(31) and result_66(31)) or (RES_reg(31) and result_66(31)) or (RES_reg(31) and A_reg(31)));
                            -- Latch RES_reg into RdLO
                            wad_RF <= Rn; wd_RF <= RES_reg; enw_RF <= '1';
                        elsif (i_decoded = umull or i_decoded = smull) then
                            -- Add the higher bits of result_66 with '0' 
                    Z_override <= z;
                            flag_write <= IR_reg(20);
                            optype <= addi; enw_RES <= '1';
                            op1 <= result_66(63 downto 32);
                            op2 <= "00000000000000000000000000000000";
                            -- Latch RES_reg into RdLO
                            wad_RF <= Rn; wd_RF <= RES_reg; enw_RF <= '1';
                        end if;
                    when mul_res2RF =>
                            if (i_decoded = mla or i_decoded = mul) then
                                -- Do nothing    
                            elsif (i_decoded = umlal or i_decoded = smlal or i_decoded = umull or i_decoded = smull) then
                                -- Latch RES_reg into RdHi(Rd)
                                wad_RF <= Rd; wd_RF <= RES_reg; enw_RF <= '1';
                            end if;
                when psr =>
                    case i_decoded is
                        when mrs =>
                            -- Transfer the PSR contents to a register Rd
                            if (m = '1' and IR_reg(22) = '1') then
                                -- Transfer saved values to Rd
                                wad_RF <= IR_reg(15 downto 12);
                                wd_RF <= n_svd & z_svd & c_svd & v_svd & "00000000000000000000" & i_svd & "00" & "10011";
                                enw_RF <= '1';
                            elsif (m = '1' and IR_reg(22) = '0') then
                                -- Transfer current values
                                wad_RF <= IR_reg(15 downto 12);
                                wd_RF <= n & z & c & v & "00000000000000000000" & i & "00" & "10011";
                            elsif (m = '0') then
                                -- Transfer the current values
                                wad_RF <= IR_reg(15 downto 12);
                                wd_RF <= n & z & c & v & "00000000000000000000" & i & "00" & "10000";
                            end if;
                        when msr =>
                            -- Transfer the register contents to PSR
                            if (m = '1' and IR_reg(22) = '1') then
                                -- In supervisor mode, transfer to saved register
                                st_status <= '1'; 
                                n_svd_in <= B_reg(31);
                                z_svd_in <= B_reg(30);
                                c_svd_in <= B_reg(29);
                                v_svd_in <= B_reg(28);
                                i_svd_in <= B_reg(7);
                                if (B_reg(4 downto 0) = "10011") then 
                                    m_svd_in <= '1'; 
                                elsif (B_reg(4 downto 0) = "10000") then 
                                    m_svd_in <= '0';
                                end if;
                            elsif (m = '1' and IR_reg(22) = '0') then
                                -- Transfer to current register
                                ld_status <= '1'; 
                                n_curr_in <= B_reg(31);
                                z_curr_in <= B_reg(30);
                                c_curr_in <= B_reg(29);
                                v_curr_in <= B_reg(28);
                                i_in <= B_reg(7); enw_i <= '1';
                                enw_m <= '1';
                                if (B_reg(4 downto 0) = "10011") then 
                                    m_in <= '1'; 
                                elsif (B_reg(4 downto 0) = "10000") then 
                                    m_in <= '0';
                                end if;
                            elsif (m = '0') then
                                -- Transfer to current register, omit the mode
                                st_status <= '1';
                                n_curr_in <= B_reg(31);
                                z_curr_in <= B_reg(30);
                                c_curr_in <= B_reg(29);
                                v_curr_in <= B_reg(28);
                            end if;
                        when others =>
                            -- do nothing
                    end case;
                when rst_exc => 
                    -- Load the ISR address into PC
                   next_pc <= "00000000000000000000000000000000";
                   enw_PC <= '1';
                   
                   -- Set mode to 1
                   st_status <= '1';
                   n_svd_in <= n;
                   z_svd_in <= z;
                   v_svd_in <= v;
                   c_svd_in <= c;
                   m_svd_in <= m;
                   i_svd_in <= i;
                   enw_m <= '1'; m_in <= '1';
                   enw_i <= '1'; i_in <= '1';
                   if (m = '1') then 
                        enw_change_mode <= '1'; change_mode_in <= '0';
                   end if;
               when swi_exc =>
                   -- SWI exception
                   --Load ISR address into PC
                  next_pc <= "00000000" & IR_reg(23 downto 0);
                  enw_PC <= '1';
                  
                  -- Save the return address in r14_svc
                  enw_r14svc <= '1';
                  r14svc_in <= pc;
                  
                  -- Save the CPSR
                  st_status <= '1';
                  n_svd_in <= n;
                 z_svd_in <= z;
                 v_svd_in <= v;
                 c_svd_in <= c;
                  m_svd_in <= m;
                   i_svd_in <= i;
                   enw_m <= '1'; m_in <= '1';
                   enw_i <= '1'; i_in <= '1';
                    if (m = '1') then 
                          enw_change_mode <= '1'; change_mode_in <= '0';
                     end if;
               when undef_exc =>
                    -- Undefined exception          
              --Load ISR address into PC
              next_pc <= "00000000000000000000000000000100";
              enw_PC <= '1';
              
              -- Save the return address in r14_svc
              enw_r14svc <= '1';
              r14svc_in <= pc;
              
              -- Save the CPSR
              st_status <= '1';
              n_svd_in <= n;
               z_svd_in <= z;
               v_svd_in <= v;
               c_svd_in <= c;
              m_svd_in <= m;
               i_svd_in <= i;
               enw_m <= '1'; m_in <= '1';
               enw_i <= '1'; i_in <= '1';
               when irq_exc =>
                    -- Load ISR address into PC
                    iack <= '1';
                     next_pc <= "00000000000000000000000000011000";
                     enw_PC <= '1';
                     
                     -- Save the return address
                     enw_r14svc <= '1';
                     r14svc_in <= pc;
                     
                     -- Save the flags
                     st_status <= '1';
                     n_svd_in <= n;
                    z_svd_in <= z;
                    v_svd_in <= v;
                    c_svd_in <= c;
                     m_svd_in <= m;
                  i_svd_in <= i;
                  enw_m <= '1'; m_in <= '1';
                  enw_i <= '1'; i_in <= '1';
                  if (m = '1') then 
                    enw_change_mode <= '1'; change_mode_in <= '0';
               end if;
               when skip =>
                    -- do nothing
                when others =>
                    -- do nothing
            end case;
        
end process;

	-- Write value into middle registers
	assign_registers: process(clock, reset) begin
		if (reset = '1') then
			-- Set all registers to zero
			IR_reg <= "00000000000000000000000000000000";
			A_reg <= "00000000000000000000000000000000";
			B_reg <= "00000000000000000000000000000000";
			DR_reg <= "00000000000000000000000000000000";
			RES_reg <= "00000000000000000000000000000000";
		elsif rising_edge(clock) and is_green = '1' then
			if (enw_IR = '1') then
				IR_reg <= ins_value;
			end if;

			if (enw_A = '1') then
				A_reg <= rd1;
			end if;

			if (enw_B = '1') then
				B_reg <= rd2;
			end if;

			if (enw_RES = '1') then
				RES_reg <= result;
			end if;

			if (enw_DR = '1') then
				DR_reg <= rd_in;
			end if;
 		end if;
	end process;

	-- Pass the truncated instruction address to the program memory
	process(reset, ins_addr) begin
	   if (reset = '1') then ins_addr_trunc <= "0000000000";
	   else 
	       ins_addr_trunc <= ins_addr(11 downto 2);
	   end if;
	end process;

	-- Assign control_state values to LEDs for debugging
	state_out_assign: process(control_state_sig, execution_state_sig) begin
		case control_state_sig is
			when fetch =>
				state_out(3 downto 0) <= "0000";
			when decode =>
				state_out(3 downto 0) <= "0001";
			when arith =>
				state_out(3 downto 0) <= "0010";
			when addr =>
				state_out(3 downto 0) <= "0011";
			when brn =>
				state_out(3 downto 0) <= "0100";
			when halt =>
				state_out(3 downto 0) <= "0101";
			when res2RF =>
				state_out(3 downto 0) <= "0110";
			when mem_wr =>
				state_out(3 downto 0) <= "0111";
			when mem_rd =>
				state_out(3 downto 0) <= "1000";
			when mem2RF =>
				state_out(3 downto 0) <= "1001";
			when others =>
				state_out(3 downto 0) <= "1111";
		end case;
		
		case execution_state_sig is
		  when initial =>
		      state_out(6 downto 4) <= "000";
		  when onestep =>
		      state_out(6 downto 4) <= "001";
          when oneinstr =>
              state_out(6 downto 4) <= "010";
          when cont =>
              state_out(6 downto 4) <= "011";
          when done =>
              state_out(6 downto 4) <= "100";
          when others =>
              state_out(6 downto 4) <= "111";
		end case;
	end process;

	assign_result_66: process(reset, clock) begin
	        if (reset = '1') then
	            result_66 <= "000000000000000000000000000000000000000000000000000000000000000000";
	        elsif (rising_edge(clock)) then
	            if (enw_result_66 = '1') then result_66 <= result_66_tmp; end if;
	        end if;
	    end process;
	
	    assign_op33: process(A_reg, B_reg, i_decoded) begin
	        if i_decoded = smull then
	            op1_33 <= A_reg(31) & A_reg;
	            op2_33 <= B_reg(31) & B_reg;
	        else 
	            op1_33 <= '0' & A_reg;
	            op2_33 <= '0' & B_reg;
	        end if;
	    end process;

    ins_addr <= pc;
	pc_out <= pc;
	A_out <= A_reg;
	B_out <= B_reg;
	DR_out <= DR_reg;
	RES_out <= RES_reg;
	IR_out <= IR_reg;
	z_in <= z;
	n_in <= n;
	v_in <= v;
	c_in <= c;
	m_out <= m;
	enw_m_out <= enw_m;
	enw_RF_out <= enw_RF;
	wd_RF_out <= wd_RF;
	wad_RF_out <= wad_RF;
	r14svc_out <= r14_svc;
end Architecture;
