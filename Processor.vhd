----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01/28/2019 01:49:38 PM
-- Design Name: 
-- Module Name: Processor - Behavioral
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

entity Processor is
        Port(
           rawclock, rawreset, rawstep, rawinstr: in std_logic;
             rawgo: in std_logic;
         pselect: in std_logic_vector(2 downto 0);
         mode, rad3: in std_logic_vector(3 downto 0);
        rst_signal: in std_logic;
          rows: in std_logic_vector(3 downto 0); 
          
         led1,led2,led3,led4,led5,led6,led7,led8,led9,led10,led11,led12,led13,led14,led15,led16:out std_logic;
         A0, B0, C0, D0, E0, F0, G0: out std_logic;
         anode: out std_logic_vector(3 downto 0);
         cols: out std_logic_vector(3 downto 0)

         );
end Processor;

architecture Behavioral of Processor is
    component display is 
        Port(  
            ins_value, wd_out, pc, rf_value: in std_logic_vector(31 downto 0);
            mode: in std_logic_vector(3 downto 0);
            led1,led2,led3,led4,led5,led6,led7,led8,led9,led10,led11,led12,led13,led14,led15,led16:out std_logic;
            state_out: in std_logic_vector(6 downto 0);
            IR_in, DR_in, A_in, B_in, RES_in: in std_logic_vector(31 downto 0);
            z_in,c_in,n_in,v_in,m_in, enw_m_in, enw_RF:in std_logic;
            wad_RF: in std_logic_vector(3 downto 0);
            wd_RF: in std_logic_vector(31 downto 0);
            r14_svc: in std_logic_vector(31 downto 0)
        );
    end component;
    
    component dist_mem_gen_1 IS
      PORT (
        a : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        d : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk : IN STD_LOGIC;
        we : IN STD_LOGIC;
        spo : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
      );
    END component;
    
    component dist_mem_gen_0 IS
        port (
            a : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
            spo : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
            );    
   end component;
   
   component clockgen is 
        Port (reset, rawclk: in std_logic;
              outclk: out std_logic
         );
   end component;
        
   
   component debounce is
        Port (insig, clk: in std_logic;
           outsig: out std_logic 
          );
    end component;
   
   component CPU is 
       Port(clock, reset, step, go, instr: in std_logic; 
            ins_addr_trunc: out std_logic_vector(9 downto 0); -- instruction address fed to the program memory (truncated to 8 bits)
            ins_value: in std_logic_vector(31 downto 0); -- instruction value from program memory        
            ad_out: out std_logic_vector(31 downto 0); -- Memory address fed to the data memory (truncated to 8 bits)
            wd_out, pc_out, rd3: out std_logic_vector(31 downto 0); -- Data to write to data memory
            rd_in: in std_logic_vector(31 downto 0); -- Data fed from memory
            enw_DM: out std_logic;
            rad3: in std_logic_vector(3 downto 0);
            pselect: in std_logic_vector(2 downto 0); -- Write-enable to data memory
            state_out: out std_logic_vector(6 downto 0);
            A_out, B_out, DR_out, RES_out, IR_out: out std_logic_vector(31 downto 0);
            c_in,v_in,z_in,n_in,m_out: out std_logic;
            rst_signal, irq_signal: in std_logic;
            enw_m_out, enw_RF_out, iack: out std_logic;
            wad_RF_out: out std_logic_vector(3 downto 0);
            wd_RF_out: out std_logic_vector(31 downto 0);
            r14svc_out: out std_logic_vector(31 downto 0)
           );
   end component;
   
    -- Signals for test bench --
       signal irq_signal: std_logic := '0';
--      signal rawgo: std_logic;
--        signal rst_signal, rawclock, rawreset, rawstep, rawinstr: std_logic := '0';
--       signal pselect: std_logic_vector(2 downto 0) := "000";
--        signal mode, rad3: std_logic_vector(3 downto 0) := "0000";  
--        signal rows: std_logic_vector(3 downto 0) := "0000";

        signal enw_m, iack: std_logic;
        signal enw_DM: std_logic := '0';
        signal ROM_addr: std_logic_vector(9 downto 0) := "0000000000";
        signal RAM_addr: std_logic_vector(9 downto 0) := "0000000000";
        signal write_data, read_data, ins_value, pc: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
        signal step, clock, reset, go, instr: std_logic;
        signal rd3: std_logic_vector(31 downto 0);
        signal state_out: std_logic_vector(6 downto 0);
        signal A, B, IR, DR, RES: std_logic_vector(31 downto 0);
        signal v_in,c_in,z_in,n_in: std_logic;
        signal m, enw_RF: std_logic;
        signal wad_RF: std_logic_vector(3 downto 0);
        signal wd_RF, r14_svc: std_logic_vector(31 downto 0);
        signal digit: std_logic_vector(3 downto 0);
        signal bus_addr, bus_value_in, bus_value_out: std_logic_vector(31 downto 0);
        signal enw_bus, enw_anode, enw_digit: std_logic;
        signal bus_digit_out, bus_anode_out: std_logic_vector(3 downto 0);
        signal anode_tmp: std_logic_vector(3 downto 0);
        signal row_tmp: std_logic_vector(6 downto 0);
        signal cols_tmp: std_logic_vector(3 downto 0);
        signal enw_cols: std_logic;
        signal bus_cols_out: std_logic_vector(3 downto 0);
begin
    CPU_instance:
        CPU
        PORT MAP(
            instr => rawinstr,
            step => rawstep,
            go => rawgo,
            reset => rawreset,
            pselect => pselect,
            rd3 => rd3,
            rad3 => rad3,
            clock => rawclock,  
            enw_DM => enw_bus,
            m_out => m,
            ins_addr_trunc => ROM_addr,
            ad_out => bus_addr,
            rd_in => bus_value_out,
            wd_out => bus_value_in,
            ins_value => ins_value,
            pc_out => pc,
            iack => iack,
            state_out => state_out,
            A_out => A,
            B_out => B,
            RES_out => RES,
            DR_out => DR,
            IR_out => IR,
            c_in => c_in,
            z_in => z_in,
            v_in => v_in,
            n_in => n_in,
            irq_signal => irq_signal,
            rst_signal => rst_signal,
            enw_m_out => enw_m,
            enw_RF_out => enw_RF,
            wad_RF_out => wad_RF,
            wd_RF_out => wd_RF,
            r14svc_out => r14_svc
        );
        
       my_bus:
            entity work.my_bus(Behavioral)
            PORT MAP(
                bus_addr => bus_addr,
                bus_value_in => bus_value_in,
                bus_value_out => bus_value_out,
                bus_anode_out => bus_anode_out,
                bus_digit_out => bus_digit_out,
                enw_bus => enw_bus,
                enw_DM => enw_DM,
                enw_anode => enw_anode,
                enw_digit => enw_digit,
                mode => m,
                bus_anode_in => anode_tmp,
                bus_digit_in => digit,
                bus_DM_in => read_data,
                bus_DM_addr => RAM_addr,
                bus_DM_out => write_data,
                bus_cols_in => cols_tmp,
                bus_rows_in => rows,
                bus_cols_out => bus_cols_out,
                enw_cols => enw_cols
            );
         
       row_tmp <= "000" & rows;   
     
       display_instance:
           display
           PORT MAP(   
               ins_value => ins_value,
               pc => pc,
               wd_out => write_data,
               rf_value => rd3,
               mode => mode, 
               led1 => led1,
               led2 => led2,
               led3 => led3,
               led4 => led4,
               led5 => led5,
               led6 => led6,
               led7 => led7,
               led8 => led8,
               led9 => led9,
               led10 => led10,
               led11 => led11,
               led12 => led12,
               led13 => led13,
               led14 => led14,
               led15 => led15,
               led16 => led16,
               state_out => state_out,
               IR_in => IR,
               A_in => A,
               B_in => B,
               RES_in => RES,
               DR_in => DR,
               n_in => n_in,
               v_in => v_in,
               z_in => z_in,
               c_in => c_in,
               m_in => m,
               enw_m_in => enw_m,
               enw_RF => enw_RF,
               wad_RF => wad_RF,
               wd_RF => wd_RF,
               r14_svc => r14_svc 
       );
                   
    
        
    debounce_1:
        debounce
        PORT MAP(
            insig => rawreset,
            clk => clock,
            outsig => reset
        );
        
    debounce_2:
        debounce
        PORT MAP(
            insig => rawgo,
            clk => clock,
            outsig => go
        );
        
    debounce_3:
        debounce
        PORT MAP(
            insig => rawstep,
            clk => clock,
            outsig => step
        );
        
    debounce_4:
        debounce
        PORT MAP(
            insig => rawinstr,
            clk => clock,
            outsig => instr
        );
        
    clockgen_instance:
        clockgen
        PORT MAP(
            reset => rawreset,
            rawclk => rawclock,
            outclk => clock);
        
    ROM_instance: 
        dist_mem_gen_0
        PORT MAP(
            a => ROM_addr, 
            spo => ins_value
        );
        
    RAM_instance:
        dist_mem_gen_1
        PORT MAP(
            clk => rawclock,
            a => RAM_addr,
            d => write_data,
            we => enw_DM,
            spo => read_data
        );
        
    basys_display:
        entity work.basys_display(Behavioral)
        PORT MAP(
            a_in => digit,
            A => A0,
            B => B0,
            C => C0,
            D => D0,
            E => E0,
            F => F0,
            G => G0
        );
    
    process(rawclock, rawreset) begin
        if (rawreset = '1') then 
            anode_tmp <= "1110";
            digit <= "0000";
            cols_tmp <= "1110";
        elsif (rising_edge(rawclock)) then
            if (enw_anode = '1') then
                anode_tmp <= bus_anode_out;
            end if;
            
            if (enw_cols = '1') then
                cols_tmp <= bus_cols_out;
            end if;
            
            if (enw_digit = '1') then
                digit <= bus_digit_out;
            end if;
        end if;
    end process;
    
    anode <= anode_tmp;
    cols <= cols_tmp;
        
--    process(clock, iack) begin
--        -- Run IRQ on reset, to set the row values
--      if (iack = '1') then
--            irq_signal <= '0';
--        elsif rising_edge(clock) then
--            irq_signal <= not irq_signal;
--        end if;
--    end process;

    irq_signal <= '0';
--    rawreset <= '1', '0' after 2ns; 
--    rawgo <= '1';
--    mode <= "1101"; pselect <= "000"; rad3 <= "0100";
    
--    rawstep <= '0';
--    rows <= "1110" when cols_tmp = "1110" else "1111";
    
--    clock_proc:process begin
--        rawclock <= not rawclock;
--        wait for 1ns;
--    end process;
end Behavioral;
