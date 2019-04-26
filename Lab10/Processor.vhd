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
            rawclock, rawreset, rawstep, rawgo, rawinstr: in std_logic;
         pselect: in std_logic_vector(2 downto 0);
         mode, rad3: in std_logic_vector(3 downto 0);
         led1,led2,led3,led4,led5,led6,led7,led8,led9,led10,led11,led12,led13,led14,led15,led16:out std_logic
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
            z_in,c_in,n_in,v_in:in std_logic
        );
    end component;
    
    component dist_mem_gen_1 IS
      PORT (
        a : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        d : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk : IN STD_LOGIC;
        we : IN STD_LOGIC;
        spo : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
      );
    END component;
    
    component dist_mem_gen_0 IS
        port (
            a : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
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
            ins_addr_trunc: out std_logic_vector(7 downto 0); -- instruction address fed to the program memory (truncated to 8 bits)
            ins_value: in std_logic_vector(31 downto 0); -- instruction value from program memory        
            ad_out_trunc: out std_logic_vector(7 downto 0); -- Memory address fed to the data memory (truncated to 8 bits)
            wd_out, pc_out, rd3: out std_logic_vector(31 downto 0); -- Data to write to data memory
            rd_in: in std_logic_vector(31 downto 0); -- Data fed from memory
            enw_DM: out std_logic;
            rad3: in std_logic_vector(3 downto 0);
            pselect: in std_logic_vector(2 downto 0); -- Write-enable to data memory
            state_out: out std_logic_vector(6 downto 0);
            A_out, B_out, DR_out, RES_out, IR_out: out std_logic_vector(31 downto 0);
            c_in,v_in,z_in,n_in: out std_logic
           );
   end component;
   
    -- Signals for test bench --
--    signal rawclock, rawreset, rawstep, rawgo, rawinstr: std_logic := '0';
--    signal pselect: std_logic_vector(2 downto 0) := "000";
--    signal mode, rad3: std_logic_vector(3 downto 0) := "0000";  
    
    signal enw_DM: std_logic := '0';
    signal ROM_addr, RAM_addr: std_logic_vector(7 downto 0) := "00000000";
    signal write_data, read_data, ins_value, pc: std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
    signal step, clock, reset, go, instr: std_logic;
    signal rd3: std_logic_vector(31 downto 0);
    signal state_out: std_logic_vector(6 downto 0);
    signal A, B, IR, DR, RES: std_logic_vector(31 downto 0);
    signal v_in,c_in,z_in,n_in: std_logic;
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
            enw_DM => enw_DM,
            ins_addr_trunc => ROM_addr,
            ad_out_trunc => RAM_addr,
            rd_in => read_data,
            wd_out => write_data,
            ins_value => ins_value,
            pc_out => pc,
            state_out => state_out,
            A_out => A,
            B_out => B,
            RES_out => RES,
            DR_out => DR,
            IR_out => IR,
            c_in => c_in,
            z_in => z_in,
            v_in => v_in,
            n_in => n_in
        );
        
      
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
               c_in => c_in
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
            reset => reset,
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
        
--    rawreset <= '1', '0' after 2ns;
--    mode <= "1011"; pselect <= "000"; rad3 <= "0100";
--    rawgo <= '1'; rawstep <= '0'; rawinstr <= '0';
--    clock_proc:process begin
--        rawclock <= not rawclock;
--        wait for 1ns;
--    end process;
end Behavioral;
