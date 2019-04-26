-- This is the register file for the processor

Library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.numeric_std.all; 
use IEEE.STD_LOGIC_UNSIGNED.ALL; 

library UNISIM;
use UNISIM.VComponents.all;

entity registers is
  Port (clock, reset, enw_RF, enw_PC: in std_logic;
         -- enw_RF is the write-enable input for the register file
        pselect: in std_logic_vector(2 downto 0);
        rad1, rad2, rad3: in std_logic_vector(3 downto 0); -- rad1, rad2 are the read addresses
        wad: in std_logic_vector(3 downto 0); -- wad is the write address
        wd, pc_in: in std_logic_vector(31 downto 0); -- wd is the write data
        rd1, rd2, rd3, pc_out: out std_logic_vector(31 downto 0); -- rd1, rd2 are the read values
        is_green:  in std_logic
       );
end registers;

architecture Behavioral of registers is
      type  RAM is array(0 to 15) of std_logic_vector(31 downto 0);
      signal regfile: RAM := (others => (others => '0')); 
begin
    process(clock, reset) begin
        if rising_edge(clock) and enw_RF = '1' then
         -- Write to memory synchronously
         regfile(to_integer(unsigned(wad))) <= wd;
      end if;
      
      if reset = '1' then 
        pc_out <= "0000000000000000000000" & pselect & "0000000";
        regfile <= (others => (others => '0'));
      elsif rising_edge(clock) and enw_PC = '1' and is_green = '1' then
        pc_out <= pc_in;
      else 
        -- No change
       end if;
    end process;
    
    rd1 <= regfile(to_integer(unsigned(rad1)));
    rd2 <= regfile(to_integer(unsigned(rad2)));
    rd3 <= regfile(to_integer(unsigned(rad3)));
end Behavioral;
