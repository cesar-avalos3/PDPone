library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity TOP_sim is
--  Port ( );
end TOP_sim;

architecture Behavioral of TOP_sim is

component TOP is
port(clk, rst: in std_logic;
     led: out std_logic_vector(0 to 3));
end component;

signal clk: std_logic := '0';
signal rst: std_logic := '1';
signal led: std_logic_vector(0 to 3);

signal counter: natural := 0;

begin
    CPU_tb: TOP port map(clk => clk, rst => rst, led => led);
    process begin
        if(counter >= 3) then
            rst <= '0';
        else
            counter <= counter + 1;
        end if;
        clk <= not clk;
        wait for 25 ns;
    end process;

end Behavioral;
