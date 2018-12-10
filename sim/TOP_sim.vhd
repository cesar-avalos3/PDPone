library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity TOP_sim is
--  Port ( );
end TOP_sim;

architecture Behavioral of TOP_sim is

component TOP is
port(clk, rst: in std_logic;
               led: out std_logic_vector(0 to 3);
VGA_HS_O : out  STD_LOGIC;
VGA_VS_O : out  STD_LOGIC;
VGA_R : out  STD_LOGIC_VECTOR (3 downto 0);
VGA_B : out  STD_LOGIC_VECTOR (3 downto 0);
VGA_G : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

signal clk: std_logic := '0';
signal rst: std_logic := '1';
signal led: std_logic_vector(0 to 3);
signal vga_hs, vga_vs: std_logic;
signal vga_r, vga_b, vga_g: std_logic_vector(3 downto 0);

signal counter: natural := 0;

begin
    CPU_tb: TOP port map(clk => clk, rst => rst, led => led, VGA_HS_O => vga_hs, VGA_VS_O => vga_vs, vga_r => vga_r, vga_b => vga_b, vga_g => vga_g);
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
