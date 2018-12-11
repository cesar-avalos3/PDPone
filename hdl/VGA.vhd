----------------------------------------------------------------------------------
-- Company: Digilent
-- Engineer: Arthur Brown
-- 
--
-- Create Date:    13:01:51 02/15/2013 
-- Project Name:   pmodvga
-- Target Devices: arty
-- Tool versions:  2016.4
-- Additional Comments: 
--
-- Copyright Digilent 2017
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity vga is
    Port ( CLK_I, pxl_clk : in  STD_LOGIC;
           x, y: in natural;
           write_to_VRAM: in std_logic;
           VGA_HS_O : out  STD_LOGIC;
           VGA_VS_O : out  STD_LOGIC;
           VGA_R : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_B : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_G : out  STD_LOGIC_VECTOR (3 downto 0));
end vga;

architecture Behavioral of vga is



component blk_mem_gen_0
port(addra,addrb: in std_logic_vector(19 downto 0);
     clka: in std_logic;
     dina: in std_logic_vector(0 downto 0);
     doutb: out std_logic_vector(0 downto 0);
     wea: in std_logic_vector(0 downto 0);
     enb, clkb: in std_logic);
end component;

--Sync Generation constants

----***640x480@60Hz***--  Requires 25 MHz clock
--constant FRAME_WIDTH : natural := 640;
--constant FRAME_HEIGHT : natural := 480;

--constant H_FP : natural := 16; --H front porch width (pixels)
--constant H_PW : natural := 96; --H sync pulse width (pixels)
--constant H_MAX : natural := 800; --H total period (pixels)

--constant V_FP : natural := 10; --V front porch width (lines)
--constant V_PW : natural := 2; --V sync pulse width (lines)
--constant V_MAX : natural := 525; --V total period (lines)

--constant H_POL : std_logic := '0';
--constant V_POL : std_logic := '0';

----***800x600@60Hz***--  Requires 40 MHz clock
--constant FRAME_WIDTH : natural := 800;
--constant FRAME_HEIGHT : natural := 600;
--
--constant H_FP : natural := 40; --H front porch width (pixels)
--constant H_PW : natural := 128; --H sync pulse width (pixels)
--constant H_MAX : natural := 1056; --H total period (pixels)
--
--constant V_FP : natural := 1; --V front porch width (lines)
--constant V_PW : natural := 4; --V sync pulse width (lines)
--constant V_MAX : natural := 628; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';


----***1280x720@60Hz***-- Requires 74.25 MHz clock
--constant FRAME_WIDTH : natural := 1280;
--constant FRAME_HEIGHT : natural := 720;
--
--constant H_FP : natural := 110; --H front porch width (pixels)
--constant H_PW : natural := 40; --H sync pulse width (pixels)
--constant H_MAX : natural := 1650; --H total period (pixels)
--
--constant V_FP : natural := 5; --V front porch width (lines)
--constant V_PW : natural := 5; --V sync pulse width (lines)
--constant V_MAX : natural := 750; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';

----***1280x1024@60Hz***-- Requires 108 MHz clock
constant FRAME_WIDTH : natural := 1280;
constant FRAME_HEIGHT : natural := 1024;

constant H_FP : natural := 48; --H front porch width (pixels)
constant H_PW : natural := 112; --H sync pulse width (pixels)
constant H_MAX : natural := 1688; --H total period (pixels)

constant V_FP : natural := 1; --V front porch width (lines)
constant V_PW : natural := 3; --V sync pulse width (lines)
constant V_MAX : natural := 1066; --V total period (lines)

constant H_POL : std_logic := '1';
constant V_POL : std_logic := '1';

--Moving Box constants
constant BOX_WIDTH : natural := 8;
constant BOX_CLK_DIV : natural := 1000000; --MAX=(2^25 - 1)

constant BOX_X_MAX : natural := (512 - BOX_WIDTH);
constant BOX_Y_MAX : natural := (FRAME_HEIGHT - BOX_WIDTH);

constant BOX_X_MIN : natural := 0;
constant BOX_Y_MIN : natural := 256;

constant BOX_X_INIT : std_logic_vector(11 downto 0) := x"000";
constant BOX_Y_INIT : std_logic_vector(11 downto 0) := x"190"; --400

signal active : std_logic;

signal h, v: natural := 0;

signal h_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');
signal v_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');

signal h_sync_reg : std_logic := not(H_POL);
signal v_sync_reg : std_logic := not(V_POL);

signal h_sync_dly_reg : std_logic := not(H_POL);
signal v_sync_dly_reg : std_logic :=  not(V_POL);

signal vga_red_reg : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_green_reg : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_blue_reg : std_logic_vector(3 downto 0) := (others =>'0');

signal vga_red : std_logic_vector(3 downto 0);
signal vga_green : std_logic_vector(3 downto 0);
signal vga_blue : std_logic_vector(3 downto 0);

signal box_x_reg : std_logic_vector(11 downto 0) := BOX_X_INIT;
signal box_x_dir : std_logic := '1';
signal box_y_reg : std_logic_vector(11 downto 0) := BOX_Y_INIT;
signal box_y_dir : std_logic := '1';
signal box_cntr_reg : std_logic_vector(24 downto 0) := (others =>'0');

signal update_box : std_logic;
signal pixel_in_box : std_logic;

type vid_buffer is array(0 to 30, 0 to 30) of std_logic;
signal video_buffer : vid_buffer := (others => (others => '0'));

signal video_buffer_out: std_logic;

signal addra, addrb: std_logic_vector(19 downto 0);
signal clka: std_logic;
signal dina, doutb: std_logic_vector(0 downto 0);
signal wea: std_logic_vector(0 downto 0);
begin

video_ram: blk_mem_gen_0 port map(addra => addra, addrb => addrb, clka => clk_i, dina => dina, doutb => doutb, wea(0) => write_to_VRAM, enb => '1', clkb=>pxl_clk);

-- Port A is the WRITE port
-- Port B is the READ port
addra <= std_logic_vector(to_unsigned(y * 1024 + x, addra'length));
  ----------------------------------------------------
  -------         TEST PATTERN LOGIC           -------
  ----------------------------------------------------
  --vga_red   <=  video_buffer(x,y) &  video_buffer(x,y) &  video_buffer(x,y) & video_buffer(x,y) when (active = '1') else (others => '0');
                
  --vga_blue  <= "00" & video_buffer(x,y) & video_buffer(x,y) when (active = '1') else (others => '0');
              
  --vga_green <= "000" & video_buffer(x,y) when (active = '1') else (others => '0');
              
 ------------------------------------------------------
 -------         SYNC GENERATION                 ------
 ------------------------------------------------------
dina(0) <= '1';
 
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg = (H_MAX - 1)) then
        h_cntr_reg <= (others =>'0');
      else
        h_cntr_reg <= h_cntr_reg + 1;
      end if;
    end if;
  end process;
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if ((h_cntr_reg = (H_MAX - 1)) and (v_cntr_reg = (V_MAX - 1))) then
        v_cntr_reg <= (others =>'0');
      elsif (h_cntr_reg = (H_MAX - 1)) then
        v_cntr_reg <= v_cntr_reg + 1;
      end if;
    end if;
  end process;
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg >= (H_FP + FRAME_WIDTH - 1)) and (h_cntr_reg < (H_FP + FRAME_WIDTH + H_PW - 1)) then
        h_sync_reg <= H_POL;
      else
        h_sync_reg <= not(H_POL);
      end if;
    end if;
  end process;
  
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (v_cntr_reg >= (V_FP + FRAME_HEIGHT - 1)) and (v_cntr_reg < (V_FP + FRAME_HEIGHT + V_PW - 1)) then
        v_sync_reg <= V_POL;
      else
        v_sync_reg <= not(V_POL);
      end if;
    end if;
  end process;
  
  
active <= '1' when ((h_cntr_reg < FRAME_WIDTH) and (v_cntr_reg < FRAME_HEIGHT))else
            '0';

h <= to_integer(unsigned(h_cntr_reg));
v <= to_integer(unsigned(v_cntr_reg));

addrb <= std_logic_vector( to_unsigned(h + v * 1024, addrb'length)) when active = '1' else (others => '0');  
  
  -- Read the pixels at X,Y
  process (pxl_clk) 
  begin
    if(rising_edge(pxl_clk)) then
        if(h_cntr_reg < 1024) AND (active = '1') then
            vga_red <= doutb(0) & "000";
            vga_blue <= doutb(0) & doutb(0) & "00";
            vga_green <= doutb(0) & "000";
        else
            vga_red <= (others => '0');
            vga_blue <= (others => '0');
            vga_green <= (others => '0');
        end if;
    end if;
  end process;

  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      v_sync_dly_reg <= v_sync_reg;
      h_sync_dly_reg <= h_sync_reg;
      vga_red_reg <= vga_red;
      vga_green_reg <= vga_green;
      vga_blue_reg <= vga_blue;
    end if;
  end process;

  VGA_HS_O <= h_sync_dly_reg;
  VGA_VS_O <= v_sync_dly_reg;
  VGA_R <= vga_red_reg;
  VGA_G <= vga_green_reg;
  VGA_B <= vga_blue_reg;

end Behavioral;