library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

entity TOP is
    port( clk, rst: in std_logic;
          btns: in std_logic_vector(0 to 3);
          led: out std_logic_vector(0 to 3);
          rgb_led: out std_logic_vector(5 downto 0);
          VGA_HS_O : out  STD_LOGIC;
          VGA_VS_O : out  STD_LOGIC;
          VGA_R : out  STD_LOGIC_VECTOR (3 downto 0);
          VGA_B : out  STD_LOGIC_VECTOR (3 downto 0);
          VGA_G : out  STD_LOGIC_VECTOR (3 downto 0));
end TOP;


architecture RTL of TOP is

component ALU is
    port(clk,rst: in std_logic;
     ac, io, cy: in std_logic_vector(0 to 17);
     operation: in std_logic_vector(0 to 4);
     overflow, skip: out std_logic;
     ac_next, io_next, cy_next: out std_logic_vector(0 to 17));
end component;

component VGA is
      port(CLK_I,pxl_clk, rst: in  STD_LOGIC;
           x, y: in natural;
           write_to_VRAM: in std_logic;
           wait_for_clear: out std_logic;
           VGA_HS_O : out  STD_LOGIC;
           VGA_VS_O : out  STD_LOGIC;
           VGA_R : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_B : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_G : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

component clk_wiz_0
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic
 );
end component;

signal ac, io, cy, instruction, ac_reg, ac_alu, io_reg, cy_reg, result, indirect_register: std_logic_vector(0 to 17);
signal overflow, skip: std_logic;

alias btn : std_logic is btns(0);
alias keys: std_logic_vector(0 to 2) is btns(1 to 3);

type STATE is (RESET, RUNNING_LOW, RUNNING_HIGH, WAITFORINPUT, INDIRECT_FETCH, INDIRECT_DONE);
signal current_state : STATE := RESET;

signal ac_reg_toggle,deposit_toggle,io_reg_toggle : std_logic := '0';

alias opcode : std_logic_vector(0 to 4) is instruction(0 to 4);
alias indirect_bit : std_logic is instruction(5);
alias shift_amount: std_logic_vector(0 to 8) is instruction(9 to 17);
alias iot_memory_address: std_logic_vector(0 to 5) is instruction(12 to 17);
alias iot_wait: std_logic_vector(0 to 1) is instruction(5 to 6);
alias iot_control: std_logic_vector(0 to 4) is instruction(7 to 11);


signal shift_amount_integer: std_logic_vector(0 to 8) := (others => '0');
signal memory_location : std_logic_vector(0 to 11);
signal deposit_mask : std_logic_vector(0 to 17);

signal shift_reg_ac, shift_reg_io : std_logic_vector(0 to 17);

signal x, y: natural;

subtype opcode_t is std_logic_vector(0 to 4); 
signal verbose_opcode : opcode_t;

-- Opcodes
-- Arithmetic Instructions
constant Add            : opcode_t := "10000"; -- ac = cy + ac
constant Subtract       : opcode_t := "10001"; -- ac = ac - cy
constant Multiply_Step  : opcode_t := "10110"; -- 
constant Divide_Step    : opcode_t := "10111"; --
constant Index          : opcode_t := "10010"; -- cy++   ac = cy++
constant Index_and_Skip : opcode_t := "10011"; -- cy++   ac = cy++  if cy++ > 0 -> PC + 1

-- Logical Instructions
constant and_i : opcode_t := "00001";
constant eor_i : opcode_t := "00010";
constant xor_i : opcode_t := "00011";

-- General Instructions
constant load_ac                : opcode_t := "01000"; -- ac = cy
constant deposit_ac             : opcode_t := "01010"; -- cy = ac
constant deposit_address        : opcode_t := "01011"; -- cy(6 to 17) = ac (6 to 17)
constant deposit_instruction    : opcode_t := "01100"; -- cy(0 to 5) = ac(0 to 5)
constant load_io                : opcode_t := "01001"; -- io = cy
constant deposit_io             : opcode_t := "01101"; -- cy = io
constant deposit_zero           : opcode_t := "01110"; -- cy = +0
constant execute                : opcode_t := "00100"; -- next_instruction = cy, pc remains unchanged
constant jump                   : opcode_t := "11000"; -- PC = cy
constant jump_and_save_pc       : opcode_t := "11001"; -- PC = cy  ac = PC
constant call_subroutine_and_jmp: opcode_t := "00111"; -- cy[100] = ac  cy[101] = PC
constant skip_if_ac_neq_y       : opcode_t := "10100"; -- if cy != ac -> PC+2
constant skip_if_ac_eq_y        : opcode_t := "10101"; -- if cy == ac -> PC+2

-- Augmented Instructions
constant load_ac_n              : opcode_t := "11100"; -- Let N be the memory address bit field in the instruction, 
                                                       -- if indirect bit is 1, ac = -N, else ac = N
constant shift_group            : opcode_t := "11011";


-- 11010          1/0           000000000010
-- Opcode   If 1 eq is false    
-- Skip Instructions
constant skip_group             : opcode_t := "11010";

subtype skip_address is std_logic_vector(0 to 11);
constant skip_on_zero_ac        : skip_address := 12o"0100";
constant skip_on_plus_ac        : skip_address := 12o"0200";
constant skip_on_mins_ac        : skip_address := 12o"0400";
constant skip_on_zero_overflow  : skip_address := 12o"1000";
constant skip_on_plus_io        : skip_address := 12o"2000";
constant skip_on_zero_switch_1  : skip_address := 12o"0010";
constant skip_on_zero_switch_2  : skip_address := 12o"0020";
constant skip_on_zero_switch_3  : skip_address := 12o"0030";
constant skip_on_zero_switch_4  : skip_address := 12o"0040";
constant skip_on_zero_switch_5  : skip_address := 12o"0050";
constant skip_on_zero_switch_6  : skip_address := 12o"0060";
constant skip_on_zero_switch_7  : skip_address := 12o"0070";
constant skip_on_zero_pflag_1   : skip_address := 12o"0001";
constant skip_on_zero_pflag_2   : skip_address := 12o"0002";
constant skip_on_zero_pflag_3   : skip_address := 12o"0003";
constant skip_on_zero_pflag_4   : skip_address := 12o"0004";
constant skip_on_zero_pflag_5   : skip_address := 12o"0005";
constant skip_on_zero_pflag_6   : skip_address := 12o"0006";
constant skip_on_zero_pflag_7   : skip_address := 12o"0007";

-- Extended Instructions
constant iot                    : opcode_t := "11101";

constant operate_group          : opcode_t := "11111";
subtype address_operate is std_logic_vector(0 to 11);
constant clear_io               : address_operate := 12o"4000";
constant load_ac_from_test      : address_operate := 12o"2000";
constant load_ac_with_pc        : address_operate := 12o"0100";
constant complement_ac          : address_operate := 12o"1000";
constant halt                   : address_operate := 12o"0400";
constant clear_ac               : address_operate := 12o"0200";
constant clear_pflag_1          : address_operate := 12o"0001";
constant clear_pflag_2          : address_operate := 12o"0002";
constant clear_pflag_3          : address_operate := 12o"0003";
constant clear_pflag_4          : address_operate := 12o"0004";
constant clear_pflag_5          : address_operate := 12o"0005";
constant clear_pflag_6          : address_operate := 12o"0006";
constant clear_pflag_7          : address_operate := 12o"0007";
constant set_pflag_1            : address_operate := 12o"0011";
constant set_pflag_2            : address_operate := 12o"0012";
constant set_pflag_3            : address_operate := 12o"0013";
constant set_pflag_4            : address_operate := 12o"0014";
constant set_pflag_5            : address_operate := 12o"0015";
constant set_pflag_6            : address_operate := 12o"0016";
constant set_pflag_7            : address_operate := 12o"0017";
constant nop                    : address_operate := 12o"0000";

-- IOT Addresses
subtype address_iot is std_logic_vector(0 to 5);
constant iot_display_screen: address_iot  := "000111"; -- 7
constant iot_display_rgb_led: address_iot := "000011"; -- Custom display LED function
constant iot_get_key_down: address_iot    := "000001";

signal STACK: std_logic_vector(0 to 17) := (others => '0');
signal PC: natural := 0;
signal write_to_VRAM: std_logic := '0';
signal arithmetic_unit_in_use: std_logic := '0';
signal completion_pulse : std_logic := '0';
-- It's gonna be 4096 Words of 18-bits each
type INST_MEM is array(0 to 50) of std_logic_vector(0 to 17); --Around 4K of working memory
signal instruction_memory : INST_MEM := (0 => load_ac_n         & '0'   & 12x"1",  -- ac = 1
                                         1 => deposit_ac        & '0'   & 12x"0",  -- m[0] = 1
                                         2 => load_ac_n         & '0'   & 12x"3C", -- ac = width of the box
                                         3 => deposit_ac        & '0'   & 12x"5",  -- m[5] = width of the box
                                         4 => load_ac_n         & '0'   & 12x"10", -- ac = 10
                                         5 => deposit_ac        & '0'   & 12x"1",  -- m[1] = 10 -> io = 10
                                         6 => load_io           & '0'   & 12x"1",  -- io = 1
                                         -- Outer loop starts here
                                         7 => load_ac           & '0'   & 12x"10", -- ac = m[10]
                                         8 => iot               & 7x"0" & iot_display_screen, -- Render Screen
                                         9 => add               & '0'   & 12x"0",  -- ac += m[0] -> ac++
                                        10 => skip_if_ac_eq_y   & '0'   & 12x"5",  -- if(ac == m[5]) skip jump
                                        11 => jump              & '0'   & 12x"8",  -- goto Render Screen
                                        12 => load_ac           & '0'   & 12x"1",  -- ac = m[1] = io
                                        13 => add               & '0'   & 12x"0",  -- Add 1 to io
                                        14 => skip_if_ac_neq_y  & '0'   & 12x"5",  -- if(io != m[5]) skip jump , if(ac != 5)
                                        15 => jump              & '0'   & 12d"19",  -- if(io == m[5]) then restart from 0
                                        16 => deposit_ac        & '0'   & 12x"1",  -- m[1] = ac = io
                                        17 => load_io           & '0'   & 12x"1",  -- io = m[1]
                                        18 => jump              & '0'   & 12x"7",  -- jump back to Render Screen
                                        -- Handle control once things are drawn
                                        19 => iot               & 7x"0" & iot_get_key_down,
                                        20 => deposit_ac        & '0'   & 12x"3",  -- m[3] = ac
                                        21 => deposit_io        & '0'   & 12x"4",  -- m[4] = status of button 2, 1 is down 0 is up
                                        22 => load_ac           & '0'   & 12x"4",
                                        23 => skip_if_ac_eq_y   & '0'   & 12x"0",  -- if ac == 1 then skip the following instruction
                                        24 => jump              & '0'   & 12x"4",
                                        25 => load_ac_n         & '0'   & 12x"5",  -- ac = 5
                                        26 => add               & '0'   & 12x"5",  -- ac = width + 5
                                        27 => deposit_ac        & '0'   & 12x"5",  -- width = width + 5
                                        28 => load_ac_n         & '0'   & 12x"5",
                                        29 => add               & '0'   & 12x"10",
                                        30 => deposit_ac        & '0'   & 12x"10",
                                        31 => load_ac           & '0'   & 12x"3",  -- restore ac to whatever it was before we ran this function
                                        32 => jump              & '0'   & 12x"4",
                                         others => "100000000000000000");
-- It's gonna be 4096 Words of 18-bits each
type WORK_MEM is array(0 to 102) of std_logic_vector(0 to 17);
signal work_memory : WORK_MEM := (0 => "000000000000000001", others => (others => '0'));
signal pxl_clk,wait_for_clear: std_logic;
begin

ALU_I: ALU port map( clk => clk, rst => rst, ac => ac, io => io, cy => cy, operation => opcode, 
                     overflow => overflow, skip => skip, ac_next => ac_alu, io_next => io_reg, cy_next => cy_reg);

VGA_I: VGA port map(clk_i => clk,pxl_clk => pxl_clk,rst =>rst, wait_for_clear => wait_for_clear,x => x, y => y, write_to_VRAM => write_to_VRAM, VGA_HS_O => VGA_HS_O, VGA_VS_O => VGA_VS_O, VGA_R => VGA_R, VGA_G => VGA_G, VGA_B => VGA_B);



clk_div_inst : clk_wiz_0
  port map
   (-- Clock in ports
    CLK_IN1 => clk,
    -- Clock out ports
    CLK_OUT1 => pxl_clk);



-- ## TODO ## --
-- Clean up the source muxes
verbose_opcode <= opcode;
arithmetic_unit_in_use <= '1' when opcode = Add or opcode =  Subtract or opcode = Multiply_Step or opcode = Divide_Step or opcode = Index or opcode = Index_and_Skip or opcode = and_i or opcode =  eor_i or opcode = xor_i else '0';
memory_location <= instruction(6 to 17);

ac_reg_toggle <= '1' when arithmetic_unit_in_use = '1' else '0';
ac_reg <= "000000" & memory_location when opcode = "11100" else "000000" & not memory_location when opcode = "11011" else ac_reg;

deposit_mask <= "111111111111111111" when opcode = deposit_ac else
                "000000000111111111" when opcode = deposit_address else
                "111111000000000000" when opcode = deposit_instruction else
                "111111111111111111";

io_reg_toggle <= '1' when opcode = "01001" else '0';


-- Handling the single button press now

process(clk) begin
    if(rising_edge(clk)) then
            completion_pulse <= '0';
        if(current_state = WAITFORINPUT and (keys(0) = '1' or keys(1) = '1' or keys(2) = '1')) then
            completion_pulse <= '1';
        end if;
    end if;
end process;

cy <= work_memory(to_integer(unsigned(memory_location))) when ac_reg_toggle = '1' else cy;

-- Add the amount of 1's in the shift field
-- Used in shift functions
process(shift_amount) 
    variable temp_reg: unsigned(0 to 8);
    begin
    temp_reg := (others => '0');
    for i in 0 to 8 loop
        if(shift_amount(i) = '1') then
            temp_reg := temp_reg + 1;
        end if;
    end loop;
    shift_amount_integer <= std_logic_vector(temp_reg);
end process;

-- Shifter
process(instruction,ac, io) 
    variable rot_temp : std_logic_vector(0 to 35);
    begin
    shift_reg_ac <= ac;
    shift_reg_io <= io;
    if(opcode = shift_group) then
        if(instruction(5) = '1') then -- Right shift
            if(instruction(6) = '1') then -- Logical Shift
                if(instruction(7 to 8) = "01") then -- AC
                    shift_reg_ac <= ac ror to_integer(unsigned(shift_amount));
                elsif(instruction(7 to 8) = "10") then
                    shift_reg_io <= io ror to_integer(unsigned(shift_amount));
                else
                    rot_temp := ac & io;
                    rot_temp := rot_temp ror to_integer(unsigned(shift_amount));
                    shift_reg_ac <= rot_temp(0 to 17);
                    shift_reg_io <= rot_temp(18 to 35);
                end if;
            else -- Arithmetic shift
                if(instruction(7 to 8) = "01") then
                    shift_reg_ac <= std_logic_vector(shift_right(unsigned(ac),to_integer(unsigned(shift_amount))));
                elsif(instruction(7 to 8) = "10") then
                    shift_reg_io <= std_logic_vector(shift_right(unsigned(io),to_integer(unsigned(shift_amount))));
                else
                    rot_temp := ac & io;
                    rot_temp := std_logic_vector(shift_right(unsigned(rot_temp),to_integer(unsigned(shift_amount))));
                    shift_reg_ac <= rot_temp(0 to 17);
                    shift_reg_io <= rot_temp(18 to 35);
                end if;
            end if;
        else -- Left Shift
            if(instruction(6) = '1') then -- Logical Shift
                if(instruction(7 to 8) = "01") then -- AC
                    shift_reg_ac <= ac rol to_integer(unsigned(shift_amount));
                elsif(instruction(7 to 8) = "10") then
                    shift_reg_io <= io rol to_integer(unsigned(shift_amount));
                else
                    rot_temp := ac & io;
                    rot_temp := rot_temp rol to_integer(unsigned(shift_amount));
                    shift_reg_ac <= rot_temp(0 to 17);
                    shift_reg_io <= rot_temp(18 to 35);
                end if;
            else -- Arithmetic shift
                if(instruction(7 to 8) = "01") then
                    shift_reg_ac <= std_logic_vector(shift_left(unsigned(ac),to_integer(unsigned(shift_amount))));
                elsif(instruction(7 to 8) = "10") then
                    shift_reg_io <= std_logic_vector(shift_left(unsigned(io),to_integer(unsigned(shift_amount))));
                else
                    rot_temp := ac & io;
                    rot_temp := std_logic_vector(shift_left(unsigned(rot_temp),to_integer(unsigned(shift_amount))));
                    shift_reg_ac <= rot_temp(0 to 17);
                    shift_reg_io <= rot_temp(18 to 35);
                    end if;
                end if;
        end if;
    end if;
end process;

process(clk, rst)
    variable rot_temp : std_logic_vector(0 to 35);
    begin
    if(rising_edge(clk)) then
        if(rst = '1') then
            ac <= (others => '0');
            io <= (others => '0');
            PC <= 0;
            x  <= 0;
            y  <= 0;
          --  ac_reg <= (others => '0');
            --memory <= (others => (others => '0'));
            current_state <= RESET;
        else
            case current_state is
                when RESET =>
                    current_state <= RUNNING_LOW;

                    -- Fetch the instruction
                    instruction <= instruction_memory(PC);

                    -- Add pc immediately
                    PC <= PC + 1;
                when RUNNING_LOW =>
                    write_to_VRAM <= '0';
                    current_state <= RUNNING_HIGH;
                    case opcode is
                        when Add | Subtract | Multiply_Step | Divide_Step | Index | Index_and_Skip | and_i | eor_i | xor_i =>
                            if(indirect_bit = '1') then
                                current_state <= INDIRECT_FETCH;
                                indirect_register <= instruction;
                            else
                                ac <= ac_alu;
                            end if;
                        when load_ac =>
                            ac <= work_memory(to_integer(unsigned(memory_location)));
                        when deposit_ac =>
                            work_memory(to_integer(unsigned(memory_location))) <= ac;
                        when deposit_address | deposit_instruction =>
                            work_memory(to_integer(unsigned(memory_location))) <= ac and deposit_mask;
                        when load_io =>
                            io <= work_memory(to_integer(unsigned(memory_location)));
                        when deposit_io =>
                            work_memory(to_integer(unsigned(memory_location))) <= io;
                        when deposit_zero =>
                            work_memory(to_integer(unsigned(memory_location))) <= (others => '0');
                        when execute =>
                            instruction <= instruction_memory(to_integer(unsigned(memory_location))); 
                        when jump =>
                            PC <= to_integer(unsigned(memory_location)); 
                        when jump_and_save_pc =>
                            PC <= to_integer(unsigned(memory_location)); 
                            ac <= std_logic_vector(to_unsigned(PC, ac'length));
                        when call_subroutine_and_jmp =>
                            work_memory(99) <= ac; -- Memory location #100
                            ac <= std_logic_vector(to_unsigned(PC, ac'length));
                            PC <= to_integer(unsigned(work_memory(100)));
                            if(indirect_bit = '1') then
                                work_memory(to_integer(unsigned(memory_location))) <= ac;
                                PC <= to_integer(unsigned(work_memory(100))) + 1;
                            end if;
                        when skip_if_ac_neq_y =>
                            if(work_memory(to_integer(unsigned(memory_location))) /= ac) then
                                PC <= PC + 1;
                            end if;
                        when skip_if_ac_eq_y =>
                            if(work_memory(to_integer(unsigned(memory_location))) = ac) then
                                PC <= PC + 1;
                            end if;
                        when load_ac_n =>
                            ac <= "000000" & memory_location;
                            if(indirect_bit = '1') then
                                ac <= "111111" & not memory_location;
                            end if;
                        when shift_group =>
                            ac <= shift_reg_ac;
                            io <= shift_reg_io;
                        when operate_group =>
                            case memory_location is
                                when clear_ac =>
                                    ac <= (others => '0');
                                when complement_ac =>
                                    ac <= not ac;
                                when clear_io =>
                                    io <= ( others => '0');
                                when load_ac_with_pc =>
                                    ac <= std_logic_vector(to_unsigned(PC, ac'length));
                                when others =>
                            end case;
                        when iot =>
                            if(iot_wait = "01") then
                                current_state <= WAITFORINPUT;
                            end if;
                            case iot_memory_address is
                                when iot_display_screen => --DPY
                                    -- Draw onto screen, bits 0 to 9 of AC is the X coordinates (signed)
                                    --                   bits 0 to 9 of IO is the Y coordinates (signed)
                                    --x <= to_integer(signed(ac(0 to 9))) + 513;
                                    if(wait_for_clear = '0') then
                                        x <= to_integer(signed(ac(8 to 17))) + 513; -- For testing purposes we'll be using the lower 8 to 17 bits
                                        y <= to_integer(signed(io(8 to 17))) + 513;
                                        write_to_VRAM <= '1';
                                    else
                                        current_state <= RUNNING_LOW;
                                    end if;
                                when iot_display_rgb_led =>
                                    rgb_led(2 downto 0) <= ac(17) & ac(16) & ac(15);
                                    rgb_led(5 downto 3) <= io(17) & io(16) & io(15);
                                when iot_get_key_down =>
                                    if(btns(2) = '1') then
                                        io <= (17 => '1', others => '0');
                                    else
                                        io <= (others => '0');
                                    end if;
                                when others =>                      
                            end case;
                        when skip_group =>
                            case memory_location is
                                when skip_on_zero_ac =>
                                    if( (unsigned(ac) = 0) or (ac = "1000000000000000000") ) then
                                        PC <= PC + 1;
                                    end if;
                                when skip_on_plus_ac =>
                                    if(signed(ac) > 1) then -- Need to account for the 1's complement issue
                                        PC <= PC + 1;
                                    end if;
                                when skip_on_mins_ac =>
                                    if(signed(io) < 0) then
                                        PC <= PC + 1;
                                    end if;
                                when skip_on_zero_overflow =>
                                    if(overflow = '1' ) then
                                        PC <= PC + 1;    
                                    end if;
                                when skip_on_plus_io =>
                                    if(signed(io) > 1) then -- Need to account for the 1's complement issue
                                        PC <= PC + 1;
                                    end if;
                                -- The rest of the switches and programflags
                                when others =>
                            end case;
                        when others =>
                    end case;
                    
                when RUNNING_HIGH =>
                 if(btn = '1' or btns(3) = '1') then
                    current_state <= RESET;
                 end if;
                 write_to_VRAM <= '0';
                 -- Shifts
                when WAITFORINPUT =>
                    -- Need to figure out how to handle this
                    if(completion_pulse = '1') then
                        current_state <= RUNNING_HIGH;
                    end if;
                when INDIRECT_FETCH =>
                    indirect_register <= work_memory(to_integer(unsigned(indirect_register(6 to 17))));
                    current_state <= INDIRECT_DONE;
                when INDIRECT_DONE =>
                    if(indirect_register(5) = '0') then
                        current_state <= RUNNING_LOW;
                        instruction(5 to 17) <= '0' & indirect_register(6 to 17);
                    else
                        current_state <= INDIRECT_FETCH;
                    end if;
                when OTHERS =>
                    current_state <= RESET;
            end case;
        end if;
    end if;
    
end process;
    led <= std_logic_vector(to_unsigned(PC, led'length));
end RTL;