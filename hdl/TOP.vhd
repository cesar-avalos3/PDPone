library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

entity TOP is
    port( clk, rst: in std_logic;
          led: out std_logic_vector(0 to 3);
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
      port(CLK_I : in  STD_LOGIC;
           x, y: in natural;
           VGA_HS_O : out  STD_LOGIC;
           VGA_VS_O : out  STD_LOGIC;
           VGA_R : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_B : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_G : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

signal ac, io, cy, instruction, ac_reg, ac_alu, io_reg, cy_reg, result: std_logic_vector(0 to 17);
signal overflow, skip: std_logic;

type STATE is (RESET, RUNNING_LOW, RUNNING_HIGH, WAITAMINUTE, INDIRECT);
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


-- Extended Instructions
constant iot                    : opcode_t := "11101";

-- IOT Addresses
subtype address_iot is std_logic_vector(0 to 5);
constant iot_display_screen: address_iot := "000111"; -- 7

signal STACK: std_logic_vector(0 to 17) := (others => '0');
signal PC: natural := 0;
-- It's gonna be 4096 Words of 18-bits each
type INST_MEM is array(0 to 200) of std_logic_vector(0 to 17); --Around 4K of working memory
signal instruction_memory : INST_MEM := (0 => load_ac_n  & '0' & 12x"1", 
                                         1 => deposit_ac & '0' & 12x"0", 
                                         2 => load_ac_n  & '0' & 12x"9",
                                         3 => add        & '0' & 12x"0",
                                         4 => iot        & 7x"0" & iot_display_screen,
                                         5 => jump       & '0' & 12x"3",
                                         others => "100000000000000000");
-- It's gonna be 4096 Words of 18-bits each
type WORK_MEM is array(0 to 133) of std_logic_vector(0 to 17);
signal work_memory : WORK_MEM := (0 => "000000000000000001", others => (others => '0'));

begin

ALU_I: ALU port map( clk => clk, rst => rst, ac => ac, io => io, cy => cy, operation => opcode, 
                     overflow => overflow, skip => skip, ac_next => ac_alu, io_next => io_reg, cy_next => cy_reg);

VGA_I: VGA port map(clk_i => clk, x => x, y => y, VGA_HS_O => VGA_HS_O, VGA_VS_O => VGA_VS_O, VGA_R => VGA_R, VGA_G => VGA_G, VGA_B => VGA_B);

verbose_opcode <= opcode;

memory_location <= instruction(6 to 17);

ac_reg_toggle <= '1' when opcode = "10000" else '0';
ac_reg <= "000000" & memory_location when opcode = "11100" else "000000" & not memory_location when opcode = "11011" else ac_reg;

deposit_mask <= "111111111111111111" when opcode = deposit_ac else
                "000000000111111111" when opcode = deposit_address else
                "111111000000000000" when opcode = deposit_instruction else
                "111111111111111111";

io_reg_toggle <= '1' when opcode = "01001" else '0';

cy <= work_memory(to_integer(unsigned(memory_location))) when ac_reg_toggle = '1' else cy;

-- One's adder
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

---- Instruction Decoder
--process(instruction) begin
--    case opcode begin
--        -- Arithmetic Instructions
--        when Add =>
--
--        when Subtract =>
--
--        when Multiply_Step =>
--        
--        when Divide_Step =>
--        
--        when Index =>
--        
--        when Index_and_Skip =>
--
--    end case;
--end process;

-- Shifter
process(instruction) 
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
                    case opcode is
                        when Add | Subtract | Multiply_Step | Divide_Step | Index | Index_and_Skip | and_i | eor_i | xor_i =>
                            ac <= ac_alu;
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
                        when iot =>
                            case iot_memory_address is
                                when iot_display_screen => --DPY
                                    -- Draw onto screen, bits 0 to 9 of AC is the X coordinates (signed)
                                    --                   bits 0 to 9 of IO is the Y coordinates (signed)
                                    --x <= to_integer(signed(ac(0 to 9))) + 513;
                                    x <= to_integer(signed(ac(8 to 17))) + 513;
                                    y <= to_integer(signed(ac(0 to 9))) + 513;
                                when others =>                      
                            end case;
                        when others =>
                    end case;
                    
                    current_state <= RUNNING_HIGH;
                when RUNNING_HIGH =>
                 current_state <= RESET;

                 -- Shifts
                 
                when OTHERS =>
                    current_state <= RESET;
            end case;
        end if;
    end if;
    
end process;

end RTL;