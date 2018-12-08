library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

entity ALU is
    port(clk,rst: in std_logic;
         ac, io, cy: in std_logic_vector(0 to 17);
         operation: in std_logic_vector(0 to 4);
         overflow, skip: out std_logic;
         ac_next, io_next, cy_next: out std_logic_vector(0 to 17));
end ALU;

architecture RTL of ALU is

constant overflow_limit : integer := 2 ** 18 / 2;

begin

process(ac, io, cy, operation) 
    variable temp_res : std_logic_vector(0 to 18);
    begin 
        overflow <= '0';
        skip <= '0';
        ac_next <= ac;
    case operation is
        when "10000" =>
            temp_res := std_logic_vector(signed(ac(0) & ac) + signed(cy(0) & cy));
            if(signed(temp_res) > overflow_limit or signed(temp_res) < (-1 * overflow_limit)) then
                overflow <= '1';
            end if;
            ac_next <= temp_res(1 to 18);
        when "10001" =>
            temp_res := std_logic_vector(signed(ac) - signed(cy));
            if(signed(temp_res) > 4095 or signed(temp_res) < -4095) then
                overflow <= '1';
            end if;
            ac_next <= temp_res(1 to 18);
        when "10110" =>
            if(io(17) = '1') then
                temp_res := std_logic_vector(signed(ac) + signed(cy));
            end if;
            ac_next <= '0' & temp_res(0 to 16);
            io_next <= '0' & io(0 to 16);
        when "10111" =>
            if(io(17) = '1') then
                temp_res := std_logic_vector(signed(ac) - signed(cy));
            else
                temp_res := std_logic_vector(signed(ac) + signed(cy) + 1);
            end if;
            ac_next <= temp_res(0 to 17);
            io_next(17)<= not ac(0);
        when "11010" =>
            cy_next <= std_logic_vector(signed(cy) + 1);
            ac_next <= std_logic_vector(signed(cy) + 1);
        when "11011" =>
            cy_next <= std_logic_vector(signed(cy) + 1);
            ac_next <= std_logic_vector(signed(cy) + 1);
            if(signed(cy) + 1 > 0) then
                skip <= '1';
            end if;
        when "00001" =>
            ac_next <= ac AND cy;
        when "00011" =>
            ac_next <= ac XOR cy;
        when "00010" =>
            ac_next <= ac OR cy;
        when others =>
    end case;        
end process;
end architecture;