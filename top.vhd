library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity display_top is
    Port ( mclk : in  STD_LOGIC;
           btn : in  STD_LOGIC_VECTOR (3 downto 0);  -- Kept original btn interface
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           red : out  STD_LOGIC_VECTOR (3 downto 0);
           green : out  STD_LOGIC_VECTOR (3 downto 0);
           blue : out  STD_LOGIC_VECTOR (3 downto 0));
end display_top;

architecture Behavioral of display_top is

-- Original component declarations remain the same
component clk_generator
    Port ( clk100_in : in  STD_LOGIC;
           sel : in STD_LOGIC_VECTOR(1 downto 0);
           pixel_clk : out  STD_LOGIC);
end component;

-- Keep all your original VGA components
component vga_640x480
    Port ( clk : in  STD_LOGIC;
           clr : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hc : out  STD_LOGIC_VECTOR (10 downto 0);
           vc : out  STD_LOGIC_VECTOR (10 downto 0);
           vidon : out  STD_LOGIC);
end component;

component vga_1024x768
    Port ( clk : in  STD_LOGIC;
           clr : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hc : out  STD_LOGIC_VECTOR (10 downto 0);
           vc : out  STD_LOGIC_VECTOR (10 downto 0);
           vidon : out  STD_LOGIC);
end component;

component vga_1280x1024
    Port ( clk : in  STD_LOGIC;
           clr : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hc : out  STD_LOGIC_VECTOR (10 downto 0);
           vc : out  STD_LOGIC_VECTOR (10 downto 0);
           vidon : out  STD_LOGIC);
end component;

component vga_output
    Port ( vidon : in  STD_LOGIC;
           hc : in  STD_LOGIC_VECTOR (10 downto 0);
           vc : in  STD_LOGIC_VECTOR (10 downto 0);
           res_sel : in STD_LOGIC_VECTOR(1 downto 0);
           red : out  STD_LOGIC_VECTOR (3 downto 0);
           green : out  STD_LOGIC_VECTOR (3 downto 0);
           blue : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

-- Internal signals (keep original signals and add new ones)
signal pixel_clk, vidon : std_logic;
signal hc, vc : std_logic_vector(10 downto 0);
signal res_sel : std_logic_vector(1 downto 0);
signal hsync_640, hsync_1024, hsync_1280 : std_logic;
signal vsync_640, vsync_1024, vsync_1280 : std_logic;
signal vidon_640, vidon_1024, vidon_1280 : std_logic;
signal hc_640, hc_1024, hc_1280 : std_logic_vector(10 downto 0);
signal vc_640, vc_1024, vc_1280 : std_logic_vector(10 downto 0);

-- New signals for button handling
signal btn_prev : std_logic_vector(3 downto 0) := (others => '0');
signal debounce_counter : integer range 0 to 1000000 := 0;
constant DEBOUNCE_LIMIT : integer := 1000000; -- Adjust based on your clock frequency

begin

-- Button debouncing and resolution selection process
process(mclk)
begin
    if rising_edge(mclk) then
        -- Debounce counter
        if debounce_counter = DEBOUNCE_LIMIT then
            debounce_counter <= 0;
            
            -- Button 0 increases resolution
            if btn(0) = '1' and btn_prev(0) = '0' then
                case res_sel is
                    when "00" => res_sel <= "01";  -- 640x480 -> 1024x768
                    when "01" => res_sel <= "10";  -- 1024x768 -> 1280x1024
                    when others => res_sel <= "10"; -- Stay at max
                end case;
            end if;
            
            -- Button 1 decreases resolution
            if btn(1) = '1' and btn_prev(1) = '0' then
                case res_sel is
                    when "10" => res_sel <= "01";  -- 1280x1024 -> 1024x768
                    when "01" => res_sel <= "00";  -- 1024x768 -> 640x480
                    when others => res_sel <= "00"; -- Stay at min
                end case;
            end if;
            
            -- Reset to 640x480 with button 3
            if btn(3) = '1' and btn_prev(3) = '0' then
                res_sel <= "00";
            end if;
            
            -- Update previous button state
            btn_prev <= btn;
            
        else
            debounce_counter <= debounce_counter + 1;
        end if;
    end if;
end process;

-- Clock generator instance (remains the same)
clk_gen: clk_generator
    port map(
        clk100_in => mclk,
        sel => res_sel,
        pixel_clk => pixel_clk
    );

-- VGA module instances (modified to use same clear signal)
vga_640: vga_640x480
    port map(
        clk => pixel_clk,
        clr => btn(3),
        hsync => hsync_640,
        vsync => vsync_640,
        hc => hc_640,
        vc => vc_640,
        vidon => vidon_640
    );

vga_1024: vga_1024x768
    port map(
        clk => pixel_clk,
        clr => btn(3),  -- Use same reset for all
        hsync => hsync_1024,
        vsync => vsync_1024,
        hc => hc_1024,
        vc => vc_1024,
        vidon => vidon_1024
    );

vga_1280: vga_1280x1024
    port map(
        clk => pixel_clk,
        clr => btn(3),  -- Use same reset for all
        hsync => hsync_1280,
        vsync => vsync_1280,
        hc => hc_1280,
        vc => vc_1280,
        vidon => vidon_1280
    );

-- Resolution multiplexer (remains the same)
process(res_sel, hsync_640, hsync_1024, hsync_1280,
        vsync_640, vsync_1024, vsync_1280,
        vidon_640, vidon_1024, vidon_1280,
        hc_640, hc_1024, hc_1280,
        vc_640, vc_1024, vc_1280)
begin
    case res_sel is
        when "00" =>  -- 640x480
            hsync <= hsync_640;
            vsync <= vsync_640;
            vidon <= vidon_640;
            hc <= hc_640;
            vc <= vc_640;
        when "01" =>  -- 1024x768
            hsync <= hsync_1024;
            vsync <= vsync_1024;
            vidon <= vidon_1024;
            hc <= hc_1024;
            vc <= vc_1024;
        when others =>  -- 1280x1024
            hsync <= hsync_1280;
            vsync <= vsync_1280;
            vidon <= vidon_1280;
            hc <= hc_1280;
            vc <= vc_1280;
    end case;
end process;

-- VGA output instance (remains the same)
output: vga_output
    port map(
        vidon => vidon,
        hc => hc,
        vc => vc,
        res_sel => res_sel,
        red => red,
        green => green,
        blue => blue
    );

end Behavioral;


--------------------------------

-- Clock generator implementation
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity clk_generator is
    Port ( clk100_in : in  STD_LOGIC;
           sel : in STD_LOGIC_VECTOR(1 downto 0);
           pixel_clk : out  STD_LOGIC);
end clk_generator;

architecture Behavioral of clk_generator is

-- You'll need to implement this using your FPGA's clock management tiles
-- This is a simplified example
component clk_wiz
    port(
        clk_in : in std_logic;
        clk_25 : out std_logic;    -- 25.175 MHz for 640x480
        clk_65 : out std_logic;    -- 65 MHz for 1024x768
        clk_108 : out std_logic;   -- 108 MHz for 1280x1024
        reset : in std_logic;
        locked : out std_logic
    );
end component;

signal clk_25_MHz, clk_65_MHz, clk_108_MHz : std_logic;
signal locked : std_logic;

begin

-- Clock wizard instance
clk_wizard: clk_wiz
    port map(
        clk_in => clk100_in,
        clk_25 => clk_25_MHz,
        clk_65 => clk_65_MHz,
        clk_108 => clk_108_MHz,
        reset => '0',
        locked => locked
    );

-- Clock selection multiplexer
process(sel, clk_25_MHz, clk_65_MHz, clk_108_MHz)
begin
    case sel is
        when "00" =>   -- 640x480
            pixel_clk <= clk_25_MHz;
        when "01" =>   -- 1024x768
            pixel_clk <= clk_65_MHz;
        when others => -- 1280x1024
            pixel_clk <= clk_108_MHz;
    end case;
end process;

end Behavioral;

---------------------------------
-- 640x480 VGA Module
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity vga_640x480 is
    Port ( clk : in  STD_LOGIC;
           clr : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hc : out  STD_LOGIC_VECTOR (10 downto 0);
           vc : out  STD_LOGIC_VECTOR (10 downto 0);
           vidon : out  STD_LOGIC);
end vga_640x480;

architecture Behavioral of vga_640x480 is
    -- Total horizontal pixels = 800 = 640 + 16 + 96 + 48
    constant hpixels: std_logic_vector(10 downto 0) := "01100100000";  -- 800
    -- Total lines = 525 = 480 + 10 + 2 + 33
    constant vlines: std_logic_vector(10 downto 0) := "01000001101";   -- 525
    -- Horizontal timing
    constant hbp: std_logic_vector(10 downto 0) := "00010010000";      -- 144 (96 + 48)
    constant hfp: std_logic_vector(10 downto 0) := "01000100000";      -- 784 (144 + 640)
    -- Vertical timing
    constant vbp: std_logic_vector(10 downto 0) := "00000101101";      -- 35 (2 + 33)
    constant vfp: std_logic_vector(10 downto 0) := "00101001111";      -- 515 (35 + 480)

    signal hcs, vcs : std_logic_vector(10 downto 0);
    signal vsenable : std_logic;

begin
    -- Horizontal counter
    process(clk, clr)
    begin
        if(clr = '1')then
            hcs <= "00000000000";
        elsif(rising_edge(clk)) then
            if hcs = hpixels - 1 then
                hcs <= "00000000000";
                vsenable <= '1';
            else
                hcs <= hcs + 1;
                vsenable <= '0';
            end if;
        end if;
    end process;
    
    hsync <= '0' when hcs < 96 else '1';  -- Hsync pulse is 96 pixels

    -- Vertical counter
    process(clk, clr)
    begin
        if(clr = '1')then
            vcs <= "00000000000";
        elsif(rising_edge(clk) and vsenable='1') then
            if vcs = vlines - 1 then
                vcs <= "00000000000";
            else
                vcs <= vcs + 1;
            end if;
        end if;
    end process;
    
    vsync <= '0' when vcs < 2 else '1';  -- Vsync pulse is 2 lines

    vidon <= '1' when (((hcs < hfp) and (hcs >= hbp)) and
                      ((vcs < vfp) and (vcs >= vbp))) else '0';

    hc <= hcs;
    vc <= vcs;
end Behavioral;
----------------------
----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:00:34 10/26/2018 
-- Design Name: 
-- Module Name:    vga_1024x768 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity vga_1024x768 is
    Port ( clk : in  STD_LOGIC;
           clr : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hc : out  STD_LOGIC_VECTOR (10 downto 0);
           vc : out  STD_LOGIC_VECTOR (10 downto 0);
           vidon : out  STD_LOGIC);
end vga_1024x768;

architecture Behavioral of vga_1024x768 is


constant hpixels   : std_logic_vector(10 downto 0) := "10000000000";  -- 1024 pixels
	--Value of pixels in a horizontal line = 1344 = 1024 + 24 + 136 + 160
constant vlines: std_logic_vector(10 downto 0) := "01100100110";
	--Number of horizontal lines = 806 = 768 + 3 + 6 + 29
constant hbp: std_logic_vector(10 downto 0) := "00010100000";
	--Pixels to end of horizontal back porch = 296 = 136 + 160 
constant hfp: std_logic_vector(10 downto 0) := "01110000000";
	--Pixels in a horizontal front porch = 1320 = 136 + 160 + 1024 
constant vbp: std_logic_vector(10 downto 0) := "00000100011";
	--Pixels to end of vertical back porch = 35 = 29 + 6 
constant vfp: std_logic_vector(10 downto 0) := "01000010111";
	--Pixels in a vertical front porch = 803 = 6 + 29 + 768
signal hcs, vcs : std_logic_vector(10 downto 0);
   --Horizontal and vertical counters
signal vsenable : std_logic;
	--Enable for the vertical counter
	
begin
	--Counter for horizontal sync signal
	process(clk, clr)
	begin
		if(clr = '1')then
			hcs <= "00000000000";
		elsif(rising_edge(clk)) then
			if hcs = hpixels - 1 then
				-- Counter reached the end of the line
				hcs <= "00000000000"; --reset counter
				vsenable <= '1'; --Enable the vertical counter
			else
				hcs <= hcs + 1; --Increment horizontal pixel
				vsenable <= '0'; --Disable vertical counter
			end if;
		end if;
	end process;
	hsync <= '0' when hcs < 136 else '1';
		-- Horizontal sync Pulse is low when hcounter is in sync

	--Counter for vertical sync signal
	process(clk, clr)
	begin
		if(clr = '1')then
			vcs <= "00000000000";
		elsif(rising_edge(clk) and vsenable='1') then
			if vcs = vlines - 1 then
				-- Counter reached the end of the frame
				vcs <= "00000000000"; --reset counter
			else
				vcs <= vcs + 1; --Increment vertical line
			end if;
		end if;
	end process;
	vsync <= '0' when vcs < 6 else '1';
		-- Vertical sync Pulse is low when vcounter is in sync
		
	--Enable video out when within the horiz and vert porches
	vidon <= '1' when (((hcs < hfp) and (hcs >= hbp))
						and ((vcs < vfp) and (vcs >= vbp))) else '0';
						
	--Output horizontal and vertical counter
	hc <= hcs;
	vc <= vcs;

end Behavioral;

---------------------
-- 1280x1024 VGA Module
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity vga_1280x1024 is
    Port ( clk : in  STD_LOGIC;
           clr : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hc : out  STD_LOGIC_VECTOR (10 downto 0);
           vc : out  STD_LOGIC_VECTOR (10 downto 0);
           vidon : out  STD_LOGIC);
end vga_1280x1024;

architecture Behavioral of vga_1280x1024 is
    -- Total horizontal pixels = 1688 = 1280 + 48 + 112 + 248
    constant hpixels: std_logic_vector(10 downto 0) := "11010011000";  -- 1688
    -- Total lines = 1066 = 1024 + 1 + 3 + 38
    constant vlines: std_logic_vector(10 downto 0) := "10000101010";   -- 1066
    -- Horizontal timing
    constant hbp: std_logic_vector(10 downto 0) := "00101100000";      -- 360 (112 + 248)
    constant hfp: std_logic_vector(10 downto 0) := "11001100000";      -- 1640 (360 + 1280)
    -- Vertical timing
    constant vbp: std_logic_vector(10 downto 0) := "00000101001";      -- 41 (3 + 38)
    constant vfp: std_logic_vector(10 downto 0) := "10000101001";      -- 1065 (41 + 1024)

    signal hcs, vcs : std_logic_vector(10 downto 0);
    signal vsenable : std_logic;

begin
    -- Horizontal counter
    process(clk, clr)
    begin
        if(clr = '1')then
            hcs <= "00000000000";
        elsif(rising_edge(clk)) then
            if hcs = hpixels - 1 then
                hcs <= "00000000000";
                vsenable <= '1';
            else
                hcs <= hcs + 1;
                vsenable <= '0';
            end if;
        end if;
    end process;
    
    hsync <= '0' when hcs < 112 else '1';  -- Hsync pulse is 112 pixels

    -- Vertical counter
    process(clk, clr)
    begin
        if(clr = '1')then
            vcs <= "00000000000";
        elsif(rising_edge(clk) and vsenable='1') then
            if vcs = vlines - 1 then
                vcs <= "00000000000";
            else
                vcs <= vcs + 1;
            end if;
        end if;
    end process;
    
    vsync <= '0' when vcs < 3 else '1';  -- Vsync pulse is 3 lines

    vidon <= '1' when (((hcs < hfp) and (hcs >= hbp)) and
                      ((vcs < vfp) and (vcs >= vbp))) else '0';

    hc <= hcs;
    vc <= vcs;
end Behavioral;

----------------------------
----------------------------------------------------------------------------------
-----------------------
-- Modified VGA Output
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity vga_output is
    Port ( vidon : in  STD_LOGIC;
           hc : in  STD_LOGIC_VECTOR (10 downto 0);
           vc : in  STD_LOGIC_VECTOR (10 downto 0);
           res_sel : in STD_LOGIC_VECTOR(1 downto 0);
           red : out  STD_LOGIC_VECTOR (3 downto 0);
           green : out  STD_LOGIC_VECTOR (3 downto 0);
           blue : out  STD_LOGIC_VECTOR (3 downto 0));
end vga_output;

architecture Behavioral of vga_output is

component rom_inits
    Port ( addr : in  STD_LOGIC_VECTOR (5 downto 0);
           M_f : out  STD_LOGIC_VECTOR (0 to 31);
           M_m : out  STD_LOGIC_VECTOR (0 to 31);
           M_l : out  STD_LOGIC_VECTOR (0 to 31));
end component;

-- Resolution-dependent constants
signal hbp, vbp : std_logic_vector(10 downto 0);
signal x_offset, y_offset : std_logic_vector(10 downto 0);

signal addr : std_logic_vector(5 downto 0);
signal M_f, M_m, M_l : std_logic_vector(0 to 31);
signal spriteon_f, spriteon_m, spriteon_l, R, G, B: std_logic;
signal rom_addr, rom_pix : std_logic_vector(10 downto 0);
constant w: integer := 32;
constant h: integer := 32;

begin
    
    -- Set resolution-dependent parameters
    process(res_sel)
    begin
        case res_sel is
            when "00" =>   -- 640x480
                hbp <= "00010010000";     -- 144 (96 + 48)
                vbp <= "00000101101";     -- 35 (2 + 33)
                x_offset <= "00000000000"; -- Adjusted for 640x480
                y_offset <= "00000000000";
            when "01" =>   -- 1024x768
                hbp <= "00010100000";     -- Original values
                vbp <= "00000100011";
                x_offset <= "00000000000";
                y_offset <= "00000000000";
            when others => -- 1280x1024
                hbp <= "00101100000";     -- 360 (112 + 248)
                vbp <= "00000101001";     -- 41 (3 + 38)
                x_offset <= "00000000000"; -- Adjusted for 1280x1024
                y_offset <= "00000000000";
        end case;
    end process;

    rom_addr <= vc - vbp - y_offset;
    rom_pix <= hc - hbp - x_offset;
    addr <= rom_addr(5 downto 0);
    
    -- Enable sprite video out when within the initial region
    spriteon_f <= '1' when ((hc >= x_offset + hbp) and (hc < x_offset + hbp + w))
                    and  ((vc >= y_offset + vbp) and (vc < y_offset + vbp + h))
                else '0';
    spriteon_m <= '1' when ((hc >= x_offset + hbp + w) and (hc < x_offset + hbp + w + w))
                    and  ((vc >= y_offset + vbp) and (vc < y_offset + vbp + h))
                else '0';
    spriteon_l <= '1' when ((hc >= x_offset + hbp + w + w) and (hc < x_offset + hbp + w + w + w))
                    and  ((vc >= y_offset + vbp) and (vc < y_offset + vbp + h))
                else '0';

    process(spriteon_f, spriteon_m, spriteon_l, vidon, vc, hc, rom_pix, M_f, M_m, M_l)
        variable j: integer;
    begin
        red <= "0000";
        green <= "0000";
        blue <= "0000";
        if (vidon = '1') then
            -- Background pattern
            red <= vc(4) & vc(4) & vc(4) & vc(4);
            green <= not(vc(4) & vc(4) & vc(4) & vc(4));
            blue <= hc(4) & hc(4) & hc(4) & hc(4);
            
            -- First sprite
            if (spriteon_f = '1') then
                j := conv_integer(rom_pix);
                R <= M_f(j);
                G <= M_f(j);
                B <= M_f(j);
                if(R = '1') then
                    red <= R & R & R & R;
                    green <= G & G & G & G;
                    blue <= B & B & B & B;
                end if;
            end if;
            
            -- Middle sprite
            if (spriteon_m = '1') then
                j := conv_integer(rom_pix - w);
                R <= M_m(j);
                G <= M_m(j);
                B <= M_m(j);
                if(R = '1') then
                    red <= R & R & R & R;
                    green <= G & G & G & G;
                    blue <= B & B & B & B;
                end if;
            end if;
            
            -- Last sprite
            if (spriteon_l = '1') then
                j := conv_integer(rom_pix - w - w);
                R <= M_l(j);
                G <= M_l(j);
                B <= M_l(j);
                if(R = '1') then
                    red <= R & R & R & R;
                    green <= G & G & G & G;
                    blue <= B & B & B & B;
                end if;
            end if;
        end if;
  end process;

end Behavioral;