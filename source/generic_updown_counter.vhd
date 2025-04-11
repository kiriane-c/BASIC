-- Clock gating :
--	component which deactivates the clock when there is inactivity
--	reduces dynamic power consumption while stopping the clock
--	controlled by the signal enable
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity clock_gating is
  generic(
  	POWER_MODE : boolean := true
  );
  port(
  	clock_in   : in  std_logic;
	enable	   : in  std_logic;
	clock_out  : out std_logic
  );
end entity clock_gating;

architecture rtl of clock_gating is
  signal gated_clock : std_logic;
begin

  clock_p : process(clock_in, gated_clock, enable)
  begin
  	gated_clock <= not clock_in and enable;
	if POWER_MODE then
		clock_out <= gated_clock;
	else
		clock_out <= clock_in;
	end if;
	
  end process clock_p;

end architecture rtl;

-- Configurable counter: Important elements in VHDL, used to manage temporisation and counting.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_updown_counter is
  generic(
  	COUNT_WIDTH  : natural := 8;
	INIT_VALUE   : natural := 0;
	USE_MODULO   : boolean := true;
	MOD_VALUE    : natural := 256
  );
  port(
  	clock	  : in  std_logic;
	reset_n	  : in  std_logic;
	enable	  : in  std_logic;
	updown	  : in  std_logic;
	-- synchronization and control signals --
	sync_load : in  std_logic;
	load_val  : in  std_logic_vector(COUNT_WIDTH-1 downto 0);
	out_val	  : out std_logic_vector(COUNT_WIDTH-1 downto 0);
	-- status signals --
	max_val	  : out std_logic;
	min_val	  : out std_logic
  );
end entity generic_updown_counter;

architecture rtl of generic_updown_counter is

  -- constant configuration --  
  constant DEFAULT_MAX : unsigned(COUNT_WIDTH-1 downto 0) :=
  	to_unsigned(MOD_VALUE-1, COUNT_WIDTH);
	
  constant DEFAULT_MIN : unsigned(COUNT_WIDTH-1 downto 0) :=
  	to_unsigned(0, COUNT_WIDTH);

  signal count 	   : unsigned(COUNT_WIDTH-1 downto 0);
  signal gated_clk : std_logic;
  
  component clock_gating is
    generic(
    	POWER_MODE  : boolean
    );
    port(
    	  clock_in  : in  std_logic;
    	  enable    : in  std_logic;
    	  clock_out : out std_logic
    );
  end component clock_gating;
  
begin

  clock_gate_inst : clock_gating
  generic map(
  	POWER_MODE => true
  )
  port map(
  	clock_in   => clock,
	enable     => enable,
	clock_out  => gated_clk
  );

  count_p : process(gated_clk, reset_n)
  begin
  	  if reset_n = '0' then
  	  -- reinitialize value --
  		  count    <= to_unsigned(INIT_VALUE, COUNT_WIDTH);
  	  elsif rising_edge(gated_clk) then
  		  -- synchronous load --
  		  if sync_load = '1' then
  			  count    <= unsigned(load_val);
		  else
  			  if updown = '1' then
  				  -- Up count --
  				  if (USE_MODULO and count = DEFAULT_MAX) then
  					  count    <= (others => '0');
  				  else
  					  count    <= count + 1;
  				  end if;
  			  else
  				  -- Down count --
  				  if(USE_MODULO and count = DEFAULT_MIN) then
  					  count    <= to_unsigned(MOD_VALUE-1, COUNT_WIDTH);
  				  else
  					  count    <= count - 1;
  				  end if;
  			  end if;
  		  end if;
  	  end if;
  end process count_p;

  -- assign outputs --
  max_val <= '1' when count = DEFAULT_MAX else
  	     '0';
  	     
  min_val <= '1' when count = DEFAULT_MIN else
  	     '0';
  	     
  out_val <= std_logic_vector(count);

end architecture rtl;
