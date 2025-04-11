-- generic updown counter testbench
------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_generic_updown_counter is
  generic(
  	COUNT_WIDTH  : natural := 8;
  	INIT_VALUE   : natural := 0;
  	USE_MODULO   : boolean := true;
  	MOD_VALUE    : natural := 256
  );
end entity tb_generic_updown_counter;

architecture rtl of tb_generic_updown_counter is
  component generic_updown_counter is
    generic(
    	  COUNT_WIDTH  : natural;
    	  INIT_VALUE   : natural;
    	  USE_MODULO   : boolean;
    	  MOD_VALUE    : natural
    );
    port(
    	  clock     : in  std_logic;
    	  reset_n   : in  std_logic;
    	  enable    : in  std_logic;
    	  updown    : in  std_logic;
    	  -- synchronization and control signals --
    	  sync_load : in  std_logic;
    	  load_val  : in  std_logic_vector(COUNT_WIDTH-1 downto 0);
    	  out_val   : out std_logic_vector(COUNT_WIDTH-1 downto 0);
    	  -- status signals --
    	  max_val   : out std_logic;
    	  min_val   : out std_logic
    );
  end component generic_updown_counter;
  
  -- constants
  CONSTANT CLOCK_PERIOD : time := 50 ns;
  CONSTANT RESET_TIME	  : time := 75 ns;
  
  signal clock      : std_logic := '0';
  signal reset_n    : std_logic := '0';
  signal enable     : std_logic := '0';
  signal updown     : std_logic := '0';
  signal sync_load  : std_logic := '0';
  signal load_val   : std_logic_vector(COUNT_WIDTH-1 downto 0) := (others => '0');
  signal out_val    : std_logic_vector(COUNT_WIDTH-1 downto 0) := (others => '0');
  signal max_val    : std_logic := '0';
  signal min_val    : std_logic := '0';
	 
  signal stop_clock : boolean	  := false;
  	 
begin

  uut : generic_updown_counter
  generic map(
  	COUNT_WIDTH => COUNT_WIDTH,
  	INIT_VALUE  => INIT_VALUE, 
  	USE_MODULO  => USE_MODULO, 
  	MOD_VALUE   => MOD_VALUE  
  )
  port map(
  	clock  	    => clock,	
  	reset_n     => reset_n,  
  	enable      => enable,	
  	updown 	    => updown,	
  	sync_load   => sync_load,
  	load_val    => load_val, 
  	out_val     => out_val,  
  	max_val     => max_val,  
  	min_val     => min_val 
  );
  
  stimulus_p: process
  begin
  	-- apply reset
  	reset_n   <= '1' after RESET_TIME;

  	wait for CLOCK_PERIOD * 2.5;
	
	  enable	<= '1';
	  updown	<= '1';
	
	  -- Test 1: count up to overflow --
	  for i in 1 to MOD_VALUE loop
		  wait for CLOCK_PERIOD;
	  end loop;
	
	  -- Test 2: count down to underflow --
	  updown	<= '0';
    for i in 1 to MOD_VALUE loop
      	wait for CLOCK_PERIOD;
    end loop;
	
	  -- Test 3: sync_load, max_val and min_val signals functionality
	  -- Load value near max
    load_val  <= std_logic_vector(to_unsigned(MOD_VALUE-4, COUNT_WIDTH));
    sync_load <= '1';
    wait for CLOCK_PERIOD;
    sync_load <= '0';
    
   	-- Count up to max
	  updown	  <= '1';
    for i in 0 to 5 loop
     		wait for CLOCK_PERIOD;
   	end loop;
    
    -- Load value near min
    load_val  <= std_logic_vector(to_unsigned(1, COUNT_WIDTH));
    sync_load <= '1';
    wait for CLOCK_PERIOD;
    sync_load <= '0';
    
    -- Down count to min
    updown 	  <= '0';
    for i in 0 to 3 loop
     		wait for CLOCK_PERIOD;
   	end loop;
	
	  wait for CLOCK_PERIOD*10;
	  stop_clock <= true;
	  wait;
  
  end process stimulus_p;
  
  clocking_process : process
  begin
  	  while not stop_clock loop
  		  clock <= '0', '1' after CLOCK_PERIOD / 2;
  		  wait for CLOCK_PERIOD;
  	  end loop;
  	  wait;
  end process clocking_process;

end architecture rtl;
