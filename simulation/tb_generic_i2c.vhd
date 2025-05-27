library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_generic_i2c_master is
end tb_generic_i2c_master;

architecture rtl of tb_generic_i2c_master is

  component generic_i2c_master is
    generic(
  	  CLOCK_FREQ : integer := 50_000_000; 
  	  I2C_FREQ   : integer := 100_000;
  	  WIDTH      : integer := 7	
    );
    port(
  	  clock      : in  std_logic; 
  	  reset_n    : in  std_logic; 
  	  start      : in  std_logic; 
  	  stop       : in  std_logic; 
  	  mode       : in  std_logic; 
  	  busy       : out std_logic; 
  	  ack	     : out std_logic; 
  	  address    : in  std_logic_vector(WIDTH-1 downto 0);
  	  wdata      : in  std_logic_vector(WIDTH-1 downto 0);
  	  rdata      : out std_logic_vector(WIDTH-1 downto 0);
  	  valid      : out std_logic;  
  	  scl	     : inout std_logic;
  	  sda	     : inout std_logic 
    );
  end component generic_i2c_master;
  
  -- constants
  constant CLOCK_PERIOD : time	  := 20 ns;	-- 50 MHz clock
  constant RESET_TIME 	: time    := 30 ns;
  constant CLOCK_FREQ  	: integer := 50_000_000;
  constant I2C_FREQ    	: integer := 400_000;
  constant WIDTH       	: integer := 8;
  
  -- signals
  signal clock   : std_logic := '0';
  signal reset_n : std_logic := '0';
  signal start   : std_logic := '0';
  signal stop	 : std_logic := '0';
  signal mode	 : std_logic := '0'; 
  signal busy	 : std_logic; 
  signal ack	 : std_logic; 
  signal address : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
  signal wdata   : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
  signal rdata   : std_logic_vector(WIDTH-1 downto 0);
  signal valid   : std_logic;  
  signal scl	 : std_logic := '0';
  signal sda	 : std_logic := '0';
  
  signal stop_clock : boolean := false;
   
begin

  -- Instantiate UUT
  uut: generic_i2c_master
  generic map(
  	CLOCK_FREQ => CLOCK_FREQ,
  	I2C_FREQ   => I2C_FREQ,  
  	WIDTH	   => WIDTH	
  )
  port map(
  	clock	  => clock,   
  	reset_n   => reset_n, 
  	start	  => start,   
  	stop	  => stop,   
  	mode	  => mode,   
  	busy	  => busy,   
  	ack	  => ack,   
  	address   => address, 
  	wdata	  => wdata,
  	rdata	  => rdata,  
  	valid	  => valid,  
  	scl	  => scl,  
  	sda	  => sda     
  );
  
  stimulus_p : process
  	variable seed1 : positive;
  	variable seed2 : positive;
  	variable rand  : real;
  begin
  	-- Reset
	reset_n  <= '1' after RESET_TIME;
	wait for CLOCK_PERIOD*2.5;
	
	for i in 0 to 10 loop
		uniform(seed1, seed2, rand);
		address  <= std_logic_vector(to_unsigned(integer(rand * 512.0), 8));
		
		uniform(seed1, seed2, rand);
		wdata  <= std_logic_vector(to_unsigned(integer(rand * 256.0), 8));
		
		-- Configurer pour écriture
		mode <= '1';
		
		-- Commencer la transaction
		wait for CLOCK_PERIOD;
		start <= '1';
		wait for CLOCK_PERIOD;
		start <= '0';
		
		-- Attendre que la transaction soit terminée
		wait until busy = '1';
		mode <= '0';
		wait until busy = '0';
		
		-- Terminer la transaction
		stop <= '1';
		wait for CLOCK_PERIOD;
		stop <= '0';
		
	end loop;
		
	wait for CLOCK_PERIOD*1000;
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
