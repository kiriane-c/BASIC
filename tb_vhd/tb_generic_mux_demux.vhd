library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_generic_flat_mux_demux is
  
end entity tb_generic_flat_mux_demux;

architecture rtl of tb_generic_flat_mux_demux is

  component generic_flat_mux_demux is
    generic(
  	  NUM_INOUT : integer;
  	  SEL_WIDTH : integer;
  	  INO_WIDTH : integer
    );
    port(
  	  sel	    : in  std_logic_vector(SEL_WIDTH-1 downto 0);
  	  mode      : in  std_logic;
  	  mux_in    : in  std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0);
  	  mux_out   : out std_logic_vector(INO_WIDTH-1 downto 0);
  	  demux_in  : in  std_logic_vector(INO_WIDTH-1 downto 0);
  	  demux_out : out std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0)
    );
  end component generic_flat_mux_demux;
  
  component generic_tree_mux_demux is
    generic(
    	  NUM_INOUT : integer;
    	  SEL_WIDTH : integer;
    	  INO_WIDTH : integer 
    );
    port(
    	  sel	    : in  std_logic_vector(SEL_WIDTH-1 downto 0);
    	  mode      : in  std_logic;
    	  mux_in    : in  std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0);
    	  mux_out   : out std_logic_vector(INO_WIDTH-1 downto 0);
    	  demux_in  : in  std_logic_vector(INO_WIDTH-1 downto 0);
    	  demux_out : out std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0)
    );
  end component generic_tree_mux_demux;
  
  -- constants
  CONSTANT CLOCK_PERIOD : time := 50 ns;
  -- Constants
  constant NUM_INOUT_C : integer := 8;
  constant INO_WIDTH_C : integer := 8;
  constant SEL_WIDTH_C : integer := integer(ceil(log2(real(NUM_INOUT_C))));
  
  signal clock     : std_logic := '0';
    
  signal sel	   : std_logic_vector(SEL_WIDTH_C-1 downto 0) := (others => '0');
  signal mode	   : std_logic := '0';
  signal mux_in	   : std_logic_vector((NUM_INOUT_C*INO_WIDTH_C)-1 downto 0) := (others => '0');
  signal mux_out   : std_logic_vector(INO_WIDTH_C-1 downto 0) := (others => '0');
  signal demux_in  : std_logic_vector(INO_WIDTH_C-1 downto 0) := (others => '0');
  signal demux_out : std_logic_vector((NUM_INOUT_C*INO_WIDTH_C)-1 downto 0) := (others => '0');
  
  signal stop_clock : boolean	:= false;
  
begin

  uut: generic_tree_mux_demux
  generic map(
  	NUM_INOUT => NUM_INOUT_C,
	SEL_WIDTH => SEL_WIDTH_C,
	INO_WIDTH => INO_WIDTH_C
  )
  port map(
  	sel	  => sel,     
	mode	  => mode,     
	mux_in    => mux_in,   
	mux_out   => mux_out,  
	demux_in  => demux_in, 
	demux_out => demux_out
  );
  
  stimulus_process: process  
    procedure check_mux(
    	sel_val	 : in integer;
	data_val : in std_logic_vector(INO_WIDTH_C-1 downto 0)
    ) is
    	variable index : integer;
    begin
    
    	-- Initialize the full mux_in with zeros
      	mux_in <= (others => '0');
      
      	-- Set specific input value at selected position
      	index := sel_val * (INO_WIDTH_C-1);
      	for i in 0 to INO_WIDTH_C-1 loop
        	mux_in(index+i) <= data_val(i);
     	end loop;
	
	-- Set the select value
      	sel <= std_logic_vector(to_unsigned(sel_val, SEL_WIDTH_C));
      
      	wait for CLOCK_PERIOD;
    
    end procedure check_mux;
    
    procedure check_demux(
    	sel_val	 : in integer;
	data_val : in std_logic_vector(INO_WIDTH_C-1 downto 0)
    ) is
    	variable index : integer;
    begin
    	-- Set input value
	demux_in <= data_val;
	
	-- Set the select value
      	sel <= std_logic_vector(to_unsigned(sel_val, SEL_WIDTH_C));
	
	wait for CLOCK_PERIOD;
    
    end procedure check_demux;
    
  begin
  	-- Test multiplexer mode 
     	-- Test each select value with different data patterns
    	for i in 0 to NUM_INOUT_C-1 loop
      		-- Test with all 1's
      		check_mux(i, (others => '1'));
      		-- Test with alternating pattern
      		check_mux(i+1, "01010101");
   	end loop;
	
	-- Test demultiplexer mode 
	wait for CLOCK_PERIOD*2;
	mode <= '1';
	-- Test each select value with different data patterns
    	for i in 0 to NUM_INOUT_C-1 loop
		-- Test with all 1's
		check_demux(i, "10001000");
		-- Test with alternating pattern
		check_demux(i+1, "01110111");
	end loop;
	
    	wait for CLOCK_PERIOD*100;
	stop_clock <= true;
	wait;
  
  end process stimulus_process;
  
  clocking_process : process
  begin
  	  while not stop_clock loop
  		  clock <= '0', '1' after CLOCK_PERIOD / 2;
  		  wait for CLOCK_PERIOD;
  	  end loop;
  	  wait;
  end process clocking_process;

end architecture rtl;
