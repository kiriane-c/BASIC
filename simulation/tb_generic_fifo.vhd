--library ieee;
--use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
--
--entity tb_generic_wptr_cell is
--end entity tb_generic_wptr_cell;
--
--architecture rtl of tb_generic_wptr_cell is
--
--  component generic_wptr_cell is
--    generic(
--	    WIDTH   : natural := 8;
--	    DEPTH   : natural := 32
--    );
--    port(
--	    -- write domain signals --
--	    clock_w	: in  std_logic;
--	    rst_n_w	: in  std_logic;
--	    incr_w	: in  std_logic;
--	    enable_w	: out std_logic;
--	    addr_w	: out std_logic_vector(WIDTH-1 downto 0);
--	    -- fifo configuration --
--	    fifo_size	: in  std_logic_vector(DEPTH-1 downto 0);
--	    addr_offset : in  std_logic_vector(WIDTH-1 downto 0);
--	    -- status flags --
--	    full	: out std_logic;
--	    overflow	: out std_logic;
--	    -- handshake signals with read domain (pre-synchronized) --
--	    rptr_gray	: in  std_logic_vector(WIDTH-1 downto 0);
--	    wptr_gray	: out std_logic_vector(WIDTH-1 downto 0)
--    );
--  end component generic_wptr_cell;
--  
--  -- Constants
--  constant WIDTH	  : natural := 8;
--  constant DEPTH	  : natural := 32;
--  constant CLOCK_PERIOD : time    := 50 ns;
--  constant RESET_TIME   : time    := 75 ns;
--  
--  -- DUT ports
--  signal clock_w     : std_logic := '0';
--  signal rst_n_w     : std_logic := '0';
--  signal incr_w      : std_logic := '0';
--  signal enable_w    : std_logic;
--  signal addr_w      : std_logic_vector(WIDTH-1 downto 0);
--  signal fifo_size   : std_logic_vector(DEPTH-1 downto 0);
--  signal addr_offset : std_logic_vector(WIDTH-1 downto 0);
--  signal full        : std_logic;
--  signal overflow    : std_logic;
--  signal rptr_gray   : std_logic_vector(WIDTH-1 downto 0);
--  signal wptr_gray   : std_logic_vector(WIDTH-1 downto 0);
--  
--  -- Test utility signals
--  signal stop_clock : boolean := false;
--  
--begin
--  -- Device Under Test instantiation
--  uut: generic_wptr_cell
--    generic map (
--	WIDTH => WIDTH,
--	DEPTH => DEPTH
--    )
--    port map (
--	clock_w     => clock_w,
--	rst_n_w     => rst_n_w,
--	incr_w      => incr_w,
--	enable_w    => enable_w,
--	addr_w      => addr_w,
--	fifo_size   => fifo_size,
--	addr_offset => addr_offset,
--	full	    => full,
--	overflow    => overflow,
--	rptr_gray   => rptr_gray,
--	wptr_gray   => wptr_gray
--    );
--
--  -- Main test stimulus process
--  stimulus_p: process
--  begin
--    -- Initial values
--    fifo_size   <= std_logic_vector(to_unsigned(16, DEPTH));  -- FIFO size of 16
--    addr_offset <= std_logic_vector(to_unsigned(0, WIDTH));	-- No address offset initially
--    rptr_gray   <= (others => '0');				-- Read pointer at 0
--    incr_w	  <= '0';
--    
--    -- Reset
--    rst_n_w <= '1' after RESET_TIME;
--    wait for CLOCK_PERIOD*2.5;
--    
--    -- Test case 1: Basic write pointer increment    
--    for i in 0 to 16 loop
--	incr_w <= '1';
--	wait for CLOCK_PERIOD;
--    end loop;
--    
--    -- End of simulation
--    stop_clock <= true;
--    wait for CLOCK_PERIOD * 100;
--    wait;
--  end process;
--  
--  clocking_process : process
--  begin
--    while not stop_clock loop
--	  clock_w <= '0', '1' after CLOCK_PERIOD / 2;
--	  wait for CLOCK_PERIOD;
--    end loop;
--    wait;
--  end process clocking_process;
--
--end architecture rtl;


--library ieee;
--use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
--
--entity tb_generic_rptr_cell is
--end entity tb_generic_rptr_cell;
--
--architecture rtl of tb_generic_rptr_cell is
--  
--  component generic_rptr_cell is
--    generic(
--	    WIDTH   : natural := 8;
--	    DEPTH   : natural := 32
--    );
--    port(
--	    -- write domain signals --
--	    clock_r	: in  std_logic;
--	    rst_n_r	: in  std_logic;
--	    incr_r	: in  std_logic;
--	    enable_r	: out std_logic;
--	    addr_r	: out std_logic_vector(WIDTH-1 downto 0);
--	    -- fifo configuration --
--	    fifo_size	: in  std_logic_vector(DEPTH-1 downto 0);
--	    addr_offset : in  std_logic_vector(WIDTH-1 downto 0);
--	    -- status flags --
--	    empty	: out std_logic;
--	    underrun	: out std_logic;
--	    -- handshake signals with read domain (pre-synchronized) --
--	    wptr_gray	: in  std_logic_vector(WIDTH-1 downto 0);
--	    rptr_gray	: out std_logic_vector(WIDTH-1 downto 0)
--    );
--  end component generic_rptr_cell;
--  
--  -- Constants
--  constant WIDTH	  : natural := 8;
--  constant DEPTH	  : natural := 32;
--  constant CLOCK_PERIOD : time    := 50 ns;
--  constant RESET_TIME   : time    := 75 ns;
--  
--  -- DUT ports
--  signal clock_r     : std_logic := '0';
--  signal rst_n_r     : std_logic := '0';
--  signal incr_r      : std_logic := '0';
--  signal enable_r    : std_logic;
--  signal addr_r      : std_logic_vector(WIDTH-1 downto 0);
--  signal fifo_size   : std_logic_vector(DEPTH-1 downto 0);
--  signal addr_offset : std_logic_vector(WIDTH-1 downto 0);
--  signal empty       : std_logic;
--  signal underrun    : std_logic;
--  signal wptr_gray   : std_logic_vector(WIDTH-1 downto 0);
--  signal rptr_gray   : std_logic_vector(WIDTH-1 downto 0);
--  
--  -- Test utility signals
--  signal stop_clock  : boolean := false;
--  
--begin
--  -- Unit Under Test instantiation
--  uut: generic_rptr_cell
--    generic map (
--	WIDTH => WIDTH,
--	DEPTH => DEPTH
--    )
--    port map (
--	clock_r     => clock_r,
--	rst_n_r     => rst_n_r,
--	incr_r      => incr_r,
--	enable_r    => enable_r,
--	addr_r      => addr_r,
--	fifo_size   => fifo_size,
--	addr_offset => addr_offset,
--	empty	    => empty,
--	underrun    => underrun,
--	wptr_gray   => wptr_gray,
--	rptr_gray   => rptr_gray
--    );
--  
--  -- Main test stimulus process
--  stim_proc: process
--  begin
--    -- Initial values
--    fifo_size   <= std_logic_vector(to_unsigned(16, DEPTH));  -- FIFO size of 16
--    addr_offset <= std_logic_vector(to_unsigned(0, WIDTH));	-- No address offset initially
--    wptr_gray   <= (others => '0');				-- Write pointer at 0
--    incr_r	  <= '0';
--    
--    rst_n_r <= '1' after RESET_TIME;
--    wait for CLOCK_PERIOD*1.5;
--    
--    -- Test case : Wrap around at fifo_size
--    -- Set write pointer to a value beyond fifo_size
--    wptr_gray <= (0 => '0', 1 => '0', 2 => '1', 3 => '1', 4 => '0', others => '0');  -- Some value beyond 16
--    wait for CLOCK_PERIOD;
--    
--    -- Simulate multiple reads to reach fifo_size and beyond
--    for i in 0 to 20 loop
--	incr_r <= '1';
--	wait for CLOCK_PERIOD;
--    end loop;
--    
--    -- End of simulation
--    stop_clock <= true;
--    wait for CLOCK_PERIOD * 10;
--    wait;
--  end process;
--  
--  clocking_process : process
--  begin
--  while not stop_clock loop
--	clock_r <= '0', '1' after CLOCK_PERIOD / 2;
--	wait for CLOCK_PERIOD;
--  end loop;
--  wait;
--  end process clocking_process;
--  
--end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_generic_fifo_cell is
end entity tb_generic_fifo_cell;

architecture rtl of tb_generic_fifo_cell is

  -- Constants
  constant FIFO_DEPTH	  : natural := 32;
  constant DATA_WIDTH	  : natural := 32;
  constant W_CLOCK_PERIOD : time := 2.500 ns; -- 400MHz
  constant W_RESET_TIME   : time := 3.750 ns;
  constant R_CLOCK_PERIOD : time := 5.988 ns; -- 167MHz
  constant R_RESET_TIME   : time := 8.982 ns;
  
  -- Component declaration for the DUT
  component generic_fifo_cell is
    generic(
        FIFO_DEPTH   : natural;
        DATA_WIDTH   : natural
    );
    port(
        -- read clock domain
        clock_r      : in  std_logic;
        rst_n_r      : in  std_logic;
        enable_r     : in  std_logic;
        data_r       : out std_logic_vector(DATA_WIDTH-1 downto 0);
        empty	     : out std_logic;
        -- write clock domain
        clock_w      : in  std_logic;
        rst_n_w      : in  std_logic;
        enable_w     : in  std_logic;
        data_w       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        full	     : out std_logic
    );
  end component;
  
  -- Signal declarations
  signal clock_r      : std_logic := '0';
  signal rst_n_r      : std_logic := '0';
  signal enable_r     : std_logic := '0';
  signal data_r       : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal empty        : std_logic;
  
  signal clock_w      : std_logic := '0';
  signal rst_n_w      : std_logic := '0';
  signal enable_w     : std_logic := '0';
  signal data_w       : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal full	      : std_logic;
  
  signal stop_w_clock : boolean := false;
  signal stop_r_clock : boolean := false;

begin

  -- Instantiate the UUT
  uut: generic_fifo_cell
    generic map(
        FIFO_DEPTH => FIFO_DEPTH,
        DATA_WIDTH => DATA_WIDTH
    )
    port map(
        clock_r  => clock_r,
        rst_n_r  => rst_n_r,
        enable_r => enable_r,
        data_r   => data_r,
        empty	 => empty,
        clock_w  => clock_w,
        rst_n_w  => rst_n_w,
        enable_w => enable_w,
        data_w   => data_w,
        full	 => full
    );
    
  write_stimulus_process : process
  	  variable seed1 : positive;
  	  variable seed2 : positive;
  	  variable rand  : real;
  begin
  	  -- apply reset
  	  rst_n_w <= '1' after W_RESET_TIME;
  
  	  wait until (rst_n_w = '1' and rst_n_r = '1' and rising_edge(clock_w));

  	  for i in 1 to 50 loop
  		  uniform(seed1, seed2, rand);
  		  data_w <= std_logic_vector(to_unsigned(integer(rand * 512.0), 32));

  		  enable_w  <= '1';
  
		  wait for W_CLOCK_PERIOD;  
  	  end loop;
	  wait for w_CLOCK_PERIOD * 50;
	  stop_w_clock <= true;
	  
	  wait;
  end process write_stimulus_process;
  
  read_stimulus_process : process
  begin
  	  -- apply reset
  	  rst_n_r <= '1' after R_RESET_TIME;
  
  	  wait until (rst_n_w = '1' and rst_n_r = '1' and rising_edge(clock_r));
  
  	  for i in 1 to 10 loop
  		  enable_r  <= '1';
  
  		  wait for R_CLOCK_PERIOD;  
  	  end loop;
	  wait for R_CLOCK_PERIOD * 50;
	  stop_r_clock <= true;
	  
	  wait;
  end process read_stimulus_process;
  
  write_clocking_process : process
  begin
  	while not stop_w_clock loop
  	        clock_w <= '0', '1' after W_CLOCK_PERIOD / 2;
  	        wait for W_CLOCK_PERIOD;
  	end loop;
  	wait;
  end process write_clocking_process;
  
  read_clocking_process : process
  begin
  	while not stop_r_clock loop
  	        clock_r <= '0', '1' after R_CLOCK_PERIOD / 2;
  	        wait for R_CLOCK_PERIOD;
  	end loop;
  	wait;
  end process read_clocking_process;

end architecture rtl;
