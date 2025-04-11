-- generic_sync_cell
--	2 flip flop synchronization module for CDC
--------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity generic_sync_cell is 
  port(
  	clock	: in  std_logic;
	reset_n	: in  std_logic;
	async	: in  std_logic;	-- asynchronous signal
	sync	: out std_logic		-- synchronous signal
  );
end entity generic_sync_cell;

architecture rtl of generic_sync_cell is
  
  signal sync_1_ff : std_logic;
  signal sync_2_ff : std_logic;

begin
  -- ***************************************************************
  -- The Core of the problem --
  -- ***************************************************************
  
  sync_p : process(clock, reset_n) 
  begin
  	if reset_n = '0' then
		sync_1_ff <= '0';
		sync_2_ff <= '0';
	elsif rising_edge(clock) then
		sync_1_ff <= async;
		sync_2_ff <= sync_1_ff;
	end if;
  
  end process sync_p;
  
  sync <= sync_2_ff;

end architecture rtl;

-- generic_wptr_cell
--	writing part of FIFO using gray code encoding. 
--	The FIFO maintains a gray pointer indicating the next writing address
--	conversion from gray to binary. Incrementation of binary value and reconversion to gray
--	The gray write pointer is sent to read clock domain.
--	full flag asserted by comparing write and read pointers
--	G3 = B3, G2 = B2 xor B3, G1 = B1 xor B2, G0 = B0 xor B1
--	B3 = G3, B2 = G2 xor G3, B1 = G1 xor G2 xor G3 = G1 xor B2, B0 = G0 xor G1 xor G2 xor G3
--									= G0 xor B1
-----------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_wptr_cell is
  generic(
  	WIDTH	: natural := 8
  );
  port(
  	-- write domain signals --
	clock_w     : in  std_logic;
	rst_n_w     : in  std_logic;
	incr_w	    : in  std_logic;
	enable_w    : out std_logic;
	addr_w	    : out std_logic_vector(WIDTH-1 downto 0);
	-- fifo configuration --
	fifo_size   : in  std_logic_vector(WIDTH-1 downto 0);
	addr_offset : in  std_logic_vector(WIDTH-1 downto 0);
	-- status flags --
	full	    : out std_logic;
	overflow    : out std_logic;
	-- handshake signals with read domain (pre-synchronized) --
	rptr_gray   : in  std_logic_vector(WIDTH-1 downto 0);
	wptr_gray   : out std_logic_vector(WIDTH-1 downto 0)
  );
end entity generic_wptr_cell;

architecture rtl of generic_wptr_cell is
  
  -- Gray code conversion --
  function to_gray(bin : std_logic_vector) return std_logic_vector is
  	variable gray : std_logic_vector(bin'range);
  begin
  	gray(gray'high) := bin(bin'high);
  	for i in bin'high-1 downto 0 loop
  		gray(i) := bin(i) xor bin(i+1);
  	end loop;
  	return gray;
  end function;
  
  -- Binary code conversion --
  function to_binary(gray : std_logic_vector) return std_logic_vector is
  	variable bin : std_logic_vector(gray'range);
  begin
  	bin(bin'high) := gray(gray'high);
  	for i in gray'high-1 downto 0 loop
  		bin(i) := gray(i) xor bin(i+1);
  	end loop;
  	return bin;
  end function;
  
  signal wptr_bin      	: unsigned(WIDTH-1 downto 0);
  signal wptr_bin_next 	: unsigned(WIDTH-1 downto 0);
  signal wptr_gray_next : unsigned(WIDTH-1 downto 0);
  
  signal full_s	: std_logic;
  
begin

  -- ******************************************************************
  -- The Core of the problem --
  -- ******************************************************************
    
  -- write pointer update process
  wptr_bin_p : process(clock_w, rst_n_w)
  begin
      if rst_n_w = '0' then
	      wptr_bin <= (others => '0');
      elsif rising_edge(clock_w) then
	      if incr_w = '1' and full_s = '0' then
		      wptr_bin <= wptr_bin_next;
	      end if;
      end if;
  end process;
  
  -- calculate next write pointer value for full flag logic
  wptr_next_p : process(wptr_bin, fifo_size)
  begin
	if (wptr_bin = unsigned(fifo_size) - 1) then
		wptr_bin_next <= (others => '0');
	else
		wptr_bin_next <= wptr_bin + 1;
	end if;
  end process wptr_next_p;
		
  -- conversion of next write pointer to gray
  wptr_gray_next <= unsigned(to_gray(std_logic_vector(wptr_bin_next)));
  
  full_p : process(wptr_bin_next, rptr_gray)
  begin
  	if (unsigned(to_gray(std_logic_vector(wptr_bin_next))) = unsigned(rptr_gray)) then
		full_s 	<= '1';
	else
		full_s	<= '0';
	end if;
  end process full_p;
  
  -- output assertion
  -- full flag
  full     <= full_s;
  -- enable write when not full
  enable_w <= incr_w and (not full_s);
  -- write gray pointer
  wptr_gray <= to_gray(std_logic_vector(wptr_bin));
  -- address output
  addr_w    <= std_logic_vector(wptr_bin + unsigned(addr_offset));
  
end architecture rtl;

-- generic_rptr_cell
--	reading part of FIFO using gray code encoding. 
--	The FIFO maintains a gray pointer indicating the next reading address
--	conversion from gray to binary. Incrementation of binary value and reconversion to gray
--	The gray read pointer is sent to write clock domain.
--	empty flag asserted by comparing write and read pointers
-----------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_rptr_cell is
  generic(
  	WIDTH	: natural := 8
  );
  port(
  	-- write domain signals --
	clock_r     : in  std_logic;
	rst_n_r     : in  std_logic;
	incr_r	    : in  std_logic;
	enable_r    : out std_logic;
	addr_r	    : out std_logic_vector(WIDTH-1 downto 0);
	-- fifo configuration --
	fifo_size   : in  std_logic_vector(WIDTH-1 downto 0);
	addr_offset : in  std_logic_vector(WIDTH-1 downto 0);
	-- status flags --
	empty	    : out std_logic;
	underrun    : out std_logic;
	-- handshake signals with read domain (pre-synchronized) --
	wptr_gray   : in  std_logic_vector(WIDTH-1 downto 0);
	rptr_gray   : out std_logic_vector(WIDTH-1 downto 0)
  );
end entity generic_rptr_cell;

architecture rtl of generic_rptr_cell is
  
  -- Gray code conversion --
  function to_gray(bin : std_logic_vector) return std_logic_vector is
  	variable gray : std_logic_vector(bin'range);
  begin
  	gray(gray'high) := bin(bin'high);
  	for i in bin'high-1 downto 0 loop
  		gray(i) := bin(i) xor bin(i+1);
  	end loop;
  	return gray;
  end function;
  
  -- Binary code conversion --
  function to_binary(gray : std_logic_vector) return std_logic_vector is
  	variable bin : std_logic_vector(gray'range);
  begin
  	bin(bin'high) := gray(gray'high);
  	for i in gray'high-1 downto 0 loop
  		bin(i) := gray(i) xor bin(i+1);
  	end loop;
  	return bin;
  end function;
  
  signal rptr_bin	      : unsigned(WIDTH-1 downto 0);
  signal rptr_bin_next 	: unsigned(WIDTH-1 downto 0);
  signal rptr_gray_next : unsigned(WIDTH-1 downto 0);
  
  signal empty_s   : std_logic;
  
begin

  -- ******************************************************************
  -- The Core of the problem --
  -- ******************************************************************
  
  -- read pointer update process
  rptr_bin_p : process(clock_r, rst_n_r)
  begin
      if rst_n_r = '0' then
	      rptr_bin <= (others => '0');
      elsif rising_edge(clock_r) then
	      if incr_r = '1' and empty_s = '0' then
		      rptr_bin <= rptr_bin_next;
	      end if;
      end if;
  end process;
  
  -- calculate next read pointer value for empty flag logic
  rptr_next_p : process(rptr_bin, fifo_size)
  begin
	if (rptr_bin = unsigned(fifo_size) - 1) then
		rptr_bin_next <= (others => '0');
	else
		rptr_bin_next <= rptr_bin + 1;
	end if;
  end process rptr_next_p;
		
  -- conversion of next read pointer to gray
  rptr_gray_next <= unsigned(to_gray(std_logic_vector(rptr_bin_next)));
  
  empty_p : process(rptr_bin_next, wptr_gray)
  begin
  	if (rptr_bin_next = unsigned(to_binary(wptr_gray)) + 1) then
		empty_s <= '1';
	else
		empty_s	<= '0';
	end if;
  end process empty_p;
  
  -- output assertion
  -- empty flag
  empty    <= empty_s;
  -- enable read when not empty
  enable_r <= incr_r and (not empty_s);
  -- read gray pointer
  rptr_gray <= to_gray(std_logic_vector(rptr_bin));
  -- address output
  addr_r    <= std_logic_vector(rptr_bin + unsigned(addr_offset));
  
end architecture rtl;

-- generic_dual_port_ram
--	RAM that can be accessed via two different buses allowing simultaneous 
--	read and write accesses.
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity generic_dual_port_ram is
  generic(
  	ADDR_WIDTH : natural := 32;
MEMO_DEPTH : integer := 2**16;
	DATA_WIDTH : natural := 32
  );
  port(
  	clock_r    : in  std_logic;
	rst_n_r    : in  std_logic;
	enable_r   : in  std_logic;
	addr_r	   : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
	data_r	   : out std_logic_vector(DATA_WIDTH-1 downto 0);
	clock_w    : in  std_logic;
	rst_n_w    : in  std_logic;
	enable_w   : in  std_logic;
	addr_w	   : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
	data_w	   : in  std_logic_vector(DATA_WIDTH-1 downto 0)	   
  );
end entity generic_dual_port_ram;

architecture rtl of generic_dual_port_ram is

  -- memory signal
  type memory_t is array (MEMO_DEPTH-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal memory : memory_t;
  
begin
  
  -- ***********************************************************************************************
      -- The Core of the problem --
  -- ***********************************************************************************************
  
  write_p : process(clock_w, rst_n_w)
  begin
  	if rst_n_w = '0' then
		memory 	<= (others => (others => '0'));
	elsif rising_edge(clock_w) then
		if enable_w = '1' then
			memory(to_integer(unsigned(addr_w))) <= data_w;
		else
			memory(to_integer(unsigned(addr_w))) <= memory(to_integer(unsigned(addr_w)));
		end if;
	end if;
  end process write_p;
  
  read_p : process(clock_r, rst_n_r)
  begin
  	if rst_n_r = '0' then
  		data_r <= (others => '0');
  	elsif rising_edge(clock_r) then
  		if enable_r = '1' then
  			data_r <= memory(to_integer(unsigned(addr_r)));
  		end if;
  	end if;
  end process read_p;
  
end architecture rtl;

-- generic_fifo_cell
--	connect everythiiiiiiiiiiiiiiiiiiiiing
--------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity generic_fifo_cell is 
  generic(
  	FIFO_DEPTH   : natural := 16;
  	DATA_WIDTH   : natural := 32
  );
  port(
  	-- read clock domain
  	clock_r      : in  std_logic;
	rst_n_r      : in  std_logic;
	enable_r     : in  std_logic;
	data_r	     : out std_logic_vector(DATA_WIDTH-1 downto 0);
	empty	       : out std_logic;
	-- write clock domain
	clock_w      : in  std_logic;
	rst_n_w      : in  std_logic;
	enable_w     : in  std_logic;
	data_w	     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
	full	       : out std_logic
  );
end entity generic_fifo_cell;

architecture rtl of generic_fifo_cell is

  component generic_sync_cell is 
    port(
  	  clock   : in  std_logic;
  	  reset_n : in  std_logic;
  	  async   : in  std_logic;
  	  sync    : out std_logic 
    );
  end component generic_sync_cell;
  
  component generic_rptr_cell is
    generic(
  	  WIDTH   : natural := 8
    );
    port(
  	  -- write domain signals --
  	  clock_r     : in  std_logic;
  	  rst_n_r     : in  std_logic;
  	  incr_r      : in  std_logic;
  	  enable_r    : out std_logic;
  	  addr_r      : out std_logic_vector(WIDTH-1 downto 0);
  	  -- fifo configuration --
  	  fifo_size   : in  std_logic_vector(WIDTH-1 downto 0);
  	  addr_offset : in  std_logic_vector(WIDTH-1 downto 0);
  	  -- status flags --
  	  empty       : out std_logic;
  	  underrun    : out std_logic;
  	  -- handshake signals with write domain (pre-synchronized) --
  	  wptr_gray   : in  std_logic_vector(WIDTH-1 downto 0);
  	  rptr_gray   : out std_logic_vector(WIDTH-1 downto 0)
    );
  end component generic_rptr_cell;
  
  component generic_wptr_cell is
    generic(
  	  WIDTH   : natural := 8
    );
    port(
  	  -- write domain signals --
  	  clock_w     : in  std_logic;
  	  rst_n_w     : in  std_logic;
  	  incr_w      : in  std_logic;
  	  enable_w    : out std_logic;
  	  addr_w      : out std_logic_vector(WIDTH-1 downto 0);
  	  -- fifo configuration --
  	  fifo_size   : in  std_logic_vector(WIDTH-1 downto 0);
  	  addr_offset : in  std_logic_vector(WIDTH-1 downto 0);
  	  -- status flags --
  	  full        : out std_logic;
  	  overflow    : out std_logic;
  	  -- handshake signals with read domain (pre-synchronized) --
  	  rptr_gray   : in  std_logic_vector(WIDTH-1 downto 0);
  	  wptr_gray   : out std_logic_vector(WIDTH-1 downto 0)
    );
  end component generic_wptr_cell;
  
  component generic_dual_port_ram is
    generic(
  	  ADDR_WIDTH : natural := 32;
	  MEMO_DEPTH : natural := 2**16;
  	  DATA_WIDTH : natural := 32
    );
    port(
  	  clock_r    : in  std_logic;
  	  rst_n_r    : in  std_logic;
  	  enable_r   : in  std_logic;
  	  addr_r     : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  	  data_r     : out std_logic_vector(DATA_WIDTH-1 downto 0);
  	  clock_w    : in  std_logic;
  	  rst_n_w    : in  std_logic;
  	  enable_w   : in  std_logic;
  	  addr_w     : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
  	  data_w     : in  std_logic_vector(DATA_WIDTH-1 downto 0)
    );
  end component generic_dual_port_ram;
  
  -- constants
  constant ADDR_WIDTH : natural := integer(ceil(log2(real(FIFO_DEPTH + 1))));
  constant FIFO_SIZE  : std_logic_vector(ADDR_WIDTH - 1 DOWNTO 0) := std_logic_vector(to_unsigned(FIFO_DEPTH, ADDR_WIDTH));

  -- fifo configuration --
  signal addr_offset  : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
  
  signal mem_addr_w   : std_logic_vector(ADDR_WIDTH-1 downto 0);    -- out
  signal mem_data_w   : std_logic_vector(DATA_WIDTH-1 downto 0);    -- out
  signal mem_enable_w : std_logic;				    -- out
  signal mem_addr_r   : std_logic_vector(ADDR_WIDTH-1 downto 0);    -- out
  signal mem_data_r   : std_logic_vector(DATA_WIDTH-1 downto 0);    -- in
  signal mem_enable_r : std_logic;				    -- out
  
  -- signals to synchronized gray pointers
  signal rptr      : std_logic_vector(ADDR_WIDTH-1 downto 0);
  signal wptr      : std_logic_vector(ADDR_WIDTH-1 downto 0);
  signal sync_rptr : std_logic_vector(ADDR_WIDTH-1 downto 0);
  signal sync_wptr : std_logic_vector(ADDR_WIDTH-1 downto 0);
  
  signal full_s  : std_logic;
  signal empty_s : std_logic;
    
begin
 
  mem_data_w <= data_w;
  
  full  <= full_s;
  empty <= empty_s;
  
  -- synchronize in write cell
  writetoread_gen : for i in 0 to ADDR_WIDTH-1 generate
    w2r_sync_cell_inst : generic_sync_cell
    port map(
    	clock 	=> clock_r,
	reset_n => rst_n_r,
	async   => wptr(i),
	sync    => sync_wptr(i)
    );
  end generate writetoread_gen;
  
  -- synchronize in read cell
  readtowrite_gen : for i in 0 to ADDR_WIDTH-1 generate
    r2w_sync_cell_inst : generic_sync_cell
    port map(
  	clock	=> clock_w,
  	reset_n => rst_n_w,
  	async	=> rptr(i),
  	sync	=> sync_rptr(i)
    );
  end generate readtowrite_gen;
  
  -- instantiate write cell
  wptr_cell_inst: generic_wptr_cell
    generic map (
      WIDTH => ADDR_WIDTH
    )
    port map (
      clock_w	  => clock_w,
      rst_n_w	  => rst_n_w,
      incr_w	  => enable_w,		    -- FIFO we
      enable_w    => mem_enable_w,	-- Memory we
      addr_w	  => mem_addr_w,	    -- Memory addr
      fifo_size   => FIFO_SIZE,
      addr_offset => addr_offset,
      full	  => full_s,
      overflow    => open,
      rptr_gray   => sync_rptr,
      wptr_gray   => wptr
    );
    
    -- instantiate read cell
    rptr_cell_inst: generic_rptr_cell
      generic map (
    	WIDTH => ADDR_WIDTH
      )
      port map (
    	clock_r     => clock_r,
    	rst_n_r     => rst_n_r,
    	incr_r      => enable_r,	      -- FIFO re
    	enable_r    => mem_enable_r,	  -- Memory re
    	addr_r      => mem_addr_r,	    -- Memory addr
    	fifo_size   => FIFO_SIZE,
    	addr_offset => addr_offset,
    	empty	    => empty_s,
    	underrun    => open,
    	wptr_gray   => sync_wptr,
    	rptr_gray   => rptr
      );
    
    -- instantiate dual port RAM
    dual_port_ram_inst: generic_dual_port_ram
      generic map (
        ADDR_WIDTH => ADDR_WIDTH,
	MEMO_DEPTH => FIFO_DEPTH,
        DATA_WIDTH => DATA_WIDTH
      )
      port map (
        clock_r    => clock_r,
        rst_n_r    => rst_n_r,
        enable_r   => mem_enable_r,
        addr_r     => mem_addr_r,
        data_r     => mem_data_r,
        clock_w    => clock_w,
        rst_n_w    => rst_n_w,
        enable_w   => mem_enable_w,
        addr_w     => mem_addr_w,
        data_w     => mem_data_w
      );
    
    -- connect output signals
    data_r <= mem_data_r; 

end architecture rtl;
