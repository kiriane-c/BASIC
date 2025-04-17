-- generic_uart
--	universal asynchronous receiver/transmitter and defines a protocol for exchanging serial data
--	between two devices. Communication in UART can be simplex, half-duplex or full_duplex both
--	ends using same baud rates, frame and parameter. UART frame contain start and stop bits, data 
--	bits, and an optional parity bit. Because UART is asynchronous, the transmitter needs to
--	signal that data bits are coming (start bit) and indicate the end of user data (stop bit). 
--	The data bits are reversed in LSB order before sending them out. Parity bit is used for error 
--	detection. Inserted between the end of data bits and stop bit.
--		In even parity, this bit is set such that the total number of 1s in the frame is even
--		In odd parity, this bit is set such that the total number of 1s in the frame is odd
-----------------------------------------------------------------------------------------------------

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


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_uart_rx is
  generic(
  	-- clock frequency of the system in Hz : 
	--	represents how many clock cycles occur in 1s 
  	CLOCK_FREQ  : integer := 100000000;
  	-- baud rate in bits per second
	--	determine how long each bit in serial communication lasts
  	BAUD_RATE   : integer := 9600;
  	-- number of data bits
  	DATA_BITS   : integer range 5 to 9 := 8;
  	-- number of stop bits
  	STOP_BITS   : integer range 1 to 2 := 1;
  	-- parity type: 0 = none, 1 = odd, 2 = even
  	PARITY_TYPE : integer range 0 to 2 := 0
  );
  port(
  	clock	  : in  std_logic;
	reset_n	  : in  std_logic;
	-- UART serial receiver interface
	rx	  : in  std_logic;	-- serial input
	rx_data	  : out std_logic_vector(DATA_BITS-1 downto 0);
	rx_valid  : out std_logic;
	rx_parity : out std_logic
	
  );
end entity generic_uart_rx;

architecture rtl of generic_uart_rx is

  component generic_sync_cell is 
    port(
  	  clock   : in  std_logic;
  	  reset_n : in  std_logic;
  	  async   : in  std_logic;
  	  sync    : out std_logic 
    );
  end component generic_sync_cell;
  
  -- calculate number of clock cycles per bit
  --	tells the FSM how many clock cycle to count before sampling the next bit
  constant CLOCKS_PER_BIT : integer := CLOCK_FREQ/BAUD_RATE;
  
  -- Define state type
  type state_t is (IDLE, START, DATA, PARITY, STOP, DONE);
  
  -- Signal declaration
  signal state : state_t := IDLE;
  
  -- counters and data registers
  signal bit_timer : integer range 0 to CLOCKS_PER_BIT-1 := 0;
  signal bit_count : integer range 0 to DATA_BITS     -1 := 0;
  signal bit_stop  : integer range 0 to STOP_BITS     	 := 0;
  
  -- receive uart signals
  signal rx_sync      : std_logic; 
  signal rx_prt_reg   : std_logic;	-- receive parity register signal
  signal rx_shift_reg : std_logic_vector(DATA_BITS-1 downto 0) := (others => '0');
  
  -- Function to get parity
  function get_parity(data : std_logic_vector; parity_type : integer) return std_logic is
      variable parity : std_logic := '0';
  begin
      if parity_type = 0 then  -- No parity
  	  return '0';
      else
  	  -- Calculate parity over all bits
  	  for i in data'range loop
  	      parity := parity xor data(i);
  	  end loop;
  	  
  	  -- Return appropriate parity bit based on type
  	  if parity_type = 1 then  -- Odd parity
  	      return not parity;
  	  else  -- Even parity
  	      return parity;
  	  end if;
      end if;
  end function get_parity;

begin
  
  -- synchronization process
  sync_cell_inst : generic_sync_cell
  port map(
  	clock  	=> clock,
	reset_n	=> reset_n,
	async  	=> rx,
	sync   	=> rx_sync
  );
  
  -- uart receive state machine process 
  uart_rx_p : process(clock, reset_n)
  begin
  	if reset_n = '0' then
		bit_timer    <= 0;
		bit_count    <= 0;
		bit_stop     <= 0;
		rx_data	     <= (others => '0');
		rx_valid     <= '0';
		rx_parity    <= '0';
		rx_prt_reg   <= '0';
		rx_shift_reg <= (others => '0');
		state	     <= IDLE;
	elsif rising_edge(clock) then
		case state is 
			when IDLE =>
		  	-- look for start bit (falling edge)
			if rx_sync = '0' then
				state <= START;
			else
				state <= IDLE;
			end if;
		
			-- check middle of start bit to make sure it is still low
			when START =>
			-- sample in the middle of start bit
			if bit_timer = (CLOCKS_PER_BIT-1)/2 then
				-- verify it's still low (valid start bit)
				if rx_sync = '0' then
					-- start bit confirmed, prepare for data bits
					bit_timer <= 0;
					state	  <= DATA;
				else
					-- false start, return to idle
					state	  <= IDLE;
				end if;
			else
				bit_timer <= bit_timer + 1;
				state	  <= START;
			end if;
		
			-- wait CLOCKS_PER_BIT-1 clock cycles to sample serial data
			when DATA =>
			if bit_timer = CLOCKS_PER_BIT-1 then
				-- sample the data bit at the end of bit period
				bit_timer <= 0;
				-- shift in the received bit
				rx_shift_reg <= rx_sync & rx_shift_reg(DATA_BITS-1 downto 1);
				-- check if all data bits are received
				if bit_count = DATA_BITS-1 then
					bit_count <= 0;
					-- move to parity check or stop bit based on configuration
					if PARITY_TYPE > 0 then
						state <= PARITY;
					else
						state <= STOP;
					end if;
				else
					bit_count <= bit_count + 1;
				end if;
			else
				bit_timer <= bit_timer + 1;
			end if;
			
			when PARITY =>
			if bit_timer = CLOCKS_PER_BIT-1 then
				bit_timer <= 0;
				
				-- Check parity based on selected type
				rx_parity <= get_parity(rx_shift_reg, PARITY_TYPE);
				
				-- move to stop bit
				state	  <= STOP;
			else
				bit_timer <= bit_timer + 1;
			end if;
		
			-- receive stop bit. stop bit = 1
			when STOP =>
			-- wait CLOCKS_PER_BIT-1 clock cycles for stop bit to finish
			if bit_timer = CLOCKS_PER_BIT-1 then
				bit_timer <= 0;
				-- if all stop bits received
				if bit_stop = STOP_BITS-1 then
					rx_data  <= rx_shift_reg;
					rx_valid <= '1';
					state	 <= DONE;
				else
					bit_stop <= bit_stop + 1;
				end if;
			else
				bit_timer <= bit_timer + 1;
				state	  <= STOP;
			end if;
			
			when DONE =>
			-- wait for line to go idle before resuming
			rx_valid <= '0';
			state	 <= IDLE;

			when OTHERS =>
			state <= IDLE;
		end case;
	end if;
  end process uart_rx_p;

end architecture rtl;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_uart_tx is
  generic(
  	-- clock frequency in Hz
  	CLOCK_FREQ  : integer := 100000000;
  	-- baud rate in bits per second
  	BAUD_RATE   : integer := 9600;
  	-- number of data bits
  	DATA_BITS   : integer range 5 to 9 := 8;
  	-- number of stop bits
  	STOP_BITS   : integer range 1 to 2 := 1;
  	-- parity type: 0 = none, 1 = odd, 2 = even
  	PARITY_TYPE : integer range 0 to 2 := 0
  );
  port(
  	clock	  : in  std_logic;
	reset_n	  : in  std_logic;
	-- UART serial transmitter interface
	tx	  : out std_logic;	-- serial output
	tx_data	  : in  std_logic_vector(DATA_BITS-1 downto 0);
	tx_valid  : in  std_logic;	-- valid transmission (start)
	tx_active : out std_logic;	-- transmission in progress (busy)
	tx_done   : out std_logic	-- transmission done
	
  );
end entity generic_uart_tx;

architecture rtl of generic_uart_tx is
  
  -- calculate number of clock cycles per bit
  constant CLOCKS_PER_BIT : integer := CLOCK_FREQ/BAUD_RATE;
  
  -- Define state type
  type state_t is (IDLE, START, DATA, PARITY, STOP, DONE);
  
  -- Signal declaration
  signal state : state_t := IDLE;
  
  -- counters and data registers
  signal bit_timer : integer range 0 to CLOCKS_PER_BIT-1 := 0;
  signal bit_count : integer range 0 to DATA_BITS     -1 := 0;
  signal bit_stop  : integer range 0 to STOP_BITS     	 := 0;
  signal parity_s  : std_logic;
  
  -- transmit uart signals
  signal tx_reg		: std_logic;	-- tx register
  signal tx_busy	: std_logic;	-- tx_active register
  signal tx_done_reg	: std_logic;	-- tx_done register
  signal tx_shift_reg 	: std_logic_vector(DATA_BITS-1 downto 0) := (others => '0');
  
  -- Function to get parity
  function get_parity(data : std_logic_vector) return std_logic is
      variable parity_v : std_logic := '0';
  begin
      if PARITY_TYPE = 0 then  -- No parity
  	  return '0';
      else
  	  -- Calculate parity over all bits
  	  for i in data'range loop
  	      parity_v := parity_v xor data(i);
  	  end loop;
  	  
  	  -- Return appropriate parity bit based on type
  	  if PARITY_TYPE = 1 then  -- Odd parity
  	      return not parity_v;
  	  else  -- Even parity
  	      return parity_v;
  	  end if;
      end if;
  end function get_parity;

begin

  parity_s  <= get_parity(tx_data);
  tx	    <= tx_reg;
  tx_active <= tx_busy;
  tx_done   <= tx_done_reg;

  -- uart transmit state machine process 
  uart_tx_p : process(clock, reset_n)
  begin
  	if reset_n = '0' then
		bit_timer    <= 0;
		bit_count    <= 0;
		bit_stop     <= 0;
		tx_reg	     <= '0';
		tx_busy      <= '0';
		tx_done_reg  <= '0';
		tx_shift_reg <= (others => '0');
		state	     <= IDLE;
	elsif rising_edge(clock) then
		case state is 
			when IDLE =>
			tx_reg	    <= '1'; -- idle state is 1
			tx_done_reg <= '0';
			if tx_valid = '1' then
			-- load data into shift registers
				tx_shift_reg <= tx_data;
				tx_busy	     <= '1';
				state 	     <= START;
			else
				state        <= IDLE;
			end if;
		
			when START =>
			tx_reg	<= '0';	-- send start bit (always low)
			if bit_timer = CLOCKS_PER_BIT-1 then
			       	bit_timer <= 0;
			       	state	  <= DATA;
			else
				bit_timer <= bit_timer + 1;
				state	  <= START;
			end if;
		
			-- wait CLOCKS_PER_BIT-1 clock cycles to sample serial data
			when DATA =>
			tx_reg <= tx_shift_reg(0);
			if bit_timer = CLOCKS_PER_BIT-1 then
				-- sample the data bit at the end of bit period
				bit_timer <= 0;
				-- shift in the transmission bit
				tx_shift_reg <= '0' & tx_shift_reg(DATA_BITS-1 downto 1);
				-- check if all data bits are received
				if bit_count = DATA_BITS-1 then
					-- move to parity check or stop bit based on configuration
					if PARITY_TYPE > 0 then
						state <= PARITY;
					else
						state <= STOP;
					end if;
				else
					bit_count <= bit_count + 1;
				end if;
			else
				bit_timer <= bit_timer + 1;
			end if;
			
			when PARITY =>
			-- send parity bit
			tx_reg <= parity_s;
			if bit_timer = CLOCKS_PER_BIT-1 then
				bit_timer <= 0;
				
				-- move to stop bit
				state	  <= STOP;
				bit_count <= 0;
			else
				bit_timer <= bit_timer + 1;
			end if;
		
			-- send stop bit. stop bit = 1
			when STOP =>
			tx_reg	<= '1';
			-- wait CLOCKS_PER_BIT-1 clock cycles for stop bit to finish
			if bit_timer = CLOCKS_PER_BIT-1 then
				bit_timer <= 0;
				-- check for valid stop bit
				
				-- if all stop bits received
				if bit_stop = STOP_BITS-1 then
					bit_stop <= 0;
					state	 <= DONE;
				else
					bit_stop <= bit_stop + 1;
				end if;
			else
				bit_timer <= bit_timer + 1;
			end if;
			
			when DONE =>
			tx_busy     <= '0';
			tx_done_reg <= '1';
			state	    <= IDLE;
			
			when OTHERS =>
			state	<= IDLE;
			
		end case;
	end if;
  end process uart_tx_p;

end architecture rtl;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity generic_uart_rx_tx is
  generic(
  	-- clock frequency in Hz
  	CLOCK_FREQ  : integer := 100000000;
  	-- baud rate in bits per second
  	BAUD_RATE   : integer := 9600;
  	-- number of data bits
  	DATA_BITS   : integer range 5 to 9 := 8;
  	-- number of stop bits
  	STOP_BITS   : integer range 1 to 2 := 1;
  	-- parity type: 0 = none, 1 = odd, 2 = even
  	PARITY_TYPE : integer range 0 to 2 := 0
  );
  port(
  	clock	    : in  std_logic;
	reset_n	    : in  std_logic;
	-- UART interface
	rx	    : in  std_logic;
	tx	    : out std_logic;
	-- User interface for received data
	rx_data	    : out std_logic_vector(DATA_BITS-1 downto 0);
	rx_valid    : out std_logic;
	rx_parity   : out std_logic;
	-- User interface for transmit data
	tx_data	    : in  std_logic_vector(DATA_BITS-1 downto 0);
	tx_valid    : in  std_logic;
	tx_active   : out std_logic;
	tx_done	    : out std_logic
  );
end entity generic_uart_rx_tx;

architecture rtl of generic_uart_rx_tx is

  component generic_uart_rx is
    generic( 
  	  CLOCK_FREQ  : integer := 100000000;
  	  BAUD_RATE   : integer := 9600;
  	  DATA_BITS   : integer range 5 to 9 := 8;
  	  STOP_BITS   : integer range 1 to 2 := 1;
  	  PARITY_TYPE : integer range 0 to 2 := 0
    );
    port(
  	  clock     : in  std_logic;
  	  reset_n   : in  std_logic;
  	  rx	    : in  std_logic;
  	  rx_data   : out std_logic_vector(DATA_BITS-1 downto 0);
  	  rx_valid  : out std_logic;
  	  rx_parity : out std_logic
  	  
    );
  end component generic_uart_rx;
  
  component generic_uart_tx is
    generic(
  	  CLOCK_FREQ  : integer := 100000000;
  	  BAUD_RATE   : integer := 9600;
  	  DATA_BITS   : integer range 5 to 9 := 8;
  	  STOP_BITS   : integer range 1 to 2 := 1;
  	  PARITY_TYPE : integer range 0 to 2 := 0
    );
    port(
  	  clock     : in  std_logic;
  	  reset_n   : in  std_logic;
  	  tx	    : out std_logic;	  
  	  tx_data   : in  std_logic_vector(DATA_BITS-1 downto 0);
  	  tx_valid  : in  std_logic;	  
  	  tx_active : out std_logic;
	  tx_done   : out std_logic
    );
  end component generic_uart_tx;

begin
  
   -- Instantiate UART Receiver
   uart_rx_inst: generic_uart_rx
     generic map(
     	CLOCK_FREQ   => CLOCK_FREQ,
     	BAUD_RATE    => BAUD_RATE,
     	DATA_BITS    => DATA_BITS,
     	STOP_BITS    => STOP_BITS,
     	PARITY_TYPE  => PARITY_TYPE
     )
     port map(
     	clock	  => clock,
     	reset_n   => reset_n,
     	rx	  => rx,
     	rx_data   => rx_data,
     	rx_valid  => rx_valid,
     	rx_parity => rx_parity
     );
  
   -- Instantiate UART Transmitter
   uart_tx_inst: generic_uart_tx
     generic map(
   	CLOCK_FREQ   => CLOCK_FREQ,
   	BAUD_RATE    => BAUD_RATE,
   	DATA_BITS    => DATA_BITS,
   	STOP_BITS    => STOP_BITS,
   	PARITY_TYPE  => PARITY_TYPE
     )
     port map(
   	clock	  => clock,
   	reset_n   => reset_n,
   	tx	  => tx,
   	tx_data   => tx_data,
   	tx_valid  => tx_valid,
   	tx_active => tx_active,
	tx_done   => tx_done
     );
  
end architecture rtl;
