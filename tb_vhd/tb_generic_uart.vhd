library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_generic_uart_rx is
end tb_generic_uart_rx;

architecture rtl of tb_generic_uart_rx is

  component generic_uart_rx is
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
  	  clock     : in  std_logic;
  	  reset_n   : in  std_logic;
  	  -- serial input
  	  rx	    : in  std_logic;
  	  rx_data   : out std_logic_vector(DATA_BITS-1 downto 0);
  	  rx_valid  : out std_logic;
  	  rx_parity : out std_logic
  	  
    );
  end component generic_uart_rx;
  
  -- Constants for configuration
  constant CLOCK_PERIOD       : time	:= 10 ns;     -- 100 MHz clock
  constant RESET_TIME	      : time	:= 15 ns;
  constant CLOCK_FREQ	      : integer := 100000000; -- 100 MHz
  constant BAUD_RATE	      : integer := 115200;    -- Standard baud rate
  constant CLOCKS_PER_BIT     : time	:= (1.0/real(BAUD_RATE)) * 1 sec;
  constant DATA_BITS	      : integer := 8;
  constant STOP_BITS	      : integer := 1;
  constant PARITY_TYPE        : integer := 2;	      -- Even parity
  
  -- test cases
  type test_case_type is record
      data  : std_logic_vector(DATA_BITS-1 downto 0);
      parity: std_logic;  -- Calculated parity bit
  end record;
  
  type test_case_array is array (natural range <>) of test_case_type;
  constant TEST_CASES : test_case_array := (
      (data => X"DE", parity => '0'),  -- Even parity for 11011110 is 0
      (data => X"AD", parity => '0'),  -- Even parity for 10101101 is 0
      (data => X"BE", parity => '0'),  -- Even parity for 10111110 is 0
      (data => X"EF", parity => '0'),  -- Even parity for 11101111 is 0
      (data => X"BA", parity => '1'),  -- Even parity for 10111010 is 1
      (data => X"BE", parity => '1'),  -- Even parity for 10111110 is 1
      (data => X"CA", parity => '1'),  -- Even parity for 11001010 is 1
      (data => X"FE", parity => '1')   -- Even parity for 11111110 is 1
  );
  
  -- signals
  signal clock      : std_logic := '0';
  signal reset_n    : std_logic := '0';
  signal rx	    : std_logic := '0';  -- Idle state is high
  signal rx_data    : std_logic_vector(DATA_BITS-1 downto 0);
  signal rx_valid   : std_logic;
  signal rx_parity  : std_logic;
  
  signal stop_clock : boolean := false;
  
  -- Procedure to send one UART frame
  procedure send_uart_frame(
      signal   frame	: out std_logic;
      constant data	: in  std_logic_vector(DATA_BITS-1 downto 0);
      constant parity	: in  std_logic;
      constant bit_stop : in  integer) is
  begin
      -- Start bit (always low)
      frame <= '0';
      wait for CLOCKS_PER_BIT;
      
      -- Data bits (LSB first)
      for i in 0 to DATA_BITS-1 loop
  	  frame <= data(i);
  	  wait for CLOCKS_PER_BIT;
      end loop;
      
      -- Parity bit (if enabled)
      frame <= parity;
      wait for CLOCKS_PER_BIT;
      
      -- Stop bit(s) (always high)
      for i in 1 to bit_stop loop
  	  frame <= '1';
  	  wait for CLOCKS_PER_BIT;
      end loop;
  end procedure;
  
begin
  -- instantiate the UART receiver
  uut: generic_uart_rx
  generic map (
      CLOCK_FREQ  => CLOCK_FREQ,
      BAUD_RATE   => BAUD_RATE,
      DATA_BITS   => DATA_BITS,
      STOP_BITS   => STOP_BITS,
      PARITY_TYPE => PARITY_TYPE
  )
  port map (
      clock	=> clock,
      reset_n	=> reset_n,
      rx	=> rx,
      rx_data	=> rx_data,
      rx_valid  => rx_valid,
      rx_parity => rx_parity
  );
  
  -- stimulus process
  stimulus_p : process
  begin
      -- Reset
      reset_n <= '1' after RESET_TIME;
      wait for CLOCK_PERIOD*2.5;
      
      rx <= '1';
  	      
      -- Test normal reception (all test cases)
      for i in TEST_CASES'range loop
        	  
  	  send_uart_frame(rx, TEST_CASES(i).data, TEST_CASES(i).parity, STOP_BITS);
  	  
  	  -- Wait for data valid or timeout
  	  for j in 0 to 20 loop  -- Maximum wait time
  	      wait for CLOCKS_PER_BIT/2;
  	  end loop;
  	  
  	  -- Wait between test cases
  	  wait for CLOCKS_PER_BIT*2;
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
