library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_generic_adder is
generic(
      WIDTH   : natural := 8
);    
end entity tb_generic_adder;

architecture rtl of tb_generic_adder is

  component generic_half_adder is
    port(
    	  input0  : in  std_logic;
    	  input1  : in  std_logic;
    	  carry_o : out std_logic;
    	  sum_o   : out std_logic
    );
  end component generic_half_adder;
  
  component generic_full_adder is 
    port(
    	  input0  : in  std_logic;
    	  input1  : in  std_logic;
    	  carry_i : in  std_logic;
    	  carry_o : out std_logic;
    	  sum_o   : out std_logic
    );
  end component generic_full_adder;
  
  component generic_ripple_carry_adder is
    generic(
    	  WIDTH   : natural
    );    
    port(
    	  input0  : in  std_logic_vector(WIDTH-1 downto 0);
    	  input1  : in  std_logic_vector(WIDTH-1 downto 0);
    	  carry_i : in  std_logic;
    	  carry_o : out std_logic;
    	  sum_o   : out std_logic_vector(WIDTH-1 downto 0)
    );
  end component generic_ripple_carry_adder;
  
  component generic_carry_lookahead_adder is
    generic(
  	  WIDTH   : natural
    );    
    port(
  	  input0  : in  std_logic_vector(WIDTH-1 downto 0);
  	  input1  : in  std_logic_vector(WIDTH-1 downto 0);
  	  carry_i : in  std_logic;
  	  carry_o : out std_logic;
  	  sum_o   : out std_logic_vector(WIDTH-1 downto 0)
    );
  end component generic_carry_lookahead_adder;
  
  component generic_carry_select_adder is
    generic(
  	  WIDTH   : natural
    );    
    port(
  	  input0  : in  std_logic_vector(WIDTH-1 downto 0);
  	  input1  : in  std_logic_vector(WIDTH-1 downto 0);
  	  carry_i : in  std_logic;
  	  carry_o : out std_logic;
  	  sum_o   : out std_logic_vector(WIDTH-1 downto 0)
    );
  end component generic_carry_select_adder;
  
  component generic_carry_save_adder is
    generic(
    	  WIDTH   : natural := 32
    );
    port(
    	  input0  : in  std_logic_vector(WIDTH-1 downto 0);
    	  input1  : in  std_logic_vector(WIDTH-1 downto 0);
    	  input2  : in  std_logic_vector(WIDTH-1 downto 0);
    	  carry_o : out std_logic;
    	  sum_o   : out std_logic_vector(WIDTH+1 downto 0)
    );
    
  end component generic_carry_save_adder;
    
  -- constants
  CONSTANT CLOCK_PERIOD : time := 50 ns;
  
  -- signals 
  signal clock   : std_logic := '0';
  
  signal input0  : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
  signal input1  : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
  signal input2  : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
  signal carry_i : std_logic := '0';
  signal carry_o : std_logic := '0';
  signal sum_o   : std_logic_vector(WIDTH+1 downto 0) := (others => '0'); -- WIDTH-1 downto 0 for all others except carry-save
  
  signal stop_clock : boolean	:= false;
  
begin

--  -- half-adder testbench
--  uut: generic_half_adder
--  port map(
--	  input0  => input0(0), 
--	  input1  => input1(0), 
--	  carry_o => carry_o,
--	  sum_o   => sum_o(0) 
--  );
  
--  stimulus_p: process
--  begin
--
--  	--  test case 1: 00
--	  input0(0) <= '0';
--	  input1(0) <= '0';
--	  wait for CLOCK_PERIOD;
--	  
--	  --  test case 2: 01
--	  input0(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  --  test case 3: 10
--	  input0(0) <= '0';
--	  input1(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  --  test case 4: 11
--	  input0(0) <= '1';	  
--	  wait for CLOCK_PERIOD;
--
--	  stop_clock <= true;
--	  wait;
--  
--  end process stimulus_p;

--  -- full-adder testbench
--  uut: generic_full_adder
--  port map(
--	  input0  => input0(0), 
--	  input1  => input1(0), 
--	  carry_i => carry_i,
--	  carry_o => carry_o,
--	  sum_o   => sum_o(0)  
--    );
--  
--  stimulus_p: process
--  begin
--  
--	--  test case 1: 000
--	  input0(0)  <= '0';
--	  input1(0)  <= '0';
--	  carry_i <= '0';
--	  wait for CLOCK_PERIOD;
--	  
--	  --  test case 2: 001
--	  input0(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  --  test case 3: 010
--	  input0(0) <= '0';
--	  input1(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  --  test case 4: 011
--	  input0(0) <= '1';  
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 5: 100
--	  input0(0)  <= '0';
--	  input1(0)  <= '0';
--	  carry_i <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 6: 101
--	  input0(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
  	  -- test case 7: 110
--	  input0(0) <= '0';
--	  input1(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 8: 111
--	  input0(0) <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  stop_clock <= true;
--	  wait;
--  
--  end process stimulus_p;

--  -- ripple carry/carry lookahead/carry select adder testbench
--  uut: generic_carry_select_adder
--  generic map(
--	  WIDTH   => 8
--  )
--  port map(
--	  input0  => input0, 
--	  input1  => input1, 
--	  carry_i => carry_i,
--	  carry_o => carry_o,
--	  sum_o   => sum_o  
--  );
--  
--  stimulus_p: process
--	  variable seed1 : positive;
--	  variable seed2 : positive;
--	  variable rand  : real;
--  begin
--	  -- test case 1: simple addition without carry
--	  input0  <= "00000011";
--	  input1  <= "00000101";
--	  carry_i <= '0';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 2: simple addition with carry
--	  input0  <= "00000111";
--	  input1  <= "00000110";
--	  carry_i <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 3: overflow addition without carry
--	  input0  <= "11110000";
--  	  input1  <= "01010101";
--	  carry_i <= '0';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 4: maximum addition without carry
--	  input0  <= "11111111";
--	  input1  <= "11111111";
--	  carry_i <= '0';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 5: maximum addition with carry
--	  input0  <= "11111111";
--	  input1  <= "11111111";
--	  carry_i <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 6: carry propagate
--	  input0  <= "10000001";
--	  input1  <= "01111111";
--	  carry_i <= '1';
--	  wait for CLOCK_PERIOD;
--	  
--	  -- test case 7: random test
--	  for i in 1 to 10 loop
--		  uniform(seed1, seed2, rand);
--		  input0  <= std_logic_vector(to_unsigned(integer(rand * 512.0), 8));
--		  
--		  uniform(seed1, seed2, rand);
--		  input1  <= std_logic_vector(to_unsigned(integer(rand * 256.0), 8));
--		  
--		  if input0(0) = '1' or input1(0) = '1' then
--			  carry_i <= '1';
--		  else
--			  carry_i <= '0';
--		  end if;
--		  
--		  wait for CLOCK_PERIOD;
--	  end loop;
--	  
--	  stop_clock <= true;
--	  wait;
--	  
--  end process stimulus_p;

  -- carry-save adder testbench
  uut: generic_carry_save_adder
  generic map(
  	WIDTH	=> 8
  )
  port map(
  	input0  => input0, 
  	input1  => input1, 
  	input2	=> input2,
	  carry_o	=> carry_o,
  	sum_o	  => sum_o
  );
  
  stimulus_p : process
  	variable seed1 : positive;
	  variable seed2 : positive;
	  variable rand  : real;
  begin	
  	wait for CLOCK_PERIOD;
	  -- Test Case 1: test with small numbers
	  input0 <= std_logic_vector(to_unsigned(3, WIDTH));
	  input1 <= std_logic_vector(to_unsigned(5, WIDTH));
	  input2 <= std_logic_vector(to_unsigned(7, WIDTH));
	  wait for CLOCK_PERIOD;
	
	  -- Test Case 2: test with larger values
	  input0 <= std_logic_vector(to_unsigned(127, WIDTH));
	  input1 <= std_logic_vector(to_unsigned(63, WIDTH));
	  input2 <= std_logic_vector(to_unsigned(31, WIDTH));
	  wait for CLOCK_PERIOD;
	
	  -- Test Case 3: Test with maximum values
	  input0 <= (others => '1');  -- 2^WIDTH - 1
	  input1 <= (others => '1');
	  input2 <= (others => '1');
	  wait for CLOCK_PERIOD;
	
	  -- Test case 4: random test
	  for i in 1 to 30 loop
		  uniform(seed1, seed2, rand);
		  input0  <= std_logic_vector(to_unsigned(integer(rand * 512.0), 8));
		  uniform(seed1, seed2, rand);
		  input1  <= std_logic_vector(to_unsigned(integer(rand * 256.0), 8));
		  uniform(seed1, seed2, rand);
		  input2  <= std_logic_vector(to_unsigned(integer(rand * 128.0), 8));
		
		  wait for CLOCK_PERIOD;
	end loop;
	
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
