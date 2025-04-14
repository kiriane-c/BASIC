-- generic half-adder 
--	digital circuit which adds two inputs bits and generates a carry and a sum output
-----------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity generic_half_adder is
  port(
  	input0	: in  std_logic;
	input1	: in  std_logic;
	carry_o	: out std_logic;
	sum_o	: out std_logic
  );
end entity generic_half_adder;

architecture rtl of generic_half_adder is

begin
  
  -- **********************************************************************************
  -- The Core of the problem --
  -- **********************************************************************************
	sum_o	<= input0 xor input1;
	carry_o	<= input0 and input1;

end architecture rtl;

-- generic full-adder
--	digital circuit which add three inputs and generates a carry and a sum output
--	The difference btw half and full is that full has an input carry
--------------------------------------------------------------------------------------
-- generic full-adder from half-adder

library ieee;
use ieee.std_logic_1164.all;

entity generic_full_adder is
  port(
  	input0  : in  std_logic;
  	input1  : in  std_logic;
  	carry_i	: in  std_logic;
	carry_o	: out std_logic;
  	sum_o	: out std_logic
  );
end entity generic_full_adder;

architecture rtl of generic_full_adder is

  component generic_half_adder is
    port(
    	  input0  : in  std_logic;
    	  input1  : in  std_logic;
    	  carry_o : out std_logic;
    	  sum_o   : out std_logic
    );
  end component generic_half_adder;
  
  signal and_s    : std_logic;
  signal xor_s    : std_logic;
  signal carry_s  : std_logic;
  
begin

  -- **********************************************************************************
  -- The Core of the problem --
  -- **********************************************************************************

  half_adder_inst0: generic_half_adder
  port map(
  	input0 	=> input0, 
  	input1 	=> input1, 
  	carry_o	=> and_s,
  	sum_o  	=> xor_s
  );
  
  half_adder_inst1: generic_half_adder
  port map(
  	input0  => xor_s,
  	input1  => carry_i,
  	carry_o => carry_s,
  	sum_o	=> sum_o
  );
  
  carry_o <= and_s or carry_s;

end architecture rtl;

-- generic ripple carry adder
--	the simplest way to build an n-bit carry propagate adder is to chain together 
--	n full adders. The carry_o of one stage acts as the carry_i of the next.
--	a good application of modularity and regularity.
-------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity generic_ripple_carry_adder is
  generic(
  	WIDTH	: natural := 32
  );	
  port(
  	input0  : in  std_logic_vector(WIDTH-1 downto 0);
  	input1  : in  std_logic_vector(WIDTH-1 downto 0);
  	carry_i : in  std_logic;
  	carry_o : out std_logic;
  	sum_o	: out std_logic_vector(WIDTH-1 downto 0)
  );
end entity generic_ripple_carry_adder;

architecture rtl of generic_ripple_carry_adder is

  component generic_full_adder is
    port(
    	  input0  : in  std_logic;
    	  input1  : in  std_logic;
    	  carry_i : in  std_logic;
    	  carry_o : out std_logic;
    	  sum_o   : out std_logic
    );
  end component generic_full_adder;
  
  signal carry	  : std_logic_vector(WIDTH downto 0); -- carry signal

begin

  -- **********************************************************************************
  -- The Core of the problem --
  -- **********************************************************************************

  carry(0) <= carry_i;

  full_adder_gen : for i in 0 to WIDTH-1 generate
  	full_adder_inst : generic_full_adder
	port map(
		input0 	=> input0(i), 
		input1  => input1(i),
		carry_i => carry(i),
		carry_o => carry(i+1),
		sum_o   => sum_o(i) 
	);
  end generate;
  
  carry_o <= carry(WIDTH);

end architecture rtl;

-- generic carry lookahead adder
--	in this concept, the bits necessary for addition are immediately available
--	whereas every adder section needs to hold its time of arrival of carry from
--	the previous adder block. This delay is the propagation delay.
-----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity generic_carry_lookahead_adder is
  generic(
  	WIDTH	: natural := 32
  );	
  port(
  	input0  : in  std_logic_vector(WIDTH-1 downto 0);
  	input1  : in  std_logic_vector(WIDTH-1 downto 0);
  	carry_i : in  std_logic;
  	carry_o : out std_logic;
  	sum_o	: out std_logic_vector(WIDTH-1 downto 0)
  );
end entity generic_carry_lookahead_adder;

architecture rtl of generic_carry_lookahead_adder is
  
  component generic_half_adder is
    port(
  	  input0  : in  std_logic;
  	  input1  : in  std_logic; 	 
  	  carry_o : out std_logic;
  	  sum_o   : out std_logic
    );
  end component generic_half_adder;
  
  signal carry	  : std_logic_vector(WIDTH   downto 0); -- carry signal
  signal gen	  : std_logic_vector(WIDTH-1 downto 0); -- generate signal
  signal prop	  : std_logic_vector(WIDTH-1 downto 0);	-- propagation signal
	
begin

  -- **********************************************************************************
  -- The Core of the problem --
  -- **********************************************************************************

  carry(0) <= carry_i;

  prop_gen : for i in 0 to WIDTH-1 generate
  	half_adder_inst : generic_half_adder
	port map(
		input0 	=> input0(i), 
		input1  => input1(i),
		carry_o => gen(i),	-- G(A,B) = A and B
		sum_o   => prop(i) 	-- P(A,B) = A xor B
	);
	
  	carry(i+1) <= (prop(i) and carry(i)) or gen(i);	-- Ci+1 = Gi + (Pi and Ci)
	
	sum_o(i)   <= prop(i) xor carry(i);		-- Si = Pi xor Ci
	
  end generate;
  
  carry_o <= carry(WIDTH);

end architecture rtl;

-- generic carry select adder
--	adding two n-bit numbers with a carry-select adder is done with two adders (ripple-carry/lookahead adders), 
--	in order to perform the calculation twice, one time with the assumption of the carry-in being zero 
--	and the other assuming it will be one. After the two results are calculated, the correct sum, as well 
--	as the correct carry-out, is then selected with the multiplexer once the correct carry-in is known
-------------------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity generic_carry_select_adder is
  generic(
  	WIDTH	: natural := 8
  );
  port(
       	input0  : in  std_logic_vector(WIDTH-1 downto 0);
       	input1  : in  std_logic_vector(WIDTH-1 downto 0);
       	carry_i : in  std_logic;
       	carry_o : out std_logic;
       	sum_o   : out std_logic_vector(WIDTH-1 downto 0)
  );
end entity generic_carry_select_adder;

architecture rtl of generic_carry_select_adder is

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
  
  constant NUM_INOUT_C	: natural := 02;
  constant SEL_WIDTH_C	: natural := integer(ceil(log2(real(NUM_INOUT_C))));
  
  -- For sum multiplexer
  signal sum0 		: std_logic_vector(WIDTH-1 downto 0); -- sum when carry_in = 0
  signal sum1 		: std_logic_vector(WIDTH-1 downto 0); -- sum when carry_in = 1
  signal mux_in_sum 	: std_logic_vector((2*WIDTH)-1 downto 0);
  
  -- For carry multiplexer
  signal carry0 	: std_logic; -- carry out when carry_in = 0
  signal carry1 	: std_logic; -- carry out when carry_in = 1
  signal mux_in_carry 	: std_logic_vector(1 downto 0);
  
  -- Selection signal
  signal sel_s 		: std_logic_vector(0 downto 0);
    
begin

  -- **********************************************************************************
  -- The Core of the problem --
  -- **********************************************************************************

  -- when c0 = '0' 
  ripple_carry0_adder_inst : generic_ripple_carry_adder
    generic map(
    	WIDTH	=> WIDTH
    )
    port map(
    	input0 	=> input0,
	input1 	=> input1,
	carry_i	=> '0',
	carry_o	=> carry0,
	sum_o  	=> sum0
    );
  
  -- when c0 = '1'  
  ripple_carry1_adder_inst : generic_ripple_carry_adder
    generic map(
  	WIDTH	=> WIDTH
    )
    port map(
  	input0  => input0,
  	input1  => input1,
  	carry_i => '1',
  	carry_o => carry1,
  	sum_o	=> sum1
    );
    
  -- generate outputs
  sel_s(0) <= carry_i;
  
  -- generate sum output
  mux_in_sum(WIDTH-1 downto 0) 		<= sum0;
  mux_in_sum((2*WIDTH)-1 downto WIDTH) 	<= sum1;
   
  flat_mux_sum_inst : generic_flat_mux_demux
    generic map(
    	NUM_INOUT => NUM_INOUT_C, 
	SEL_WIDTH => SEL_WIDTH_C, 
	INO_WIDTH => WIDTH
    )
    port map(
    	sel	  => sel_s, 
	mode	  => '0',
	mux_in    => mux_in_sum,
	mux_out   => sum_o,
	demux_in  => (others => '0'),
	demux_out => open
    );    
  
  -- generate carry_o output
  mux_in_carry(0) <= carry0;
  mux_in_carry(1) <= carry1;
    
  flat_mux_carry_inst : generic_flat_mux_demux
  generic map(
      NUM_INOUT  => NUM_INOUT_C, 
      SEL_WIDTH  => SEL_WIDTH_C, 
      INO_WIDTH  => 1
  )   
  port map(
      sel	 => sel_s,
      mode	 => '0',
      mux_in	 => mux_in_carry,
      mux_out(0) => carry_o,
      demux_in   => (others => '0'),
      demux_out  => open
  );	

end architecture rtl;

-- generic carry-save adder
--	type of digital circuit used to compute the sum of three or more binary numbers.
--	differs from other digital adders in that it outputs two or more numbers and the answer 
--	from original summation can be achieved by adding these ouputs together. Used in binary multiplier
--	Given 3 n-bit numbers, this adder produces a partial sum ps and a shift-carry sc:
-- 	psi = ai xor bi xor ci
--	sci = (ai and bi) or (ai and ci) or (bi and ci)
--	The entire sum can be computed by :
--		shifting sc left by one place
--		appending a 0 on MSB of ps
--		add both using a ripple carry adder to produce the resulting (n+1) bit value
--	The use of generic_shift_reg (pipo) caused delay.
----------------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity generic_carry_save_adder is
  generic(
  	WIDTH	: natural := 32
  );
  port(
  	input0  : in  std_logic_vector(WIDTH-1 downto 0);
  	input1  : in  std_logic_vector(WIDTH-1 downto 0);
  	input2  : in  std_logic_vector(WIDTH-1 downto 0);
	carry_o	: out std_logic;
  	sum_o	: out std_logic_vector(WIDTH+1 downto 0)	-- in order to add 3 inputs with n bits, result is n+2
  );
end entity generic_carry_save_adder;

architecture rtl of generic_carry_save_adder is 
  
  component generic_ripple_carry_adder is
  generic(
  	WIDTH	: natural
  );	
  port(
  	input0  : in  std_logic_vector(WIDTH-1 downto 0);
  	input1  : in  std_logic_vector(WIDTH-1 downto 0);
  	carry_i : in  std_logic;
  	carry_o : out std_logic;
  	sum_o	: out std_logic_vector(WIDTH-1 downto 0)
  );  
  end component generic_ripple_carry_adder;

  signal partial_sum 	: std_logic_vector(WIDTH-1 downto 0);
  signal partial_carry 	: std_logic_vector(WIDTH-1 downto 0);
  
  -- Extended signals for the ripple carry adder
  signal extended_sum 	: std_logic_vector(WIDTH+1 downto 0);
  signal extended_carry : std_logic_vector(WIDTH+1 downto 0);
      
begin

  -- **********************************************************************************  
  -- The Core of the problem --
  -- **********************************************************************************
  
  -- generate partial outputs
  partial_out_gen : for i in 0 to WIDTH-1 generate
  	partial_sum(i) 	 <= input0(i) xor input1(i) xor input2(i);
	partial_carry(i) <= (input0(i) and input1(i)) or (input0(i) and input2(i)) or (input1(i) and input2(i));
  end generate;
  
  -- Extended carry with left shift
  extended_carry(0) 		 <= '0';  -- LSB is 0 after shift
  extended_carry(WIDTH downto 1) <= partial_carry(WIDTH-1 downto 0);  -- Shifted carry
  extended_carry(WIDTH+1) 	 <= '0';  -- MSB padding
    
  -- append zero's on MSB of partial sum
  -- extended inputs for the ripple carry adder
  extended_sum(WIDTH-1 downto 0)     <= partial_sum;
  extended_sum(WIDTH+1 downto WIDTH) <= "00";  -- Pad with zeros
  
  -- add out_carry and out_sum to get (n+1) bit final sum value
  ripple_carry_inst: generic_ripple_carry_adder
  generic map(
  	WIDTH	=> WIDTH+2
  )
  port map(
  	input0 	=> extended_carry,
	input1 	=> extended_sum,
	carry_i	=> '0',
	carry_o	=> carry_o,
	sum_o	=> sum_o
  );
  
end architecture rtl;
  
-- generic carry-skip adder
--	also known as carry-bypass adder, is an adder implementation that improves on the delay of
--	a ripple-carry adder with little efforts compared to other adders. The improvement of worse
--	case delay is achieved using several caary skip adders to form a block-carry-skip adder.
--	The n-bit-carry-skip adder consists of n-bit-carry-ripple-chain, n-input AND-gate and one 
--	multiplexer. Each propagate bit pi is connected to the n-input AND-gate. The resulting bit
--	is used as the select bit of a multiplexer that switches either the last carry-bit cn or the 
--	carry-in c0 to the carry-out signal cout.
--	In order to build a 4-bit carry-bypass adder, 6 full-adders would be needed. The first two full
--	adders would add the first two bits together.
--	Consider a 32-bit carry-skip adder divided into four groups, each containing 4 bits
-----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity generic_carry_skip_adder is
  generic(
    WIDTH   : natural := 32
  );
  port(
    input0  : in  std_logic_vector(WIDTH-1 downto 0);
    input1  : in  std_logic_vector(WIDTH-1 downto 0);
    carry_i : in  std_logic;
    carry_o : out std_logic;
    sum_o   : out std_logic_vector(WIDTH-1 downto 0)
  );
end entity generic_carry_skip_adder;

architecture rtl of generic_carry_skip_adder is

  -- Constants for block size and number of blocks
  constant BLOCK_SIZE : natural := 4;
  constant NUM_BLOCKS : natural := WIDTH/BLOCK_SIZE;
  
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
  
  component generic_flat_mux_demux is
    generic(
      NUM_INOUT : integer; 
      SEL_WIDTH : integer; 
      INO_WIDTH : integer  
    );
    port(
      sel       : in  std_logic_vector(SEL_WIDTH-1 downto 0);
      mode      : in  std_logic;
      mux_in    : in  std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0);
      mux_out   : out std_logic_vector(INO_WIDTH-1 downto 0);
      demux_in  : in  std_logic_vector(INO_WIDTH-1 downto 0);
      demux_out : out std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0)
    );
  end component generic_flat_mux_demux;
    
  type slv_array_t is array (0 to NUM_BLOCKS-1) of std_logic_vector(BLOCK_SIZE-1 downto 0);
  type sv_array_t  is array (0 to NUM_BLOCKS-1) of std_logic;
  
  -- input division into blocks
  signal input_0 : slv_array_t;
  signal input_1 : slv_array_t;
  
  -- propagate signals
  signal p_block : slv_array_t; -- propagate signals per block
  signal p_group : sv_array_t;  -- group propagate signals (AND of all p in a block)
  
  signal carry_i_block  : sv_array_t; -- carry input to each block
  signal carry_o_block  : sv_array_t; -- carry output from each block
  signal carry_skip     : sv_array_t; -- carry skip signal
  
  -- sum outputs from each block
  signal sum_block : slv_array_t;
  
  -- multiplexer signals for each block (except first)
  type mux_in_array_t  is array (1 to NUM_BLOCKS-1) of std_logic_vector(1 downto 0);
  type mux_out_array_t is array (1 to NUM_BLOCKS-1) of std_logic_vector(0 downto 0);
  type mux_sel_array_t is array (1 to NUM_BLOCKS-1) of std_logic_vector(0 downto 0);
  
  signal mux_in  : mux_in_array_t;
  signal mux_out : mux_out_array_t;
  signal mux_sel : mux_sel_array_t;
    
begin

  -- ********************************************************************************** 
  -- The Core of the problem --
  -- **********************************************************************************
  
  -- divide inputs into blocks
  input_gen : for i in 0 to NUM_BLOCKS-1 generate
    input_0(i) <= input0(((i+1)*BLOCK_SIZE)-1 downto i*BLOCK_SIZE);
    input_1(i) <= input1(((i+1)*BLOCK_SIZE)-1 downto i*BLOCK_SIZE);
  end generate;
  
  -- Initialize the first carry_skip with input carry
  carry_skip(0) <= carry_i;
  
  -- first block (no carry skip logic)
  first_block: generic_ripple_carry_adder
  generic map(
    WIDTH   => BLOCK_SIZE
  )
  port map(
    input0  => input_0(0),
    input1  => input_1(0),
    carry_i => carry_i,
    carry_o => carry_o_block(0),
    sum_o   => sum_block(0)
  );
  
  -- generate propagate signals for each bit position
  -- p_i = a_i XOR b_i
  propagate_gen: for i in 0 to NUM_BLOCKS-1 generate
    prop_bit_gen: for j in 0 to BLOCK_SIZE-1 generate
      p_block(i)(j) <= input_0(i)(j) xor input_1(i)(j);
    end generate;
  end generate;
  
  -- generate group propagate signal for each block (AND of all p in block)
  group_prop_gen: for i in 0 to NUM_BLOCKS-1 generate
    process(p_block(i))
      variable block_v : std_logic;
    begin
      block_v := '1';  -- initialize to '1' for AND operation
      for j in 0 to BLOCK_SIZE-1 loop
        block_v := block_v and p_block(i)(j);
      end loop;
      p_group(i) <= block_v;
    end process;
  end generate;
    
  -- carry skip logic for remaining blocks
  carry_logic: for i in 1 to NUM_BLOCKS-1 generate
  
    -- Use multiplexer to select between carry bypass or ripple carry
    mux_sel(i)(0) <= p_group(i-1);        -- select bypass if all bits in previous block are in propagate mode
    mux_in(i)(0)  <= carry_o_block(i-1);  -- ripple carry from previous block
    mux_in(i)(1)  <= carry_skip(i-1);  	  -- bypass carry (original carry input)
    
    -- calculate carry in for each block
    flat_mux_carry_inst : generic_flat_mux_demux
    generic map(
      NUM_INOUT  => 2, 
      SEL_WIDTH  => 1, 
      INO_WIDTH  => 1
    )  
    port map(
      sel       => mux_sel(i),
      mode      => '0',
      mux_in    => mux_in(i),
      mux_out   => mux_out(i),
      demux_in  => "0",
      demux_out => open
    );    
    
    carry_i_block(i) <= mux_out(i)(0);
                          
    -- generate ripple carry adder for each block
    block_adder: generic_ripple_carry_adder
    generic map(
      WIDTH   => BLOCK_SIZE
    )
    port map(
      input0  => input_0(i),
      input1  => input_1(i),
      carry_i => carry_i_block(i),
      carry_o => carry_o_block(i),
      sum_o   => sum_block(i)
    );
    
    -- update carry_skip for next block
    carry_skip(i) <= carry_i_block(i);
    
  end generate;
  
  -- output assignments
  output_assign: for i in 0 to NUM_BLOCKS-1 generate
    sum_o(((i+1)*BLOCK_SIZE)-1 downto i*BLOCK_SIZE) <= sum_block(i);
  end generate;
  
  -- final carry out
  carry_o <= carry_o_block(NUM_BLOCKS-1);

end architecture rtl;
