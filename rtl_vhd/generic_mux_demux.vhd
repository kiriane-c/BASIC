-- Multiplexer / Demultiplexer
-- Flat and Tree structure

-- Flat Structure Xtics :
--	direct and immediate selection
--	number of gates directly proportional to number of inputs
--	constant latency, independent of input number
--	power consumption and sureface area proportional to inputs
-------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity generic_flat_mux_demux is
  generic(
  	NUM_INOUT : integer; -- := 64; -- number of inputs/outputs
	SEL_WIDTH : integer; -- := integer(ceil(log2(real(NUM_INOUT)))); -- select width
	INO_WIDTH : integer  -- := 8 -- input/output width
  );
  port(
  	sel	  : in  std_logic_vector(SEL_WIDTH-1 downto 0);
	mode	  : in  std_logic;
	-- multiplexer mode --
	mux_in	  : in  std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0);
	mux_out	  : out std_logic_vector(INO_WIDTH-1 downto 0);
	-- demultipexer mode --
	demux_in  : in  std_logic_vector(INO_WIDTH-1 downto 0);
	demux_out : out std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0)
  );
end entity generic_flat_mux_demux;

architecture rtl of generic_flat_mux_demux is

begin

  -- Multiplexer/Demultiplexer selection process --
  flat_mux_demux_p : process(mode, mux_in, demux_in, sel)
    variable index : integer;
  begin
  
    mux_out	<= (others => '0');
    demux_out 	<= (others => '0');
    
    index := to_integer(unsigned(sel))*INO_WIDTH;
    
    if mode = '0' then
    -- multiplexer mode --
    	mux_out <= mux_in(index+INO_WIDTH-1 downto index);
    else
    -- demultiplexer mode --
    	demux_out(index+INO_WIDTH-1 downto index) <= demux_in;
    end if;
    
  end process flat_mux_demux_p;

end architecture rtl;

-- Tree Structure Xtics :
--	multi-level selection
--	logarithmic reduction of gates
--	latency increases logarithmically with input/output number
--	less power power consumption for an increase in input/outputs
--	more scalable 
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity generic_tree_mux_demux is
  generic(
  	NUM_INOUT : integer; -- := 64; -- number of inputs/outputs
	SEL_WIDTH : integer; -- := integer(ceil(log2(real(NUM_INOUT)))); -- select width
	INO_WIDTH : integer  -- := 8 -- input/output width
  );
  port(
  	sel	  : in  std_logic_vector(SEL_WIDTH-1 downto 0);
	mode	  : in  std_logic;
	-- multiplexer mode --
	mux_in	  : in  std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0);
	mux_out	  : out std_logic_vector(INO_WIDTH-1 downto 0);
	-- demultipexer mode --
	demux_in  : in  std_logic_vector(INO_WIDTH-1 downto 0);
	demux_out : out std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0)
  );
end entity generic_tree_mux_demux;

architecture rtl of generic_tree_mux_demux is
  -- signal declaration --
  type sel_matrix is array(integer(log2(real(NUM_INOUT))) downto 0) 
  	of std_logic_vector((NUM_INOUT*INO_WIDTH)-1 downto 0);
  
  signal mux_tree   : sel_matrix;
  signal demux_tree : sel_matrix;
  
begin

  tree_mux_demux_p: process(mode, mux_in, demux_in, sel)
    variable inputs_level : integer;
    variable stride_level : integer;
  begin
  	mux_tree(0)   <= mux_in;
	
	for i in 0 to NUM_INOUT-1 loop
		demux_tree(0)((i+1)*INO_WIDTH-1 downto i*INO_WIDTH) <= demux_in;
	end loop;
	
	mux_out	  <= (others => '0');
    	demux_out <= (others => '0');
	
	if mode = '0' then
		-- multiplexer mode --
		for level in 0 to SEL_WIDTH - 1 loop
			inputs_level := NUM_INOUT/(2**(level+1));
			stride_level := INO_WIDTH*(2**(level+1));
			-- reduce input by level based on select bit --
			for i in 0 to inputs_level - 1 loop
				if sel(level) = '1' then
					mux_tree(level+1)((i*stride_level) + (stride_level/2) - 1 downto (i*stride_level)) <= 
					mux_tree(level)((i*stride_level*2) + (stride_level)   - 1 downto (i*stride_level*2));
				else
					mux_tree(level+1)((i*stride_level) + (stride_level/2) - 1 downto (i*stride_level)) <= 
					mux_tree(level)((i*stride_level*2) + (stride_level/2) - 1 downto (i*stride_level*2));
				end if;
			end loop;
		end loop;
		
		mux_out	<= mux_tree(SEL_WIDTH)(INO_WIDTH-1 downto 0);
	else
		-- demultiplexer mode --
		for level in 0 to SEL_WIDTH - 1 loop
			inputs_level := NUM_INOUT/(2**(level+1));
			stride_level := INO_WIDTH*(2**(level+1));
			-- route input to specific output branch --
			for i in 0 to inputs_level - 1 loop
				if sel(level) = '1' then
				-- right branch routing --
					demux_tree(level+1)((i*stride_level) + (stride_level) - 1 downto (i*stride_level)+(stride_level/2)) <= 
					demux_in(INO_WIDTH-1 downto 0);
				else
				-- left branch routing --
					demux_tree(level+1)((i*stride_level) + (stride_level/2) - 1 downto (i*stride_level)) <= 
					demux_in(INO_WIDTH-1 downto 0);
				end if;
			end loop;
		end loop;
		
		demux_out <= demux_tree(SEL_WIDTH);
	end if;
	
  end process tree_mux_demux_p;

end architecture rtl;
