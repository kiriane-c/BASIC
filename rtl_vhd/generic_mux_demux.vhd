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
  	sel	      : in  std_logic_vector(SEL_WIDTH-1 downto 0);
	  mode	    : in  std_logic;
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
  
    mux_out	  <= (others => '0');
    demux_out <= (others => '0');
    
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
