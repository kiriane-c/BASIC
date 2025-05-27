--     Inter-Integrated Circuit is a two wire serial communication protocol using a serial data line (SDA)
--     and a serial clock line (SCL). The protocol supports multiple target devices on a communication
--     bus and can also support multiple controllers that send and receive commands and data. An I2C 
--     system features two shared communication lines for all devices on the bus. These two lines 
--     are used for bidirectional, half-duplex communication.
--     I2C communication is initiated from the controller device with an I2C START condition. If the bus is open, an I2C
--     controller claims the bus for communication by sending an I2C START. To do this, the controller device first pulls
--     the SDA low and then pulls the SCL low. This sequence indicates that the controller device is claiming the I2C
--     bus for communication, forcing other controller devices on the bus to hold their communication.
--     When the controller device has completed communication, the SCL releases high and then the SDA releases
--     high. This indicates an I2C STOP condition.
 
library ieee;
use ieee.std_logic_1164.all;

entity generic_i2c_master is
  generic(
  	CLOCK_FREQ  : integer := 50_000_000;  -- system clock frequency in Hz
       	I2C_FREQ    : integer := 100_000;     -- i2c clock frequency in Hz (standard mode)
       	WIDTH	    : integer := 7	      -- address width (7 or 10 bits)
  );
  port(
  	-- system signal --
       	clock	   : in  std_logic; -- system clock	
       	reset_n    : in  std_logic; -- active low-reset
	-- control interface --
	start	   : in  std_logic; -- start transaction
	stop	   : in  std_logic; -- stop transaction
	mode	   : in  std_logic; -- read (0)/write(1) operation
	busy	   : out std_logic; -- busy flag
	ack	   : out std_logic; -- acknowledge flag
	-- data interface --
       	address    : in  std_logic_vector(WIDTH-1 downto 0); -- slave address
       	wdata      : in  std_logic_vector(WIDTH-1 downto 0); -- write data
       	rdata      : out std_logic_vector(WIDTH-1 downto 0); -- read data
	valid	   : out std_logic; -- data valid signal
	-- i2c interface --
       	scl	   : inout std_logic; -- serial clock line
       	sda	   : inout std_logic  -- serial data line
  );
end entity generic_i2c_master;
 
architecture rtl of generic_i2c_master is

  -- Constants
  constant CLOCK_DIV : integer := (CLOCK_FREQ/I2C_FREQ)/4;  -- clock divider
  
  -- Types
  type state_t is (IDLE, CTRL, SLV_ADDR, ACK_ADDR, WRITE_DATA, READ_DATA, ACK_DATA, QUIT);
  
  -- Internal signals
  signal state      : state_t;
  signal i2c_clk    : std_logic;
  signal i2c_phase  : integer range 0 to 3;  -- 0: First quarter, 1: Second quarter, 2: Third quarter, 3: Fourth quarter
  signal sda_s      : std_logic;
  signal scl_s      : std_logic;
  signal ack_s      : std_logic;
  signal busy_s     : std_logic;
  signal bit_count  : integer range 0 to WIDTH;
  signal clk_count  : integer range 0 to (CLOCK_DIV*4)-1;
  signal shift_reg  : std_logic_vector(WIDTH downto 0);

begin

  -- I2C clock generation
  i2c_clock_p : process(clock, reset_n)
  begin
      if reset_n = '0' then
  	  clk_count <= 0;
	  i2c_phase <= 0;
  	  i2c_clk   <= '0';
      elsif rising_edge(clock) then
  	  if busy_s = '1' then
  	      if clk_count = (CLOCK_DIV*4)-1 then
  		  clk_count <= 0;
  	      else
  		  clk_count <= clk_count + 1;
  	      end if;
  	      
  	      i2c_phase <= clk_count/CLOCK_DIV;
  	      
  	      -- I2C clock generation (SCL)
  	      if clk_count < CLOCK_DIV*2 then
  		  i2c_clk <= '0';
  	      else
  		  i2c_clk <= '1';
  	      end if;
  	  else
  	      clk_count <= 0;
  	      i2c_phase <= 0;
	      i2c_clk   <= '1';
  	  end if;
      end if;
  end process i2c_clock_p;

  i2c_fsm_p : process(clock, reset_n)
  begin
       if(reset_n = '0') then
       		state     <= IDLE;
               	busy_s    <= '0'; -- available
		ack	  <= '0';
	       	ack_s	  <= '0'; -- address and data acknowledge signal received
               	scl_s     <= '1';
               	sda_s     <= '1';
	       	valid	  <= '0';
	       	rdata	  <= (others => '0');
		shift_reg <= (others => '0');
		bit_count <= 0;
       elsif rising_edge(clock) then
       		-- default value
		valid	<= '0';
		busy_s	<= '1';
		
               	case state is
		
        	       	when IDLE =>
			busy_s	  <= '0';
			bit_count <= WIDTH;
			if start = '1' then
			    busy_s <= '1'; -- not available
			    state  <= CTRL;
			end if;
			
			when CTRL =>
			if i2c_phase = 0 then
			    scl_s  <= '1';
			    sda_s  <= '1';
			elsif i2c_phase = 1 then
			    scl_s  <= '1'; 
			    sda_s  <= '0'; -- start condition: SDA goes low while SCL stays high
			elsif i2c_phase = 2 then
			    scl_s  <= '0'; -- master pulls scl low to start transmission
			    sda_s  <= '0';
			    -- prepare address with R/W bit
			    if mode = '0' then
			    	    shift_reg <= address & '1';
			    else
			    	    shift_reg <= address & '0';
			    end if;
			    state <= SLV_ADDR;
			end if;
			
			when SLV_ADDR =>
			if i2c_phase = 0 then
			    scl_s  <= '0';
			    sda_s  <= shift_reg(bit_count);
			elsif i2c_phase = 2 then
			    scl_s  <= '1';
			elsif i2c_phase = 3 then
			    if bit_count = 0 then
			    	bit_count <= WIDTH;
			    	state	  <= ACK_ADDR;
			    else
			    	bit_count <= bit_count - 1;
			    end if;
			end if;
			
			when ACK_ADDR =>
			if i2c_phase = 0 then
			    scl_s  <= '0';  
			    sda_s  <= '1'; -- release SDA to read ACK
			elsif i2c_phase = 2 then
			    scl_s  <= '1';
			    ack_s  <= not sda;  -- slave ACK is low
			elsif i2c_phase = 3 then
                            if ack_s = '1' then -- address complete
                            	if mode = '0' then
                            	    state     <= READ_DATA;
                            	else
                            	    state     <= WRITE_DATA;
                            	    shift_reg <= wdata;  -- load data to write
                            	end if;
                            else
                            	ack   <= '1';
                            	state <= QUIT;
                            end if;
			end if;
			
	                when WRITE_DATA =>	                    
	                if i2c_phase = 0 then
	                    scl_s  <= '0';
	                    sda_s  <= shift_reg(bit_count);
	                elsif i2c_phase = 2 then					  
	                    scl_s  <= '1';						  
	                elsif i2c_phase = 3 then					  
	                    if bit_count = 0 then					  
	                	bit_count <= WIDTH; 					  
	                	state 	  <= ACK_DATA;					  
	                    else							  
	                	bit_count <= bit_count - 1;				  
	                    end if;							  
	                end if; 							  
	                    						        	      
	                when READ_DATA =>				        	      	                    						        	      
	                if i2c_phase = 0 then						  
	                    scl_s  <= '0';			 
	                    sda_s  <= '1';  -- release SDA to read data 					 
	                elsif i2c_phase = 2 then					  
	                    scl_s		 <= '1';					  
	                    shift_reg(bit_count) <= sda;  -- read data bit		  
	                elsif i2c_phase = 3 then					  
	                    if bit_count = 0 then					  
	                	bit_count <= WIDTH; 					  
	                	rdata	  <= shift_reg;  -- store read data		  
	                	valid	  <= '1';					  
	                	state	  <= ACK_DATA;  					
	                    else							  
	                	bit_count <= bit_count - 1;				  
	                    end if;							  
	                end if;
			
			when ACK_DATA =>			
			if i2c_phase = 0 then
			    if mode = '0' then
				-- for read operations, master sends ACK/NACK
				if stop = '1' then
				    sda_s <= '1';  -- NACK to indicate end of read
				else
				    sda_s <= '0';  -- ACK to continue reading
				end if;
			    else
				-- For write operations, release SDA to read slave's ACK
				sda_s <= '1';
			    end if;
			    scl_s <= '0';
			elsif i2c_phase = 2 then
			    scl_s <= '1';
			    if mode = '0' then
				ack_s <= not sda;  -- slave ACK is low
			    end if;
			elsif i2c_phase = 3 then			    
			    if stop = '1' then
				state <= QUIT;
			    else
				if mode = '0' then
				    state     <= READ_DATA;   -- continue reading
				else
				    shift_reg <= wdata;     -- load next data to write
				    state     <= WRITE_DATA;  -- write next byte
				end if;
			    end if;
			end if;
			
			when QUIT =>			    
			if i2c_phase = 0 then
			    scl_s  <= '0';  -- prepare for STOP condition
			    sda_s  <= '0';
			elsif i2c_phase = 1 then
			    scl_s  <= '1';  -- SCL high before SDA
			elsif i2c_phase = 2 then
			    scl_s  <= '1';  
			    sda_s  <= '1';  -- stop condition: SDA rises while SCL is high
			elsif i2c_phase = 3 then
			    state  <= IDLE;
			end if; 					  
			
               	end case;
               
       end if;
  
  end process i2c_fsm_p;
    
  -- Output assignments
  busy <= busy_s;
  
  -- I2C bus control
  scl  <= '0' when scl_s = '0' else '1';  -- Open-drain outputs
  sda  <= '0' when sda_s = '0' else '1';  -- Open-drain outputs

end architecture rtl;
