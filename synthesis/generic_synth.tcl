#####################################################################################
#                            generic_shift_reg
#####################################################################################
#  Clock definition
#create_clock -name clock -period 2.5 [get_ports clock]
# Clock uncertainty and transition 
#set_clock_uncertainty -setup 0.05 [get_clocks clock] 
#set_clock_uncertainty -hold 0.02 [get_clocks clock] 
#set_clock_transition 0.05 [get_clocks clock] 
# Maximum setup (32%) and minimum hold (20%) time

# Input delays 
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports load] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports load]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports enable] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports enable]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports direction] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports direction]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports serial_in] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports serial_in]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports parallel_in] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports parallel_in]

# Output delays 
#set_output_delay -clock clock -max 0.8 -network_latency_included [get_ports serial_out] 
#set_output_delay -clock clock -min 0.5 -network_latency_included [get_ports serial_out] 
#set_output_delay -clock clock -max 0.8 -network_latency_included [get_ports parallel_out] 
#set_output_delay -clock clock -min 0.5 -network_latency_included [get_ports parallel_out] 

# Asynchronous reset
#set_ideal_network -no_propagate [get_ports {reset_n}]

#####################################################################################
#                            generic_updown_counter
#####################################################################################
# Clock definition 
#create_clock -name clock -period 2.5 [get_ports clock]
# Clock uncertainty and transition 
#set_clock_uncertainty -setup 0.05 [get_clocks clock] 
#set_clock_uncertainty -hold 0.02 [get_clocks clock] 
#set_clock_transition 0.05 [get_clocks clock] 
# Maximum setup (32%) and minimum hold (20%) time

# Input delays
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports enable] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports enable]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports updown] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports updown]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports sync_load] 
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports sync_load]
#set_input_delay -clock clock -max 0.8 -network_latency_included [get_ports load_val]
#set_input_delay -clock clock -min 0.5 -network_latency_included [get_ports load_val]

# Output delays
#set_output_delay -clock clock -max 0.8 -network_latency_included [get_ports out_val] 
#set_output_delay -clock clock -min 0.5 -network_latency_included [get_ports out_val] 
#set_output_delay -clock clock -max 0.8 -network_latency_included [get_ports max_val]
#set_output_delay -clock clock -min 0.5 -network_latency_included [get_ports max_val]
#set_output_delay -clock clock -max 0.8 -network_latency_included [get_ports min_val] 
#set_output_delay -clock clock -min 0.5 -network_latency_included [get_ports min_val] 
#set_output_delay -clock clock -max 0.8 -network_latency_included [get_ports overflow]
#set_output_delay -clock clock -min 0.5 -network_latency_included [get_ports overflow]

#####################################################################################
#                            generic_adder
#####################################################################################
# Clock definition
#create_clock -name virtual_clock -period 5.0 [get_ports input0]

# Input delays
#set_input_delay -clock virtual_clock -max 0.8 -network_latency_included [get_ports input0]
#set_input_delay -clock virtual_clock -min 0.5 -network_latency_included [get_ports input0]
#set_input_delay -clock virtual_clock -max 0.8 -network_latency_included [get_ports input1]
#set_input_delay -clock virtual_clock -min 0.5 -network_latency_included [get_ports input1]

# Output delays
#set_output_delay -clock virtual_clock -max 0.8 -network_latency_included [get_ports carry_o]
#set_output_delay -clock virtual_clock -min 0.5 -network_latency_included [get_ports carry_o]
#set_output_delay -clock virtual_clock -max 0.8 -network_latency_included [get_ports sum_o]
#set_output_delay -clock virtual_clock -min 0.5 -network_latency_included [get_ports sum_o]
