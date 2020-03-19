# EmbeddedHC12_Analog-Signal-Acquisition
The program steps are as follows:

Initialize\
Your program starts from ASCII monitor command 'go 3100'\
Your program prints the message on the HyperTerminal 'Baud rate changed! Please reopen the HyperTerminal with 115.2Kbaud'\
Change the SCI port baud rate from 9600 to 115.2Kbaud.\
Wait for the first RETURN key hit on the SCI port\
Print the message 'Please connect the audio cable to HCS12 board' on the new HyperTerminal (115.2kbaud) if the RETURN key was hit\
Non-RETURN key hits are ignored\
Also print the 'Well>' prompt on a new line\
Wait for the RETURN key hit again on the SCI port\
Non-RETURN key hits are ignored\
Start a single Analog-to-Digital Conversion (ADC) of the signal on the AN7 pin and display the 8 bit result on the HyperTerminal if the RETURN key was hit.\
You may check the result at this time: if the audio cable is correctly connected, the result display is between 123($7B) to 133($85). If the audio cable is not connected or incorrectly connected, the result display is between 0($00) to 32($20).\
Also print the 'Well>' prompt on a new line\
Wait for the RETURN key hit again on the SCI port\
Non-RETURN key hits are ignored\
Start a single Analog-to-Digital conversion of the signal on the AN7 pin and display the 8 bit result on the HyperTerminal if the RETURN key was hit.\
Also print the 'Well>' prompt on a new line\
The subsequent RETURN key hits, the last two steps are repeated.\
If the RETURN key hit (then the ADC result display) was followed by the 'a' key hit, Print the following messages:

'Please disconnect the HyperTerminal'\
'Start NCH Tone Generator program'\
'Start SB Data Receive program'\
'Then press the switch SW1, for 1024 point analog to digital conversions'

Wait for the Switch SW1 pressing\
Start the Timer Module Channel 2 Output Compare interrupt generation at every 125usec (8KHz rate). Each time the Output Compare interrupt occurs, carry out the following tasks:

Service the Output Compare (OC) register (update the counter compare number) for the next interrupt. Also clear the OC interrupt flag (if you selected the Fast Flag Clear option, updating the timer OC register for the next interrupt will also clear the interrupt flag).\
Pick up the ADC result (from previous conversion) and transmit it to SCI port. Only the upper 8bit of the ADC result should be picked up and 1 byte of pure binary number should be transmitted (do NOT convert to ASCII).\
Increment the transmit byte counter

Start a single Analog-to-Digital conversion of the signal on the AN7 pin\
Wait for the transmit byte count to be 1030\
Then repeat the last 3 steps, begining with Switch SW1 press waiting.
