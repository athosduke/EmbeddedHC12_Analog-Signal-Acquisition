;*******************************************************
;* CMPEN 472, HW10 sample 3 program,  converting analog 
;*            signal to digital data, MC9S12C128 Program
;*
;* Date: 11/04/2016     Kyusun Choi
;* Date: 11/07/2016     Kyusun Choi
;* Date: 11/02/2018     Kyusun Choi
;*
;*   non-interrupt, busy wait method
;*   serial communication at 9600 baud only
;*
;* This program prints what user types on a terminal 
;* window - ie. a typewriter program.  However, when a user hits an 
;* 'enter' key, a hexadecimal number will be printed on the terminal. 
;* The hexadecimal number represents the analog voltage level of the
;* ADC input channel 7 (pin number 10 of the CSM-12C128 board Connector J1).
;* The program does single AD conversion in 10bit but only the upper
;* 8bit is used for the conversion to the hexadecimal number.
;*
;* Freescale CodeWarrior,  for the MC9S12C128 Program
;* Target: Axiom's CSM-12C128 Board, ASCII Monitor Mode
;*******************************************************
; export symbols
            XDEF Entry            ; export 'Entry' symbol
            ABSENTRY Entry        ; for assembly entry point
  
; symbols/addresses
ATDCTL2     EQU  $0082
ATDCTL3     EQU  $0083
ATDCTL4     EQU  $0084
ATDCTL5     EQU  $0085
ATDSTAT0    EQU  $0086
ATDDR0H     EQU  $0090
ATDDR0L     EQU  $0091
ATDDR7H     EQU  $009e
ATDDR7L     EQU  $009f

PTIP        EQU         $0259        ; PORT P input register
DDRP        EQU         $025A        ; PORT P data direction register
PERP        EQU         $025C        ; PORT P pull device enable register
PPSP        EQU         $025D        ; PORT P polarity select register

SCIBDH      EQU         $00c8        ; Serial port (SCI) Baud Rate Register H
SCISR1      EQU         $00cc        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00cf        ; Serial port (SCI) Data Register

TIOS        EQU         $0040        ; Timer Input Capture (IC) or Output Compare (OC) select
TIE         EQU         $004C        ; Timer interrupt enable register
TCNTH       EQU         $0044        ; Timer free runing main counter
TSCR1       EQU         $0046        ; Timer system control 1
TSCR2       EQU         $004D        ; Timer system control 2
TFLG1       EQU         $004E        ; Timer interrupt flag 1
TC2H        EQU         $0054        ; Timer channel 2 register

CR          equ  $0d              ; carriage return, ASCII 'Return' key
LF          equ  $0a              ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG  $3000            ; RAMStart defined as $3000
ATDdone     DS.B  1               ; ADC finish indicator, 1 = ATD finished
ADCsaved    DS.B  1
bytecount   DS.w  1

; following escape sequence works on Hyper Terminal (but NOT the TestTerminal)
ResetTerm       DC.B    $1B, 'c', $00                 ; reset terminal to default setting
ClearScreen     DC.B    $1B, '[2J', $00               ; clear the Hyper Terminal screen
CursorToTop     DC.B    $1B, '[2',  $3B, '1f',  $00   ; move cursor to 2,1 position
;*******************************************************
; interrupt vector section

            ORG     $3FEA            ; Timer channel 2 interrupt vector setup
            DC.W    oc2isr

;*******************************************************
; code section
            ORG  $3100
Entry
            LDS   #Entry           ; initialize the stack pointer
            
; ATD initialization
            ; ATD initialization
            LDAA  #%11000000       ; Turn ON ADC, clear flags, Disable ATD interrupt
            STAA  ATDCTL2
            LDAA  #%00001000       ; Single conversion per sequence, no FIFO
            STAA  ATDCTL3
            LDAA  #%01000111       ; 10bit, ADCLK=24MHz/16=1.5MHz, sampling time=8*(1/ADCLK)
            STAA  ATDCTL4
            ; PORTP initialization
            LDAA    #%00000011
            STAA    PERP             ; enable the pull up/down feature at PORTP bit 0 and 1
            BCLR    PPSP,%00000011   ; select pull up feature at PORTP bit 0 and 1 for the
                                     ; Push Button Switch 1 and 2.
            BCLR    DDRP,%00000011   ; Push Button Switch 1 and 2 at PORTP bit 0 and 1
                                     ; set PORTP bit 0 and 1 as input
            ldx     #ResetTerm       ; reset Hyper Terminal to default values
            jsr     printmsg
            ldx     #ClearScreen     ; clear the Hyper Terminal Screen first
            jsr     printmsg
            ldx     #CursorToTop     ; and move the cursor to top left corner (2,1) to start
            jsr     printmsg
            
            ldx     #msg1
            jsr     printmsg
            jsr     nextline
            ldx     #msg2
            jsr     printmsg
            
            LDX     #$000D
            stx     SCIBDH     
           
startloop   jsr     getchar
            cmpa    #CR
            bne     startloop
            
                    
            ldx     #ClearScreen     ; clear the Hyper Terminal Screen first
            jsr     printmsg
            ldx     #CursorToTop     ; and move the cursor to top left corner (2,1) to start
            jsr     printmsg
            
            ; ATD initialization
            LDAA  #%11000000       ; Turn ON ADC, clear flags, Disable ATD interrupt
            STAA  ATDCTL2
            LDAA  #%00001000       ; Single conversion per sequence, no FIFO
            STAA  ATDCTL3
            LDAA  #%01000111       ; 10bit, ADCLK=24MHz/16=1.5MHz, sampling time=8*(1/ADCLK)
            STAA  ATDCTL4
            ; PORTP initialization
            LDAA    #%00000011
            STAA    PERP             ; enable the pull up/down feature at PORTP bit 0 and 1
            BCLR    PPSP,%00000011   ; select pull up feature at PORTP bit 0 and 1 for the
                                     ; Push Button Switch 1 and 2.
            BCLR    DDRP,%00000011   ; Push Button Switch 1 and 2 at PORTP bit 0 and 1
                                     ; set PORTP bit 0 and 1 as input

            ldx   #msg3
            jsr   printmsg
            jsr   nextline
            ldx   #msg4            ; print Well>
            jsr   printmsg
            
loop        jsr   getchar
            cmpa  #CR
            bne   loop
            
            jsr   go2ADC

loop2       jsr   getchar
            cmpa  #CR
            beq   loop2enter
            cmpa  #$61           ;'a'
            bne   loop2
            
            jsr   nextline
            ldx   #msg5
            jsr   printmsg
            jsr   nextline            
            ldx   #msg6
            jsr   printmsg
            jsr   nextline
            ldx   #msg7
            jsr   printmsg
            jsr   nextline
            ldx   #msg8
            jsr   printmsg
            jsr   nextline
            bra   loop2sw
            
loop2enter  jsr   go2ADC
            bra   loop2

            
loop2sw     ldaa  PTIP                  ;wait for sw1 pushed
            anda  #%00000001
            bne   loop2sw
            
            jsr   StartTimer2oc
            
loop1024    jsr   updateADC
            ldx   bytecount
            cpx   #1030
            beq   loop2swinit
            ldaa  ADCsaved
            jsr   putchar
            bra   loop1024

loop2swinit SEI
            bra loop2sw

;*******************************************************
; subroutine section


;***********Timer OC2 interrupt service routine***************
oc2isr
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC2H               ;    for next interrupt
            std   TC2H               ;    + Fast clear timer CH2 interrupt flag
            bset  TFLG1,%00000100    ; clear timer CH2 interrupt flag, not needed if fast clear enabled
            ldy   bytecount
            iny
            sty   bytecount
oc2done     RTI
;***********end of Timer OC2 interrupt service routine********

;***************StartTimer2oc************************
;* Program: Start the timer interrupt, timer channel 2 output compare
;* Input:   Constants - channel 2 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;**********************************************
StartTimer2oc
            PSHD
            LDAA   #%00000100
            STAA   TIOS              ; set CH2 Output Compare
            STAA   TIE               ; set CH2 interrupt Enable
            LDAA   #%10010000        ; enable timer and set Fast Flag Clear
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD     #3000            ; 125usec with (24MHz/1 clock)
            ADDD    TCNTH            ;    for first interrupt
            STD     TC2H             ;    + Fast clear timer CH2 interrupt flag
            
            ldx    #0
            stx    bytecount

            PULD
            BSET   TFLG1,%00000100   ; initial Timer CH2 interrupt flag Clear, not needed if fast clear set
            CLI                      ; enable interrupt
            RTS
;***************end of StartTimer2oc*****************
;***********updateADC*********************
; This is a sample, non-interrupt, busy wait method
;
updateADC
            PSHA
            pshx                   ; Start ATD conversion
            
            LDAA  #%00000111       ; left justified, unsigned, single conversion,
            STAA  ATDCTL5          ; single channel, CHANNEL 7, start the conversion

            ldx   bytecount
waitINT     cpx   bytecount
            beq   waitINT

            ldaa  ATDDR0H          ; pick up the upper 8bit result
            staa  ADCsaved
            
            
            pulx
            PULA
            RTS
;***********end of AD conversiton**************            


;***********single AD conversiton*********************
; This is a sample, non-interrupt, busy wait method
;
go2ADC
            PSHA                   ; Start ATD conversion
            LDAA  #%00000111       ; left justified, unsigned, single conversion,
            STAA  ATDCTL5          ; single channel, CHANNEL 7, start the conversion

adcwait     ldaa  ATDSTAT0         ; Wait until ATD conversion finish
            anda  #%10000000       ; check SCF bit, wait for ATD conversion to finish
            beq   adcwait

            ldaa  #'$'             ; print the ATD result, in hex
            jsr   putchar

            ldaa  ATDDR0H          ; pick up the upper 8bit result
            staa  ADCsaved
            jsr   printHx          ; print the ATD result
            jsr   nextline
            ldx   #msg4
            jsr   printmsg

            PULA
            RTS
;***********end of AD conversiton**************            

;***********printHx***************************
; prinHx: print the content of accumulator A in Hex on SCI port
printHx     psha
            lsra
            lsra
            lsra
            lsra
            cmpa   #$09
            bhi    alpha1
            adda   #$30
            jsr    putchar
            bra    low4bits
alpha1      adda   #$37
            jsr    putchar            
low4bits    pula
            anda   #$0f
            cmpa   #$09
            bhi    alpha2
            adda   #$30
            jsr    putchar
            rts
alpha2      adda   #$37
            jsr    putchar
            rts
;***********end of printhx***************************
 
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* C
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                jsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                       ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************

*
* Add any subroutines here
*
msg1        DC.B  'Baud rate changed! Please reopen the HyperTerminal with 115.2Kbaud', $00
msg2        DC.B  'Then press enter to continue  ', $00
msg3        DC.B  'Please connect the audio cable to HCS12 board, then press Enter', $00
msg4        DC.B  'Well>', $00
msg5        DC.B  'Please disconnect the HyperTerminal', $00
msg6        DC.B  'Start NCH Tone Generator program', $00
msg7        DC.B  'Start SB Data Receive program', $00
msg8        DC.B  'Then press the switch SW1, for 1024 point analog to digital conversions', $00




            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
