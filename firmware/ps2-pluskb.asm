;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  PS/2 to Macintosh Plus Serial Keyboard Protocol Converter
;;;
;


;;; Connections ;;;

;;;                                                                   ;;;
;                                    .--------.                         ;
;                            Supply -|01 \/ 08|- Ground                 ;
;     Serial Keyboard Data <--> RA5 -|02    07|- RA0 <--- PS/2 Data     ;
;    Serial Keyboard Clock <--- RA4 -|03    06|- RA1 <--- PS/2 Clock    ;
;                    !MCLR ---> RA3 -|04    05|- RA2 <---               ;
;                                    '--------'                         ;
;;;                                                                   ;;;


;;; Assembler Directives ;;;

	list		P=PIC12F1501, F=INHX32, ST=OFF, MM=OFF, R=DEC, X=ON
	#include	P12F1501.inc
	errorlevel	-302	;Suppress "register not in bank 0" messages
	errorlevel	-224	;Suppress TRIS instruction not recommended msgs
	__config	_CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_ON & _MCLRE_ON & _CP_OFF & _BOREN_OFF & _CLKOUTEN_OFF
			;_FOSC_INTOSC	Internal oscillator, I/O on RA5
			;_WDTE_OFF	Watchdog timer disabled
			;_PWRTE_ON	Keep in reset for 64 ms on start
			;_MCLRE_ON	RA3/!MCLR is !MCLR
			;_CP_OFF	Code protection off
			;_BOREN_OFF	Brownout reset off
			;_CLKOUTEN_OFF	CLKOUT disabled, I/O on RA4
	__config	_CONFIG2, _WRT_OFF & _STVREN_ON & _BORV_LO & _LPBOR_OFF &_LVP_OFF
			;_WRT_OFF	Write protection off
			;_STVREN_ON	Stack over/underflow causes reset
			;_BORV_LO	Brownout reset voltage low trip point
			;_LPBOR_OFF	Low power brownout reset disabled
			;_LVP_OFF	High-voltage on Vpp to program


;;; Macros ;;;

DELAY	macro	value		;Delay 3*W cycles, set W to 0
	movlw	value
	decfsz	WREG,F
	bra	$-1
	endm

DNOP	macro
	bra	$+1
	endm


;;; Constants ;;;

SKMODEL	equ	0x0B	;Serial keyboard model number
SKNULL	equ	0x7B	;Serial keyboard null byte
SKKPAD	equ	0x79	;Serial keyboard keypad key sentinel
SKPASS	equ	0x7D	;Serial keyboard self-test passed byte
SKSHIFT	equ	0x71	;Serial keyboard shift key code
SKCMD	equ	0x6F	;Serial keyboard command key code
SKOPT	equ	0x75	;Serial keyboard option key code
SKCAPS	equ	0x73	;Serial keyboard caps lock key code

;Pin Assignments:
PKD_PIN	equ	RA0	;Pin where PS/2 keyboard data is connected
PKC_PIN	equ	RA1	;Pin where PS/2 keyboard clock is connected
SKC_PIN	equ	RA4	;Pin where serial keyboard clock is connected
SKD_PIN	equ	RA5	;Pin where serial keyboard data is connected

;FLAGS:
BFINUSE	equ	7	;Set if serial keyboard queue is in use by mainline
PSKBDF0	equ	6	;Set if the PS/2 keyboard sent an 0xF0
PSKBDE0	equ	5	;Set if the PS/2 keyboard sent an 0xE0
PSKBDE1	equ	4	;Set if the PS/2 keyboard sent an 0xE1
PSKIGNX	equ	3	;Set if the PS/2 keyboard should ignore next keystroke
PRGMCHG	equ	2	;Set if the program for a programmable key has changed

;SK_MODS:
MOD_SHF	equ	7	;Set if the shift key is down as of end of queue
MOD_CMD	equ	6	;Set if the command key is down as of end of queue
MOD_OPT	equ	5	;Set if the option key is down as of end of queue
MOD_CPL	equ	4	;Set if the caps lock key is down as of end of queue


;;; Variable Storage ;;;

	cblock	0x70	;Bank-common registers
	
	FLAGS	;You've got to have flags
	SK_FSAP	;Pointer to where to resume serial keyboard state machine
	SK_SR	;Serial keyboard shift register
	SK_QPSH	;Serial keyboard queue push pointer
	SK_QPOP	;Serial keyboard queue pop pointer
	SK_MODS	;Serial keyboard modifier state
	PK_FSAP	;Pointer to where to resume PS/2 keyboard state machine
	PK_SR	;PS/2 keyboard shift register
	PK_QPSH	;PS/2 keyboard queue push pointer
	PK_QPOP	;PS/2 keyboard queue pop pointer
	PK_LAST	;Last key code sent from PS/2 keyboard
	PK_PROG	;Code of programmable button being held down (if any)
	X3
	X2
	X1
	X0
	
	endc

	;Linear memory:
	;0x2000-0x201F - Serial keyboard queue or space for modifying flash
	;0x2020-0x202F - PS/2 keyboard queue


;;; Vectors ;;;

	org	0x0		;Reset vector
	goto	Init

	org	0x4		;Interrupt vector
	goto	Interrupt


;;; Hardware Initialization ;;;

Init
	banksel	OSCCON		;16 MHz high-freq internal oscillator
	movlw	B'01111000'
	movwf	OSCCON

	banksel	IOCAN		;Serial keyboard data and PS/2 keyboard clock
	movlw	(1 << SKD_PIN) | (1 << PKC_PIN)	; set IOCAF on negative edges
	movwf	IOCAN

	banksel	OPTION_REG	;Timer0 uses instruction clock, 1:16 prescaler,
	movlw	B'01010011'	; thus ticking every 4 us; weak pull-ups on
	movwf	OPTION_REG

	banksel	T2CON		;Timer2 has 1:16 prescaler and 1:4 postscaler,
	movlw	B'00011110'	; thus overflowing after 4.096 ms
	movwf	T2CON

	banksel	CLC2CON		;CLC2 is a DFF which clocks in data from the
	clrf	CLC2SEL0	; PS/2 data pin (CLC2IN1) on the falling edge of
	movlw	B'01010000'	; the PS/2 clock pin (CLC2IN0)
	movwf	CLC2SEL1
	movlw	B'00000001'
	movwf	CLC2POL
	movlw	B'00000010'
	movwf	CLC2GLS0
	movlw	B'10000000'
	movwf	CLC2GLS1
	clrf	CLC2GLS2
	clrf	CLC2GLS3
	movlw	B'10000100'
	movwf	CLC2CON

	banksel	ANSELA		;All pins digital, not analog
	clrf	ANSELA

	banksel	LATA		;Default state of output pins is low, serial
	clrf	LATA		; keyboard pins ready to be pulled low

	banksel	TRISA		;PS/2 pins inputs, serial keyboard pins open-
	movlw	B'00111111'	; collector outputs which are currently off
	movwf	TRISA

	banksel	PMCON1		;Point high flash memory control address reg to
	movlw	high KeyLut	; key translation lookup table
	movwf	PMADRH

	banksel	PIE1		;Timer2 interrupt enabled
	movlw	1 << TMR2IE
	movwf	PIE1

	movlb	0		;Initialize key globals
	movlp	high SKFsaIdle
	movlw	0x20
	movwf	FSR0H
	movwf	FSR1H
	movwf	PK_QPSH
	movwf	PK_QPOP
	clrf	SK_QPSH
	clrf	SK_QPOP
	clrf	FLAGS
	clrf	SK_MODS
	movlw	low SKFsaIdle
	movwf	SK_FSAP
	movlw	low PKFsaStart
	movwf	PK_FSAP
	clrf	PK_PROG

	movlw	B'11001000'	;On-change interrupt, peripheral interrupts (for
	movwf	INTCON		; Timer2) and interrupt subsystem on

	;fall through


;;; Mainline ;;;

Ps2Reset
	movlw	~((1 << PSKBDF0)|(1 << PSKBDE0)|(1 << PSKBDE1)|(1 << PSKIGNX))
	andwf	FLAGS,F		;Reset flags relevant to PS/2 keyboard
	;fall through

Main
	call	PKPop		;Try to pop off the PS/2 keyboard queue
	btfsc	STATUS,Z	;If queue is empty, loop
	bra	Main		; "
	movf	WREG,W		;If the PS/2 receiver timed out, reset the flags
	btfsc	STATUS,Z	; "
	bra	Ps2Reset	; "
	xorlw	0xAA		;If the received byte was 0xAA, send a byte to
	btfsc	STATUS,Z	; the keyboard because some stupid ones require
	goto	Ps2Init		; this
	xorlw	0x83 ^ 0xAA	;If the received byte was 0x83 (F7), act like it
	btfsc	STATUS,Z	; was 0x7F so we can halve the size of the LUT
	movlw	0x7F ^ 0x83	; "
	xorlw	0xF0 ^ 0x83	;If the received byte was 0xF0, set that flag
	btfsc	STATUS,Z	; "
	bsf	FLAGS,PSKBDF0	; "
	xorlw	0xE0 ^ 0xF0	;If the received byte was 0xE0, set that flag
	btfsc	STATUS,Z	; "
	bsf	FLAGS,PSKBDE0	; "
	xorlw	0xE1 ^ 0xE0	;If the received byte was 0xE1, set that flag
	btfsc	STATUS,Z	; "
	bsf	FLAGS,PSKBDE1	; "
	xorlw	0x00 ^ 0xE1	;If the received byte's MSb was set (except if
	btfsc	WREG,7		; it was 0x83 for F7), do nothing else and
	bra	Main		; wait for the next byte
	btfsc	FLAGS,PSKBDE0	;Move the state of the 0xE0 flag into the MSb
	iorlw	B'10000000'	; before doing the lookup
	bcf	FLAGS,PSKBDE0	; "
	btfsc	FLAGS,PSKIGNX	;If we were flagged to ignore the next event,
	bra	Ps2Reset	; reset flags instead of continuing
	btfsc	FLAGS,PSKBDE1	;If the 0xE1 flag was set, change lookup index
	movlw	0xFF		; to 0xFF for Pause key and raise the flag to
	btfsc	FLAGS,PSKBDE1	; ignore the Num Lock press that will inevitably
	bsf	FLAGS,PSKIGNX	; follow
	bcf	FLAGS,PSKBDE1	; "
	movlb	3		;Save this byte as the lookup index
	movwf	PMADRL		; "
	bcf	INTCON,GIE	;Look up the corresponding serial keyboard key
	bsf	PMCON1,RD	; code and its flags in the LUT
	nop			; "
	nop			; "
	bsf	INTCON,GIE	; "
	bcf	PMDATL,7	;Move the state of the 0xF0 flag into the MSb
	btfsc	FLAGS,PSKBDF0	; of the looked-up key code
	bsf	PMDATL,7	; "
	bcf	FLAGS,PSKBDF0	; "
	movf	PMDATL,W	;If the key code is the same as last time, this
	xorwf	PK_LAST,W	; is a typematic repeat event from the PS/2
	btfsc	STATUS,Z	; keyboard and we should suppress it
	bra	Main		; "
	xorwf	PK_LAST,F	;Else update the last key code
	movf	SK_QPSH,W	;Load the serial keyboard push pointer for
	movwf	FSR1L		; dereferencing
	movf	PMDATH,W	;Switch case depending on upper byte of value
	brw			; from LUT:
	bra	Main		;0x00 - No corresponding key on serial keyboard
	bra	KKeyboard	;0x01 - Key on main keyboard
	bra	KKeypad		;0x02 - Key on keypad
	bra	KKeypadShiftUp	;0x03 - Key on keypad, shift key must be up
	bra	KKeypadShiftDown;0x04 - Key on keypad, shift key must be down
	bra	KKeyboardShift	;0x05 - Key is shift key
	bra	KKeyboardCommand;0x06 - Key is command key
	bra	KKeyboardOption	;0x07 - Key is option key
	bra	KKeyboardCaps	;0x08 - Key is caps lock key
	bra	KKeyboardCmdShft;0x09 - Key on main keyboard, command + shift
	bra	KProgrammable	;0x0A - Programmable key

KKeypad
	movlw	SKKPAD		;Push the keypad sentinel onto the queue
	call	SKPush		; "
	;fall through

KKeyboard
	movf	PMDATL,W	;Push the looked-up key code onto the queue
	call	SKPush		; "
	bra	Main		;Await the next byte

KKeypadShiftUp
	btfss	SK_MODS,MOD_SHF	;If shift is already up, branch to handle this
	bra	KKeypad		; as normal for a keypad key
	movlw	SKSHIFT | 0x80	;Push the keycode for a shift key release onto
	call	SKPush		; the queue
	movlw	SKKPAD		;Push the keypad sentinel onto the queue
	call	SKPush		; "
	movf	PMDATL,W	;Push the looked-up key code onto the queue
	call	SKPush		; "
	movlw	SKSHIFT		;Push the keycode for a shift key press onto
	call	SKPush		; the queue
	bra	Main		;Await the next byte

KKeypadShiftDown
	btfsc	SK_MODS,MOD_SHF	;If shift is already down, branch to handle this
	bra	KKeypad		; as normal for a keypad key
	movlw	SKSHIFT		;Push the keycode for a shift key press onto
	call	SKPush		; the queue
	movlw	SKKPAD		;Push the keypad sentinel onto the queue
	call	SKPush		; "
	movf	PMDATL,W	;Push the looked-up key code onto the queue
	call	SKPush		; "
	movlw	SKSHIFT	| 0x80	;Push the keycode for a shift key release onto
	call	SKPush		; the queue
	bra	Main		;Await the next byte

KKeyboardShift
	bcf	SK_MODS,MOD_SHF	;Copy (and invert) the key up/down bit into the
	btfss	PMDATL,7	; shift state flag
	bsf	SK_MODS,MOD_SHF	; "
	bra	KKeyboard	;Push the key press/release onto the queue

KKeyboardCommand
	bcf	SK_MODS,MOD_CMD	;Copy (and invert) the key up/down bit into the
	btfss	PMDATL,7	; command state flag
	bsf	SK_MODS,MOD_CMD	; "
	bra	KKeyboard	;Push the key press/release onto the queue

KKeyboardOption
	bcf	SK_MODS,MOD_OPT	;Copy (and invert) the key up/down bit into the
	btfss	PMDATL,7	; option state flag
	bsf	SK_MODS,MOD_OPT	; "
	bra	KKeyboard	;Push the key press/release onto the queue

KKeyboardCaps
	movlw	1 << MOD_CPL	;If this is a key press event, invert the stored
	btfss	PMDATL,7	; state of the caps lock key
	xorwf	SK_MODS,F	; "
	movf	PMDATL,W	;If the state of the caps lock key is released
	btfss	SK_MODS,MOD_CPL	; and this is a release event, or the state of
	xorlw	B'10000000'	; the caps lock key is pressed and this is a
	btfsc	WREG,7		; press event, queue it, else ignore it
	bra	Main		; "
	bra	KKeyboard	; "

KKeyboardCmdShft
	call	SKReleaseMods2	;Release any modifier keys (except caps lock)
	movlw	SKCMD		;Push the keycode for a command key press onto
	call	SKPush		; the queue
	movlw	SKSHIFT		;Push the keycode for a shift key press onto
	call	SKPush		; the queue
	movf	PMDATL,W	;Push the looked-up key code onto the queue
	call	SKPush		; "
	movlw	SKSHIFT | 0x80	;Push the keycode for a shift key release onto
	call	SKPush		; the queue
	movlw	SKCMD | 0x80	;Push the keycode for a command key release onto
	call	SKPush		; the queue
	bra	Main		;Await the next byte

KProgrammable
	movf	PK_PROG,W	;If not already working with a programmable key,
	btfsc	STATUS,Z	; skip ahead to start
	bra	KProgr3		; "
	btfss	PMDATL,7	;If we're already working with a programmable
	bra	Main		; key and this is a press event, ignore it
	xorwf	PMDATL,W	;If we're already working with a programmable
	btfss	STATUS,Z	; key and this is a release event for another
	bra	Main		; programmable key, ignore it
	btfss	FLAGS,PRGMCHG	;If the programming for this key didn't change,
	bra	KProgr0		; skip ahead to execute its sequence
	movf	PMDATL,W	;Update the programming for this key in flash
	andlw	B'11110000'	; "
	call	WritePage	; "
KProgr0	movlw	0x00		;Select the lower or upper half of the buffer
	btfsc	PK_PROG,3	; "
	movlw	0x10		; "
	movwf	SK_QPOP		;This is where the serial keyboard queue starts
	movwf	FSR0L		;Start scanning for the end here too
KProgr1	moviw	FSR0++		;Pick up the next byte and advance the pointer
	xorlw	SKNULL		;If it's a null, this is where queue ends
	btfsc	STATUS,Z	; "
	addfsr	FSR0,-1		; "
	btfsc	STATUS,Z	; "
	bra	KProgr2		; "
	movf	FSR0L,W		;If this is the end of the half of the buffer,
	andlw	B'00001111'	; this is where queue ends
	btfss	STATUS,Z	; "
	bra	KProgr1		;Otherwise keep scanning
KProgr2	movf	FSR0L,W		;Set the serial keyboard queue push point to the
	andlw	B'00011111'	; end of the data loaded for this programmed
	movwf	SK_QPSH		; sequence
	clrf	PK_PROG		;No longer working with a programmable key
	bcf	FLAGS,PRGMCHG	;Clear the program changed flag
	bcf	FLAGS,BFINUSE	;Let serial keyboard interrupt read the queue
	bra	Main		;Done
KProgr3	call	SKReleaseMods	;Start by releasing mods, flushing queue
	bsf	FLAGS,BFINUSE	;Claim the buffer so interrupt doesn't read it
	movf	PMDATL,W	;Flag that we're working with this key
	iorlw	B'10000000'	; "
	movwf	PK_PROG		; "
	andlw	B'11110000'	;Load the flash page containing it and the
	call	ReadPage	; adjacent key's sequence
	movlw	0x00		;Select the lower or upper half of the buffer
	btfsc	PK_PROG,3	; to write to
	movlw	0x10		; "
	movwf	SK_QPSH		; "
	bcf	FLAGS,PRGMCHG	;Key programming hasn't changed yet
	bra	Main		;Done


;;; State Machines ;;;

	org	0x100

SKFsaIdle
	movlw	-55		;The Mac has pulled the data line low, set a
	movwf	TMR0		; timer to wait 220us before we pull the clock
	bcf	INTCON,TMR0IF	; line low and start clocking a byte out of the
	bsf	INTCON,TMR0IE	; Mac
	movlb	7		;Disable the IOC interrupt on the data pin
	bcf	IOCAN,SKD_PIN	; "
	bcf	IOCAF,SKD_PIN	; "
	retlw	low SKFsaStPull	;Return in 220us to pull the clock low

SKFsaStPull
	movlw	-45		;Wait 180us between pulling the clock line low
	movwf	TMR0		; and releasing it
	bcf	INTCON,TMR0IF	; "
	movlb	1		;Pull the clock line low to signal Mac to put a
	bcf	TRISA,SKC_PIN	; data bit on the line
	retlw	low SKFsaStRel	;Return in 180us to release the clock line

SKFsaStRel
	movlw	-20		;Wait 80us between releasing the clock line and
	movwf	TMR0		; sampling the data line for the first time
	bcf	INTCON,TMR0IF	; "
	movlb	1		;Release the clock line
	bsf	TRISA,SKC_PIN	; "
	retlw	low SKFsaStSamp	;Return in 80us to sample the data line

SKFsaStSamp
	movlw	-35		;Wait 140us between sampling the data line and
	movwf	TMR0		; pulling the clock line low
	bcf	INTCON,TMR0IF	; "
	movlw	0x02		;Initialize the shift register with a sentinel
	movwf	SK_SR		; bit and the current state of the data line
	btfsc	PORTA,SKD_PIN	; "
	bsf	SK_SR,0		; "
	retlw	low SKFsaMacPull;Return in 140us to pull the clock line low

SKFsaMacPull
	movlw	-45		;Wait 180us between pulling the clock line low
	movwf	TMR0		; and releasing it
	bcf	INTCON,TMR0IF	; "
	movlb	1		;Pull the clock line low to signal Mac to put a
	bcf	TRISA,SKC_PIN	; data bit on the line
	retlw	low SKFsaMacRel	;Return in 180us to release the clock line

SKFsaMacRel
	movlw	-20		;Wait 80us between releasing the clock line and
	movwf	TMR0		; sampling the data line
	bcf	INTCON,TMR0IF	; "
	movlb	1		;Release the clock line
	bsf	TRISA,SKC_PIN	; "
	retlw	low SKFsaMacSamp;Return in 80us to sample the data line

SKFsaMacSamp
	movlw	-35		;Wait 140us between sampling the data line and
	movwf	TMR0		; pulling the clock line low
	bcf	INTCON,TMR0IF	; "
	bcf	STATUS,C	;Rotate the current state of the data line into
	btfsc	PORTA,SKD_PIN	; the shift register
	bsf	STATUS,C	; "
	rlf	SK_SR,F		; "
	btfss	STATUS,C	;If we haven't yet filled the shift register,
	retlw	low SKFsaMacPull; circle back for another clock cycle
	bcf	INTCON,TMR0IE	;Stop the timer interrupt and instead wait for
	movlb	7		; Mac to release the data line, signalling that
	bsf	IOCAP,SKD_PIN	; it's ready for us to reply
	retlw	low SKFsaMacRdy	; "

SKFsaMacRdy
	movlw	-45		;The Mac has released the data line, so set a
	movwf	TMR0		; timer to wait 180us before we put a data bit
	bcf	INTCON,TMR0IF	; on the line
	bsf	INTCON,TMR0IE	; "
	movlb	7		;Disable the IOC interrupt on the data pin
	bcf	IOCAP,SKD_PIN	; "
	bcf	IOCAF,SKD_PIN	; "
	btfsc	SK_SR,5		;Command 0x36 = Test (return SKPASS)
	bra	SKF_Tst		; "
	btfsc	SK_SR,1		;Command 0x16 = Model Number (return SKMODEL)
	bra	SKF_Mod		; "
	btfsc	SK_SR,2		;Command 0x14 = Instant
	bra	SKF_Ins		; "
	btfsc	SK_SR,4		;Command 0x10 = Inquiry
	bra	SKF_Inq		; "
	bsf	IOCAN,SKD_PIN	;If it wasn't any of the above, await Mac
	retlw	low SKFsaIdle	; pulling the data line low and starting anew
SKF_Tst	movlw	SKPASS		;Set up to transmit SKPASS on the bus to
	movwf	SK_SR		; indicatethat we've passed our self-test,
	retlw	low SKFsaKbdStrt; whatever that is
SKF_Mod	movlw	SKMODEL		;Set up to transmit SKMODEL on the bus
	movwf	SK_SR		; "
	retlw	low SKFsaKbdStrt; "
SKF_Ins	movf	SK_QPSH,W	;If the queue is not empty and not in use by
	btfsc	FLAGS,BFINUSE	; mainline, pop the first byte off and set up to
	movf	SK_QPOP,W	; transmit it
	xorwf	SK_QPOP,W	; "
	btfss	STATUS,Z	; "
	bra	SKF_Pop		; "
SKF_Nul	movlw	SKNULL		;If the queue is empty, since this is an
	movwf	SK_SR		; 'instant' command, set up to reply instantly
	retlw	low SKFsaKbdStrt; with SKNULL
SKF_Pop	movf	SK_QPOP,W	;Pop the first byte off the queue and set it up
	movwf	FSR0L		; to be transmitted
	movf	INDF0,W		; "
	movwf	SK_SR		; "
	incf	SK_QPOP,F	; "
	bcf	SK_QPOP,5	; "
	retlw	low SKFsaKbdStrt; "
SKF_Inq	movf	SK_QPSH,W	;If the queue is not empty and not in use by
	btfsc	FLAGS,BFINUSE	; mainline, pop the first byte off and set up to
	movf	SK_QPOP,W	; transmit it
	xorwf	SK_QPOP,W	; "
	btfss	STATUS,Z	; "
	bra	SKF_Pop		; "
	movlw	245		;Wait up to 0.25s (about 245 * 1024us) for the
	movwf	SK_SR		; queue to become not empty before sending null
	retlw	low SKFsaKbdWait; "

SKFsaKbdWait
	bcf	INTCON,TMR0IF	;Clear the timer interrupt but leave timer set
	movf	SK_QPSH,W	;If the queue has become not empty and not in
	btfsc	FLAGS,BFINUSE	; use by mainline, pop the first byte off and
	movf	SK_QPOP,W	; set up to transmit it
	xorwf	SK_QPOP,W	; "
	btfss	STATUS,Z	; "
	bra	SKF_Pop		; "
	decfsz	SK_SR,F		;Decrement the counter of 1024us intervals and
	retlw	low SKFsaKbdWait; if it's not yet zero, wait again for queue
	bra	SKF_Nul		;If it is zero, set up to transmit SKNULL

SKFsaKbdStrt
	movlw	-10		;Wait 40us between setting up a data bit and
	movwf	TMR0		; pulling the clock line low
	bcf	INTCON,TMR0IF	; "
	bsf	STATUS,C	;Rotate the MSb out of shift register and
	rlf	SK_SR,F		; rotate in a sentinel bit
	movlb	1		;If the MSb is a zero, pull the data line low
	btfss	STATUS,C	; "
	bcf	TRISA,SKD_PIN	; "
	retlw	low SKFsaKbdPull;Return in 40us to pull the clock line low

SKFsaKbdPull
	movlw	-40		;Wait 160us between pulling the clock line low
	movwf	TMR0		; and releasing it
	bcf	INTCON,TMR0IF	; "
	movlb	1		;Pull the clock line low
	bcf	TRISA,SKC_PIN	; "
	retlw	low SKFsaKbdRel	;Return in 160us to release the clock line

SKFsaKbdRel
	movlw	-45		;Wait 180us between releasing the clock line
	movwf	TMR0		; and setting up another data bit
	bcf	INTCON,TMR0IF	; "
	movlb	1		;Release the clock line
	bsf	TRISA,SKC_PIN	; "
	lslf	SK_SR,W		;If there are more data bits to send, then
	btfss	STATUS,Z	; return in 180us to set up another data bit
	retlw	low SKFsaKbdData; "
	bsf	TRISA,SKD_PIN	;If the last data bit (should be 1) is already
	bcf	INTCON,TMR0IE	; on the line, we've just released the the
	movlb	7		; clock line, so set up to catch Mac pulling
	bsf	IOCAN,SKD_PIN	; the data line low and return to idle
	retlw	low SKFsaIdle	; "	

SKFsaKbdData
	movlw	-10		;Wait 40us between setting up a data bit and
	movwf	TMR0		; pulling the clock line low
	bcf	INTCON,TMR0IF	; "
	lslf	SK_SR,F		;Shift the next bit out of the shift register
	movlb	1		; and reflect it on the data pin
	btfss	STATUS,C	; "
	bcf	TRISA,SKD_PIN	; "
	btfsc	STATUS,C	; "
	bsf	TRISA,SKD_PIN	; "
	retlw	low SKFsaKbdPull;Return in 40us to pull the clock line low

PKFsaStart
	btfsc	STATUS,C	;If for some reason the start bit is a 1, ignore
	retlw	low PKFsaStart	; it
	movlw	0x80		;Otherwise, initialize shift register with a
	movwf	PK_SR		; sentinel bit, ready to take data LSb first
	retlw	low PKFsaBitP	;Transition to accept first bit

PKFsaIgnore
	retlw	low PKFsaIgnore	;Utility state to ignore bus until timeout

PKFsaBitP
	rrf	PK_SR,F		;Rotate received bit (in C) into SR from left
	btfsc	STATUS,C	;If a 1 fell out of the SR, we completed a byte,
	bra	PKFBiP0		; skip ahead
	btfsc	PK_SR,7		;If not, transition to receive next bit, keeping
	retlw	low PKFsaBitN	; track of parity as we go (if we just got a 1,
	retlw	low PKFsaBitP	; swap expected parity)
PKFBiP0	btfsc	PK_SR,7		;If we completed a byte, transition to expect
	retlw	low PKFsaParN	; the appropriate parity bit
	retlw	low PKFsaParP	; "

PKFsaBitN
	rrf	PK_SR,F		;Rotate received bit (in C) into SR from left
	btfsc	STATUS,C	;If a 1 fell out of the SR, we completed a byte,
	bra	PKFBiN0		; skip ahead
	btfsc	PK_SR,7		;If not, transition to receive next bit, keeping
	retlw	low PKFsaBitP	; track of parity as we go (if we just got a 1,
	retlw	low PKFsaBitN	; swap expected parity)
PKFBiN0	btfsc	PK_SR,7		;If we completed a byte, transition to expect
	retlw	low PKFsaParP	; the appropriate parity bit
	retlw	low PKFsaParN	; "

PKFsaParP
	btfsc	STATUS,C	;If we got the expected parity bit, transition
	retlw	low PKFsaStop	; to expect the stop bit, else ignore this byte
	retlw	low PKFsaIgnore	; "

PKFsaParN
	btfss	STATUS,C	;If we got the expected parity bit, transition
	retlw	low PKFsaStop	; to expect the stop bit, else ignore this byte
	retlw	low PKFsaIgnore	; "

PKFsaStop
	btfss	STATUS,C	;Stop bit should be high; if it's not, something
	retlw	low PKFsaIgnore	; is wrong, wait for bus timeout
	movf	PK_QPSH,W	;Load the PS/2 keyboard queue push pointer for
	movwf	FSR1L		; dereferencing
	movf	PK_SR,W		;Push the received byte onto the queue
	movwf	INDF1		; "
	incf	PK_QPSH,F	;Advance the queue push pointer
	bcf	PK_QPSH,4	; "
	retlw	low PKFsaStart	;Transition to receive the next byte


;;; Interrupt Handler ;;;

Interrupt
	movlb	7		;If we got a negative edge on the PS/2 clock
	btfsc	IOCAF,PKC_PIN	; pin, handle it
	bra	IntPs2Bit	; "
	movlb	0		;If Timer2 interrupted, reset the PS/2 state
	btfsc	PIR1,TMR2IF	; machine
	bra	IntPs2Timeout	; "
	movf	SK_FSAP,W	;Else, resume the serial keyboard state machine
	callw			; "
	movwf	SK_FSAP		;On returning, save the address returned in W
	movf	FSR0L,W		;Save the updated pop pointer in case it has
	movlb	31		; changed
	movwf	FSR0L_SHAD	; "
	retfie			;Return

IntPs2Bit
	bcf	IOCAF,PKC_PIN	;Clear the interrupt
	movlb	0		;Reset Timer2 as we've received a bit
	clrf	TMR2		; "
	bcf	PIR1,TMR2IF	; "
	movlb	30		;Copy the received bit into carry
	lsrf	CLCDATA,W	; "
	lsrf	WREG,W		; "
	movf	PK_FSAP,W	;Resume the PS/2 keyboard state machine
	callw			; "
	movwf	PK_FSAP		;On returning, save the address returned in W
	retfie			; "

IntPs2Timeout
	bcf	PIR1,TMR2IF	;Clear the interrupt
	movlw	low PKFsaStart	;Reset the PS/2 keyboard state machine
	movwf	PK_FSAP		; "
	movf	PK_QPSH,W	;Load the PS/2 keyboard queue push pointer for
	movwf	FSR1L		; dereferencing
	clrf	INDF1		;Push an 0x00 to indicate a timeout
	incf	PK_QPSH,F	;Advance the queue push pointer
	bcf	PK_QPSH,4	; "
	retfie			;Return


;;; PS/2 Init Subprogram ;;;

Ps2Init
	movlb	1		;Pull the clock line low for 100 us
	bcf	TRISA,PKC_PIN	; "
	DELAY	133		; "
	bcf	TRISA,PKD_PIN	;Pull the data line low to signal we want to
	bsf	TRISA,PKC_PIN	; send a byte, release the clock line
	movlw	0xF6		;Set up 0xF6 (set defaults) to be sent
	bsf	STATUS,C	;Rotate a 1 into W and rotate out the first bit
	rrf	WREG,W		; to be sent
Ps2Ini0	call	Ps2Ini1		;Set up this bit to be sent
	lsrf	WREG,W		;Rotate the next bit into carry
	btfss	STATUS,Z	;If it wasn't the last bit of the byte, loop
	bra	Ps2Ini0		; "
	call	Ps2Ini1		;Set up a 1 (odd) parity bit to be sent
	call	Ps2Ini1		;Set up a 1 (stop) bit to be sent
	goto	Ps2Reset	;Return without waiting for ack sequence
Ps2Ini1	movlb	0		;Wait for the clock line to be released
	btfss	PORTA,PKC_PIN	; "
	bra	$-1		; "
	btfsc	PORTA,PKC_PIN	;Wait for the device to pull the clock line low
	bra	$-1		; "
	movlb	1		;Copy state of carry into data line
	bcf	TRISA,PKD_PIN	; "
	btfsc	STATUS,C	; "
	bsf	TRISA,PKD_PIN	; "
	return			;Done with setting up this bit


;;; Lookup Tables ;;;

;Key lookup table for translating PS/2 scan codes to serial keyboard codes
;First column in comment is the PS/2 scan code, second is the name of the
; corresponding serial keyboard key with the PS/2 key in parens if different
;MSb of index is set if PS/2 scan code is preceded with 0xE0; index 0x7F is used
; to hold the corresponding code for 0x83 (F7) because this is the only code
; that has the MSb set
;MSB of value indicates type of key code
KeyLut	org	0x200

	dw	0x0000		;0x00		----
	dw	0x0A48		;0x01		(F9)
	dw	0x0000		;0x02		----
	dw	0x0A28		;0x03		(F5)
	dw	0x0A18		;0x04		(F3)
	dw	0x0A08		;0x05		(F1)
	dw	0x0A10		;0x06		(F2)
	dw	0x0A60		;0x07		(F12)
	dw	0x0000		;0x08		----
	dw	0x0A50		;0x09		(F10)
	dw	0x0A40		;0x0A		(F8)
	dw	0x0A30		;0x0B		(F6)
	dw	0x0A20		;0x0C		(F4)
	dw	0x0161		;0x0D		Tab
	dw	0x0165		;0x0E		~`
	dw	0x0000		;0x0F		----
	dw	0x0000		;0x10		----
	dw	0x0775		;0x11		Option (left Alt)
	dw	0x0571		;0x12		Shift (left)
	dw	0x0000		;0x13		----
	dw	0x066F		;0x14		Command (left Ctrl)
	dw	0x0119		;0x15		Q
	dw	0x0125		;0x16		1
	dw	0x0000		;0x17		----
	dw	0x0000		;0x18		----
	dw	0x0000		;0x19		----
	dw	0x010D		;0x1A		Z
	dw	0x0103		;0x1B		S
	dw	0x0101		;0x1C		A
	dw	0x011B		;0x1D		W
	dw	0x0127		;0x1E		2
	dw	0x0000		;0x1F		----
	dw	0x0000		;0x20		----
	dw	0x0111		;0x21		C
	dw	0x010F		;0x22		X
	dw	0x0105		;0x23		D
	dw	0x011D		;0x24		E
	dw	0x012B		;0x25		4
	dw	0x0129		;0x26		3
	dw	0x0000		;0x27		----
	dw	0x0000		;0x28		----
	dw	0x0163		;0x29		Space
	dw	0x0113		;0x2A		V
	dw	0x0107		;0x2B		F
	dw	0x0123		;0x2C		T
	dw	0x011F		;0x2D		R
	dw	0x012F		;0x2E		5
	dw	0x0000		;0x2F		----
	dw	0x0000		;0x30		----
	dw	0x015B		;0x31		N
	dw	0x0117		;0x32		B
	dw	0x0109		;0x33		H
	dw	0x010B		;0x34		G
	dw	0x0121		;0x35		Y
	dw	0x012D		;0x36		6
	dw	0x0000		;0x37		----
	dw	0x0000		;0x38		----
	dw	0x0000		;0x39		----
	dw	0x015D		;0x3A		M
	dw	0x014D		;0x3B		J
	dw	0x0141		;0x3C		U
	dw	0x0135		;0x3D		7
	dw	0x0139		;0x3E		8
	dw	0x0000		;0x3F		----
	dw	0x0000		;0x40		----
	dw	0x0157		;0x41		<,
	dw	0x0151		;0x42		K
	dw	0x0145		;0x43		I
	dw	0x013F		;0x44		O
	dw	0x013B		;0x45		0
	dw	0x0133		;0x46		9
	dw	0x0000		;0x47		----
	dw	0x0000		;0x48		----
	dw	0x015F		;0x49		>.
	dw	0x0159		;0x4A		?/
	dw	0x014B		;0x4B		L
	dw	0x0153		;0x4C		:;
	dw	0x0147		;0x4D		P
	dw	0x0137		;0x4E		_-
	dw	0x0000		;0x4F		----
	dw	0x0000		;0x50		----
	dw	0x0000		;0x51		----
	dw	0x014F		;0x52		"'
	dw	0x0000		;0x53		----
	dw	0x0143		;0x54		{[
	dw	0x0131		;0x55		+=
	dw	0x0000		;0x56		----
	dw	0x0000		;0x57		----
	dw	0x0873		;0x58		Caps Lock
	dw	0x0571		;0x59		Shift (right)
	dw	0x0149		;0x5A		Return (Enter)
	dw	0x013D		;0x5B		}]
	dw	0x0000		;0x5C		----
	dw	0x0155		;0x5D		|\
	dw	0x0000		;0x5E		----
	dw	0x0000		;0x5F		----
	dw	0x0000		;0x60		----
	dw	0x0000		;0x61		----
	dw	0x0000		;0x62		----
	dw	0x0000		;0x63		----
	dw	0x0000		;0x64		----
	dw	0x0000		;0x65		----
	dw	0x0167		;0x66		Backspace
	dw	0x0000		;0x67		----
	dw	0x0000		;0x68		----
	dw	0x0227		;0x69		Keypad 1
	dw	0x0000		;0x6A		----
	dw	0x022D		;0x6B		Keypad 4
	dw	0x0233		;0x6C		Keypad 7
	dw	0x0000		;0x6D		----
	dw	0x0000		;0x6E		----
	dw	0x0000		;0x6F		----
	dw	0x0225		;0x70		Keypad 0
	dw	0x0203		;0x71		Keypad .
	dw	0x0229		;0x72		Keypad 2
	dw	0x022F		;0x73		Keypad 5
	dw	0x0231		;0x74		Keypad 6
	dw	0x0237		;0x75		Keypad 8
	dw	0x0A00		;0x76		(Esc)
	dw	0x020F		;0x77		Keypad Clear (Num Lock)
	dw	0x0A58		;0x78		(F11)
	dw	0x040D		;0x79		Keypad +
	dw	0x022B		;0x7A		Keypad 3
	dw	0x021D		;0x7B		Keypad -
	dw	0x0405		;0x7C		Keypad *
	dw	0x0239		;0x7D		Keypad 9
	dw	0x0927		;0x7E		(Scroll Lock)
	dw	0x0A38		;0x83		(F7)
	dw	0x0000		;0xE0 0x00	----
	dw	0x0000		;0xE0 0x01	----
	dw	0x0000		;0xE0 0x02	----
	dw	0x0000		;0xE0 0x03	----
	dw	0x0000		;0xE0 0x04	----
	dw	0x0000		;0xE0 0x05	----
	dw	0x0000		;0xE0 0x06	----
	dw	0x0000		;0xE0 0x07	----
	dw	0x0000		;0xE0 0x08	----
	dw	0x0000		;0xE0 0x09	----
	dw	0x0000		;0xE0 0x0A	----
	dw	0x0000		;0xE0 0x0B	----
	dw	0x0000		;0xE0 0x0C	----
	dw	0x0000		;0xE0 0x0D	----
	dw	0x0000		;0xE0 0x0E	----
	dw	0x0000		;0xE0 0x0F	----
	dw	0x0000		;0xE0 0x10	----
	dw	0x0775		;0xE0 0x11	Option (right Alt)
	dw	0x0000		;0xE0 0x12	----
	dw	0x0000		;0xE0 0x13	----
	dw	0x066F		;0xE0 0x14	Command (right Ctrl)
	dw	0x0000		;0xE0 0x15	----
	dw	0x0000		;0xE0 0x16	----
	dw	0x0000		;0xE0 0x17	----
	dw	0x0000		;0xE0 0x18	----
	dw	0x0000		;0xE0 0x19	----
	dw	0x0000		;0xE0 0x1A	----
	dw	0x0000		;0xE0 0x1B	----
	dw	0x0000		;0xE0 0x1C	----
	dw	0x0000		;0xE0 0x1D	----
	dw	0x0000		;0xE0 0x1E	----
	dw	0x066F		;0xE0 0x1F	Command (left Super)
	dw	0x0000		;0xE0 0x20	----
	dw	0x0000		;0xE0 0x21	----
	dw	0x0000		;0xE0 0x22	----
	dw	0x0000		;0xE0 0x23	----
	dw	0x0000		;0xE0 0x24	----
	dw	0x0000		;0xE0 0x25	----
	dw	0x0000		;0xE0 0x26	----
	dw	0x066F		;0xE0 0x27	Command (right Super)
	dw	0x0000		;0xE0 0x28	----
	dw	0x0000		;0xE0 0x29	----
	dw	0x0000		;0xE0 0x2A	----
	dw	0x0000		;0xE0 0x2B	----
	dw	0x0000		;0xE0 0x2C	----
	dw	0x0000		;0xE0 0x2D	----
	dw	0x0000		;0xE0 0x2E	----
	dw	0x0169		;0xE0 0x2F	Enter (Apps)
	dw	0x0000		;0xE0 0x30	----
	dw	0x0000		;0xE0 0x31	----
	dw	0x0000		;0xE0 0x32	----
	dw	0x0000		;0xE0 0x33	----
	dw	0x0000		;0xE0 0x34	----
	dw	0x0000		;0xE0 0x35	----
	dw	0x0000		;0xE0 0x36	----
	dw	0x0A68		;0xE0 0x37	(ACPI Power)
	dw	0x0000		;0xE0 0x38	----
	dw	0x0000		;0xE0 0x39	----
	dw	0x0000		;0xE0 0x3A	----
	dw	0x0000		;0xE0 0x3B	----
	dw	0x0000		;0xE0 0x3C	----
	dw	0x0000		;0xE0 0x3D	----
	dw	0x0000		;0xE0 0x3E	----
	dw	0x0A70		;0xE0 0x3F	(ACPI Sleep)
	dw	0x0000		;0xE0 0x40	----
	dw	0x0000		;0xE0 0x41	----
	dw	0x0000		;0xE0 0x42	----
	dw	0x0000		;0xE0 0x43	----
	dw	0x0000		;0xE0 0x44	----
	dw	0x0000		;0xE0 0x45	----
	dw	0x0000		;0xE0 0x46	----
	dw	0x0000		;0xE0 0x47	----
	dw	0x0000		;0xE0 0x48	----
	dw	0x0000		;0xE0 0x49	----
	dw	0x041B		;0xE0 0x4A	Keypad /
	dw	0x0000		;0xE0 0x4B	----
	dw	0x0000		;0xE0 0x4C	----
	dw	0x0000		;0xE0 0x4D	----
	dw	0x0000		;0xE0 0x4E	----
	dw	0x0000		;0xE0 0x4F	----
	dw	0x0000		;0xE0 0x50	----
	dw	0x0000		;0xE0 0x51	----
	dw	0x0000		;0xE0 0x52	----
	dw	0x0000		;0xE0 0x53	----
	dw	0x0000		;0xE0 0x54	----
	dw	0x0000		;0xE0 0x55	----
	dw	0x0000		;0xE0 0x56	----
	dw	0x0000		;0xE0 0x57	----
	dw	0x0000		;0xE0 0x58	----
	dw	0x0000		;0xE0 0x59	----
	dw	0x0219		;0xE0 0x5A	Keypad Enter
	dw	0x0000		;0xE0 0x5B	----
	dw	0x0000		;0xE0 0x5C	----
	dw	0x0000		;0xE0 0x5D	----
	dw	0x0A78		;0xE0 0x5E	(ACPI Wake)
	dw	0x0000		;0xE0 0x5F	----
	dw	0x0000		;0xE0 0x60	----
	dw	0x0000		;0xE0 0x61	----
	dw	0x0000		;0xE0 0x62	----
	dw	0x0000		;0xE0 0x63	----
	dw	0x0000		;0xE0 0x64	----
	dw	0x0000		;0xE0 0x65	----
	dw	0x0000		;0xE0 0x66	----
	dw	0x0000		;0xE0 0x67	----
	dw	0x0000		;0xE0 0x68	----
	dw	0x0000		;0xE0 0x69	(End)
	dw	0x0000		;0xE0 0x6A	----
	dw	0x030D		;0xE0 0x6B	Left Arrow
	dw	0x0000		;0xE0 0x6C	(Home)
	dw	0x0000		;0xE0 0x6D	----
	dw	0x0000		;0xE0 0x6E	----
	dw	0x0000		;0xE0 0x6F	----
	dw	0x0000		;0xE0 0x70	(Insert)
	dw	0x0000		;0xE0 0x71	(Delete)
	dw	0x0311		;0xE0 0x72	Down Arrow
	dw	0x0000		;0xE0 0x73	----
	dw	0x0305		;0xE0 0x74	Right Arrow
	dw	0x031B		;0xE0 0x75	Up Arrow
	dw	0x0000		;0xE0 0x76	----
	dw	0x0000		;0xE0 0x77	----
	dw	0x0000		;0xE0 0x78	----
	dw	0x0000		;0xE0 0x79	----
	dw	0x0000		;0xE0 0x7A	(Page Down)
	dw	0x0000		;0xE0 0x7B	----
	dw	0x0929		;0xE0 0x7C	(Print Screen)
	dw	0x0000		;0xE0 0x7D	(Page Up)
	dw	0x0000		;0xE0 0x7E	----
	dw	0x0925		;0xE1 0x14	(Pause)


;;; Subprograms ;;;

ReadPage
	movlb	3		;Save the parameter in W into low byte of
	movwf	PMADRL		; program memory address
	movlw	0x03		;High byte of program memory address is always
	movwf	PMADRH		; the uppermost 256 bytes
	clrf	FSR1L		;Start writing pointer off at the beginning of
	bsf	FLAGS,BFINUSE	; the buffer and mark it as being in use
ReadPg0	bcf	INTCON,GIE	;Read a word from program memory
	bsf	PMCON1,RD	; "
	nop			; "
	nop			; "
	bsf	INTCON,GIE	; "
	bsf	STATUS,C	;Shift bits from program memory around when
	rlf	PMDATL,F	; copying them to RAM so that 0b00AAAAAAABBBBBBB
	rlf	PMDATH,W	; becomes 0bAAAAAAA1BBBBBBB1 (LSbs of bytes sent
	bsf	STATUS,C	; to serial keyboard are always 1)
	rlf	WREG,W		; "
	movwi	FSR1++		; "
	movf	PMDATL,W	; "
	movwi	FSR1++		; "
	incf	PMADRL,F	;Advance program memory address
	btfss	FSR1L,5		;If we haven't yet read out 16 words (32 bytes),
	bra	ReadPg0		; loop to read out the next word
	movlw	high KeyLut	;Point back to the key lookup table
	movwf	PMADRH		; "
	return			;Done

WritePage
	movlb	3		;Save the parameter in W into low byte of
	movwf	PMADRL		; program memory address
	movlw	0x03		;High byte of program memory address is always
	movwf	PMADRH		; the uppermost 256 bytes
	clrf	FSR0L		;Start pointer off at the beginning of buffer
	bcf	INTCON,GIE	;Interrupts must be off for this whole affair
	bsf	PMCON1,WREN	;Enable writes
	bsf	PMCON1,FREE	;Specify an erase operation
	movlw	0x55		;Execute erase of page
	movwf	PMCON2		; "
	movlw	0xAA		; "
	movwf	PMCON2		; "
	bsf	PMCON1,WR	; "
	nop			; "
	nop			; "
	bsf	PMCON1,LWLO	;Enable writing of latches
WriteP0	lsrf	INDF0,W		;Shift bits from SRAM around when writing to
	lsrf	WREG,W		; program memory so that 0bAAAAAAAxBBBBBBBx
	movwf	PMDATH		; becomes 0b00AAAAAAABBBBBBB (LSbs of bytes sent
	incf	FSR0L,F		; to serial keyboard are always 1)
	rrf	INDF0,W		; "
	movwf	PMDATL		; "
	incf	FSR0L,F		; "
	btfsc	FSR0L,5		;If this last word will fill all 16 latches, we
	bcf	PMCON1,LWLO	; want the next write to write to flash for real
	movlw	0x55		;Write to latches or to flash
	movwf	PMCON2		; "
	movlw	0xAA		; "
	movwf	PMCON2		; "
	bsf	PMCON1,WR	; "
	nop			; "
	nop			; "
	incf	PMADRL,F	;Advance program memory address
	btfss	FSR0L,5		;If we haven't yet written out 16 words (32
	bra	WriteP0		; bytes), loop to write out the next word
	bcf	PMCON1,WREN	;Disable writes again
	bsf	INTCON,GIE	;Reenable interrupts
	movlw	high KeyLut	;Point back to the key lookup table
	movwf	PMADRH		; "
	return			;Done

PKPop
	movf	PK_QPSH,W	;If queue is empty, return with Z flag set
	xorwf	PK_QPOP,W	; "
	btfsc	STATUS,Z	; "
	return			; "
	movf	PK_QPOP,W	;Load the pop pointer for dereferencing
	movwf	FSR0L		; "
	incf	PK_QPOP,F	;Advance and wrap the pop pointer
	bcf	PK_QPOP,4	; "
	movf	INDF0,W		;Pop the next byte off the queue
	bcf	STATUS,Z	;Make sure Z is clear so caller knows a byte was
	return			; popped

SKPush
	movwi	FSR1++		;Push W onto serial keyboard queue/data buffer
	movf	PK_PROG,W	;If we're in programming mode for a programmable
	btfss	STATUS,Z	; key, take some extra steps
	call	SKPush0		; "
	bcf	FSR1L,5		;Wrap the pointer and copy it back to SK_QPSH
	movf	FSR1L,W		; "
	movwf	SK_QPSH		; "
	return			;Done
SKPush0	btfss	FLAGS,PRGMCHG	;If the program for this key hadn't changed yet,
	call	SKPush1		; take some extra extra steps
	movf	FSR1L,W		;If we just wrote the last event in the buffer,
	andlw	B'00001111'	; back the pointer up so we don't damage the one
	btfsc	STATUS,Z	; in the other half of the buffer
	decf	FSR1L,F		; "
	return			;Return
SKPush1	bsf	FLAGS,PRGMCHG	;Flag that the program for this key has changed
SKPush2	movlw	SKNULL		;Overstrike the rest of the half of the buffer
	movwi	FSR1++		; where the push pointer is with SKNULL
	movf	FSR1L,W		; "
	andlw	B'00001111'	; "
	btfss	STATUS,Z	; "
	bra	SKPush2		; "
	addfsr	FSR1,-15	;Rewind to the second position in the buffer
	return			;Done

SKReleaseMods
	btfss	SK_MODS,MOD_CPL	;If the caps lock key is down, push a caps lock
	bra	SKReleaseMods2	; key release event onto the serial keyboard
	bcf	SK_MODS,MOD_CPL	; queue and clear the flag
	movlw	SKCAPS | 0x80	; "
	call	SKPush		; "
	;fall through

SKReleaseMods2
	btfss	SK_MODS,MOD_SHF	;If the shift key is down, push a shift key
	bra	SKRele0		; release event onto the serial keyboard queue
	bcf	SK_MODS,MOD_SHF	; and clear the flag
	movlw	SKSHIFT | 0x80	; "
	call	SKPush		; "
SKRele0	btfss	SK_MODS,MOD_CMD	;If the command key is down, push a command key
	bra	SKRele1		; release event onto the serial keyboard queue
	bcf	SK_MODS,MOD_CMD	; and clear the flag
	movlw	SKCMD | 0x80	; "
	call	SKPush		; "
SKRele1	btfss	SK_MODS,MOD_OPT	;If the option key is down, push an option key
	bra	SKWaitQueue	; release event onto the serial keyboard queue
	bcf	SK_MODS,MOD_OPT	; and clear the flag
	movlw	SKOPT | 0x80	; "
	call	SKPush		; "
	;fall through		;Fall through to wait for the queue to empty

SKWaitQueue
	movf	SK_QPSH,W	;If the serial keyboard queue is not yet empty,
	xorwf	SK_QPOP,W	; loop until it is
	btfss	STATUS,Z	; "
	bra	SKWaitQueue	; "
	return			;Done


;;; Non-Volatile Parameter Storage ;;;

	org	0x380

		;Esc = command-period
	dw	0x1BAF,0x37F7,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F1 = command-Z (undo)
	dw	0x1B86,0x2377,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F2 = command-X (cut)
	dw	0x1B87,0x23F7,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F3 = command-C (copy)
	dw	0x1B88,0x2477,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F4 = command-V (paste)
	dw	0x1B89,0x24F7,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F5 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F6 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F7 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F8 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F9 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F10 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F11 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;F12 = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;ACPI Power = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;ACPI Sleep = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD

		;ACPI Wake = unassigned
	dw	0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD,0x1EBD


;;; End of Program ;;;

	end
