# wabiForth - a bare-metal, compiling, somewhat optimizing and more or less ANSI compatible Forth for the Raspberry pi3b+

This is a first pre-release. Source-code etc will be added the comming weeks.

WabiForth is copyright J.J. Hoekstra 2022 and licensed for free use under AGPL-3.0.

Using wabiForth is fairly easy:

	add adequate cooling-solution to your Raspberry 3B+ (see manual)
	check if you added adequate cooling, if not: DO NOT CONTINUE (really!!)
    copy all files from wabiForthFiles on a micro sd-card (FAT32)
    put the sd-card in a Raspberry 3B+
    connect a serial (UART) connection to pin 8 (TX) and pin 10 (RCV)
       (UART protocol is 115200-8N1)
    optionally connect a monitor to the HDMI connector (1024x768 pixels)
    power up
    
WabiForth replies in 3 seconds on the UART connection and in 5 seconds on the HDMI connector.

In the user-manual you will find a lot more info.
