# wabiForth - a bare-metal, compiling, somewhat optimizing and more or less ANSI compatible Forth for the Raspberry pi3b+

# wabiPi4 - a bare-metal, compiling, somewhat optimizing and more or less ANSI compatible Forth for the Raspberry pi4b (4 or 8 GB)

This is pre-release. Source-code etc will be added.

WabiForth is copyright J.J. Hoekstra 2022 and licensed for free use under AGPL-3.0.

WabiPi4 is copyright J.J. Hoekstra 2023 and licensed for free use under AGPL-3.0.

Using wabiForth is fairly easy:

	add adequate cooling-solution to your Raspberry 3B+ or Raspberry 4b
	   (see manual)
	check if you added adequate cooling, if not: DO NOT CONTINUE (really!!)
    copy all files from WabiForthFiles (for the Pi 3b+) or WabiPi4Files (for  
       the pi 4b) in the top directory of a micro sd-card (FAT32)
    put the sd-card in a Raspberry 3B+ or 4B
    connect a serial (UART) connection to pin 8 (TX) and pin 10 (RCV)
       (UART protocol is 115200-8N1)
    optionally connect a monitor to the HDMI connector (1024x768 pixels)
    power up
    
WabiForth replies in 3 seconds on the UART connection and in 5 seconds on the HDMI connector.

WabiPi4 replies in 6 seconds on the UART connection and in 8 seconds on the HDMI connector.

In the user-manual you will find a lot more info.
