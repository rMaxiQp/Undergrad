# electornic_trading_spring_2019_Timeserver

## Raspberry Pi NEO-6M GPS configuration guide

### Wiring
|GPS Module    |    Raspberry Pi|
| ------------ |  ------------  |
|VCC           |      Pin 1     |
|RX            |      Pin 8     |
|TX            |      Pin 10    |
|GND           |      Pin 6     |
|PPS           |      Pin 12    |

### Raspberry Pi UARTs
|         | Raspberry Pi 3 and Raspberry Pi Zero W  |   Other Pi's  |
|---------| --------------------------------------  | ------------- |
|  PL011  |             Bluetooth module            |  primary UART |
|mini UART|             primary UART                |               |

### Serial port
|             | Raspberry Pi 3 and Raspberry Pi Zero W  |   Other Pi's  |
|-------------| --------------------------------------  | ------------- |
| Serial Port |            /dev/ttyS0                   |  /dev/ttyAMA0 |

### Switch port
Add this line into *config.txt* to use the mini-UART (ttyS0) for Pi3 Bluetooth function. <br>
`dtoverlay=pi3-miniuart-bt`<br>
Since this may reduce the maximum usable baudrate, it is necessary to fix core frequency to make it works. To do that, add following line.<br>
`core_freq=250`<br><br>

### Enable UART
Edit *config.txt* <br>
`sudo nano /boot/config.txt`<br>
Add this line at bottom<br>
`enable_uart=1`<br>
Notice that this will also enforce core_freq to 250Mhz unless force_turbo is set(400Mhz).<br>

*Ctrl-X* to exit and *Enter* to save<br>
`sudo reboot now` to reboot <br><br>

After reboot, test if config is correct.<br>
`ls -l /dev`<br>
Should see<br>
`serial0 -> ttyS0`
`serial1 -> ttyAMA0`<br><br>

### Disable the console
`sudo systemctl stop serial-getty@ttyS0.service`<br>
`sudo systemctl disable serial-getty@ttyS0.service`<br>
Edit cmdline.txt<br>
`sudo nano /boot/cmdline.txt`<br>
Delete *console=serial0,115200*<br>


### Read data from GPS module
#### Use cat
`cat /dev/ttyS0`<br>
![alt text](console_output/cat.png "Logo Title Text 1")<br>

#### Use minicom
`sudo apt-get update`<br>
`sudo apt-get install minicom gpsd gpsd-clients`<br>
`sudo reboot now`<br><br>
Inform the module work with 9600 baud rate<br>
`stty -F /dev/ttyS0 9600`<br>
Check configuration<br>
`stty -F /dev/ttyS0`<br>
Accept raw GPS data<br>
`minicom -b 9600 -o -D /dev/ttyS0`<br>
![alt text](console_output/minicom.png "Logo Title Text 1")

#### Use gpsd library
##### GPSD configuration
File */etc/default/gpsd*<br>
`# Default settings for the gpsd init script and the hotplug wrapper.`<br>

`# Start the gpsd daemon automatically at boot time`<br>
`START_DAEMON="true"`<br>

`# Use USB hotplugging to add new USB devices automatically to the daemon`<br>
`USBAUTO="false"`<br>

`# Devices gpsd should collect to at boot time.`<br>
`# They need to be read/writeable, either by user gpsd or the group dialout.`<br>
`DEVICES="/dev/ttyS0 /dev/pps0"`<br>

`# Other options you want to pass to gpsd`<br>
`#`<br>
`# -n    don't wait for client to connect; poll GPS immediately`<br>

`GPSD_OPTIONS="-n"`<br>
<br><br>

Restart to take effect<br>
`systemctl daemon-reload`<br>
`systemctl enable gpsd`<br>
`systemctl start gpsd`<br><br>

Check gpsd status<br>
`systemctl status gpsd`<br>
Start gpsd and specify the baud rate<br>
`sudo gpsd /dev/ttyS0 -F /var/run/gpsd.sock -n`<br>
`cgps -s`<br>
![alt text](console_output/cpsd.png "Logo Title Text 1")
<br>
##### Trouble Shooting
If nothing shows up, `sudo killall gpsd`, then try again.<br><br>


## NTP configuration
### Disable NTP support in DHCP
Remove these lines in */etc/dhcp/dhclient.conf*<br>

`#supersede domain-name "fugue.com home.vix.com";`<br>
`#prepend domain-name-servers 127.0.0.1;`<br>
`request subnet-mask, broadcast-address, time-offset, routers,`<br>
`        domain-name, domain-name-servers, domain-search, host-name,`<br>
`        dhcp6.name-servers, dhcp6.domain-search,`<br>
`        netbios-name-servers, netbios-scope, interface-mtu,`<br>
`        rfc3442-classless-static-routes , `~~ntp-servers;~~<br>
`#require subnet-mask, domain-name-servers;`<br>
`#timeout 60;`<br>
<br>
`# A list of options to request from the DHCP server.`<br>
`option domain_name_servers, domain_name, domain_search, host_name`<br>
`option classless_static_routes`<br>
`# Most distributions have NTP support.`<br>
~~# option ntp_servers~~<br>
`# Respect the network MTU.`
<br><br>
**Remove these files:**<br>
*/etc/dhcp/dhclient-exit-hooks.d/ntp*<br>
*/lib/dhcpcd/dhcpcd-hooks/50-ntp.conf*<br>
*/var/lib/ntp/ntp.conf.dhcp (might not exist)*<br><br>

### GPS PPS configuration
PPS is using *GPIO #4* or *GPIO #18* depends on different GPS Hat<br>
Install<br>
`apt-get install pps-tools`<br>
Enable PPS support in the kernel<br>
in */boot/config.txt*<br>
add `dtoverlay=pps-gpio,gpiopin=4`<br>
Test<br>
`ppstest /dev/pps0`<br>
Should see<br>
![alt text](console_output/ppstest.png "Logo Title Text")<br>

### Feed GPS data into NTP
In file */etc/ntp.conf*, add these lines to add [driver](https://gist.github.com/edro15/c3fbaaabfe31ecb799363ffab587f336)<br>
`# GPS PPS reference`<br>
`server 127.127.28.2 prefer`<br>
`fudge  127.127.28.2 refid PPS`<br>

`# get time from SHM from gpsd; this seems working`<br>
`server 127.127.28.0`<br>
`fudge  127.127.28.0 refid GPS`<br>

Driver examples<br>

|Drivers      | Meaning                                |
|-----------  |--------------------------------------- |
|127.127.19.u | WWV/WWVH-based radio receiver          |
|127.127.20.u | NMEA-based GPS clock                   |
|127.127.28.u | SHM driver                             |

<br><br>

Restart and run ntpq<br>
`systemctl restart ntp`<br>
`ntpq -p`<br>
Should see (like in logging file *gps_ntp_output.log*)<br>
![alt text](console_output/ntp.png "Logo Title Text 1")<br>
<br>
Peer status word(the character before the remote) shows the status of the peer in NTP.<br>

|Char    | Meaning                                |
|------- |--------------------------------------- |
|(blank) | Invalid                                |
|X       | Discarded by the intersection algorithm|
|-       | Discarded by the cluster algorithm     |
|+       | Included by the combine algorithm      |
|*       | System Peer                            |
<br>

## GPS Raw Data Parser
Language: Python3 <br>
Input: GPS - NMEA sentence from file/ serial port <br>
Output: .csv file <br><br>

The parser only parse **$GPGGA** interpreted sentence (Global Positioning System Fix Data). <br>
Example input (dataset/gps_record_4_28.log):<br>
![alt text](console_output/gpsdata.png "Logo Title Text 1")<br>
The sentence consists timestamp, latitude, longitude, number of satellites fixed, altitude and data quaulity.<br>
The output data will be saved into a csv file for later plotting and analysis. <br>
Example output (GPSData.csv): <br>
![alt text](console_output/csvexample.png "Logo Title Text 1")<br>

## Reference
[The Raspberry Pi UARTs](https://www.raspberrypi.org/documentation/configuration/uart.md)
[Core frequency and uart](https://www.raspberrypi.org/forums/viewtopic.php?t=213499)
[Build Raspberry Pi GPS location/navigation device](https://tutorials-raspberrypi.com/build-raspberry-pi-gps-location-navigation-device/)
[Building a GPS Time Server with the Raspberry Pi 3](http://www.unixwiz.net/techtips/raspberry-pi3-gps-time.html)
