#!/bin/zsh
# vim:ft=zsh ts=4
#==============================================================================
#   DZEN BAR CONFIGURATION
#   Filename: rtop.sh
#   Requirements: bc, gdbar
#==============================================================================
#------------------------------------------------------------------------------
#   Last updated: September 1 - 2009
#   Changelog: Nothing much, just cleaning up the script.
#------------------------------------------------------------------------------


#   SETTINGS
#------------------------------------------------------------------------------
DIR='/usr/bin/'                 # Path to dzen
BARBG='#616161'                 # bg color gdbar
BARFG='#990000'                 # fg color gdbar
BARH=8                          # Height of gdbar
BARW=50                         # Width of gdbar

SLEEP=1

ICONPATH='/path/to/icons/'      # Path to dzen icons
BCOLOR='#990000'                # Icon color

INTERFACE=eth0

RXB=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
TXB=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`

SDEVICE='PCM'

MYDATE_FORMAT='%H:%M:%S - %a, %d.%m.%y'

#   MY FUNCTIONS
#------------------------------------------------------------------------------
mydate() {
    date  +${MYDATE_FORMAT}
}

netmon() {
   RXBN=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
   TXBN=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`

   RXR=$(printf "%4d\n" $(echo "($RXBN - $RXB) / 1024/${SLEEP}" | bc))
   TXR=$(printf "%4d\n" $(echo "($TXBN - $TXB) / 1024/${SLEEP}" | bc))

   print -n "${INTERFACE}: ^fg(white)${RXR} kB/s^fg(#80AA83)^p(3)^i(${ICONPATH}/sm4tik/net_down_03.xbm)^fg(white)${TXR} kB/s^fg(orange3)^i(${ICONPATH}/sm4tik/net_up_03.xbm)^fg()"

   RXB=$RXBN
   TXB=$TXBN
}

volicon() {
    ismuted=`amixer sget PCM | grep "Front" | awk '{if (NR == "2") print $7}' | sed -e 's/\[//g;s/\]//g'`

    if [[ $ismuted == "on" ]]
        then
        print "^i(${ICONPATH}/sm4tik/spkr_01.xbm)"
    elif [[ $ismuted == "off" ]]
        then
        print "^fg(#616161)^i(${ICONPATH}/sm4tik/spkr_02.xbm)"
    fi
}

fvolume() {
    percentage=`amixer sget $SDEVICE | grep "Front" | awk {'print $5'} | grep -m 1 % | sed -e 's/[][%]//g'`

    if [[ $percentage == 100 ]]
        then
        print -n "$(echo $percentage | $DIR/gdbar -fg $BARFG -bg $BARBG -h $BARH -w $BARW -s o -nonl)" # Volume full
    elif [[ $percentage -gt 50 ]]
        then
        print -n "$(echo $percentage | $DIR/gdbar -fg $BARFG -bg $BARBG -h $BARH -w $BARW -s o -nonl)" # Volume half to full
    elif [[ $percentage -gt 25 ]]
        then
        print -n "$(echo $percentage | $DIR/gdbar -fg $BARFG -bg $BARBG -h $BARH -w $BARW -s o -nonl)" # Volume quarter to half 
    elif [[ $percentage -lt 26 ]]
        then
        print -n "$(echo $percentage | $DIR/gdbar -fg $BARFG -bg $BARBG -h $BARH -w $BARW -s o -nonl)" # Volume low to quarter
    fi
}


#   OUTPUT FORMAT
#------------------------------------------------------------------------------
while true; do

    RXBN=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
    TXBN=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`

    RXR=$(printf "%3d\n" $(echo "($RXBN - $RXB) / 1024/${SLEEP}" | bc))
    TXR=$(printf "%3d\n" $(echo "($TXBN - $TXB) / 1024/${SLEEP}" | bc))

    print -n " ^fg(#616161)${INTERFACE}: ^fg($BCOLOR)${RXR} ^fg(#616161)kB/s ^fg($BCOLOR)^i(${ICONPATH}/sm4tik/net_down_02.xbm) ^fg($BCOLOR)${TXR} ^fg(#616161)kB/s ^fg($BCOLOR)^i(${ICONPATH}/sm4tik/net_up_02.xbm)   ^ca(1, "amixer set PCM toggle")^fg($BCOLOR)$(volicon)^ca()  ^fg()$(fvolume)   ^fg($BCOLOR)^i(${ICONPATH}/sm4tik/clock.xbm)   ^fg(#C4C4C4)$(mydate)   ^fg($BCOLOR)^i($ICONPATH/sm4tik/arch.xbm) \n"

	RXB=$RXBN; TXB=$TXBN

    sleep $SLEEP
done | $DIR/dzen2 -h 20 -x 1000 -ta r -fn '-*-liberation mono-*-r-*-*-10-*-*-*-*-*-*-*' -e ''
