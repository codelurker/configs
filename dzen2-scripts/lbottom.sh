#!/bin/zsh
# vim:ft=zsh ts=4
#------------------------------------------------------------------------------
#   DZEN BAR CONFIGURATION
#   Filename: lbottom.sh
#   Requirements: gdbar
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

MAXPOS="100"

MPDRW="mpc seek -10"
MPDFW="mpc seek +10"
MPDN="mpc next"
MPDP="mpc prev"
MPDPLAY="mpc toggle"
MPDSTOP="mpc stop"

#   MY FUNCTIONS
#------------------------------------------------------------------------------
version() {
	uname -r
}

mpd() {
  POS=`mpc | sed -ne 's/^.*(\([0-9]*\)%).*$/\1/p'`
  POSM="$POS $MAXPOS"
  echo "$POSM" | gdbar -h $BARH -w $BARW -fg $BARFG -bg $BARBG -s o -nonl
}

mpdtitle() {
    echo "^fg(#616161)Now Playing: ^fg(#C4C4C4)`mpc | sed -n '1p' | awk '{if ($1 != "volume:") print}'`" | tr '\n' ' '
}

mpdplay() {
    toggle=`mpc | awk {'print $1'} | sed -n '2p' | sed -e 's/\[//g;s/\]//g'`
    if [[ $toggle == "playing" ]]
        then
        print "^i(${ICONPATH}/sm4tik/play.xbm)"
    elif [[ $toggle == "paused" ]]
        then
        print "^fg(#616161)^i(${ICONPATH}/sm4tik/pause.xbm)"
    elif [[ $toggle != "playing" ]]
        then
        print "^fg(#616161)^i(${ICONPATH}/sm4tik/stop.xbm)"
    fi
}



# OUTPUT FORMAT
#------------------------------------------------------------------------------
while true; do

    print -n "  ^fg($BCOLOR)^i(${ICONPATH}/sm4tik/shroom.xbm)  ^fg(#616161)$(version)   ^fg($BCOLOR)^i(${ICONPATH}/sm4tik/note.xbm) ^fg(#616161) MPD: ^fg(#616161)^ca(1, ${MPDP})^i(${ICONPATH}/sm4tik/prev.xbm)^ca() ^fg($BCOLOR)^ca(1, ${MPDPLAY})$(mpdplay)^ca() ^fg(#616161)^ca(1, ${MPDRW})^i(${ICONPATH}/sm4tik/rwd.xbm)^ca() ^ca(1, ${MPDFW})^i(${ICONPATH}/sm4tik/fwd.xbm)^ca() ^ca(1, ${MPDN})^i(${ICONPATH}/sm4tik/next.xbm)^ca()  $(mpd)  $(mpdtitle) \n"

    sleep $SLEEP
done | $DIR/dzen2 -h 16 -w 900 -y 1034 -ta l -fn '-*-liberation mono-*-r-*-*-10-*-*-*-*-*-*-*' -e ''
