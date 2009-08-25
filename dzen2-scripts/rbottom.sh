#!/bin/zsh
# vim:ft=zsh ts=4
#------------------------------------------------------------------------------
#   DZEN BAR CONFIGURATION
#   Filename: rbottom.sh
#------------------------------------------------------------------------------


#   SETTINGS
#------------------------------------------------------------------------------
DIR='/usr/bin/'                 # Path to dzen
BARBG='#616161'                 # bg color gdbar
BARFG='#990000'                 # fg color gdbar
BARH=8                          # Height of gdbar
BARW=50                         # Width of gdbar

SLEEP=20

ICONPATH='/path/to/icons/'      # Path to dzen icons
BCOLOR='#990000'                # Icon color

MAILDIR='/path/to/maildir'      # Path to Maildir to check for new mail

DZMAIL="urxvt -T mailbox -e mutt"

typeset -A disks
DISKS=(root / home /home usr /usr)


# MY FUNCTIONS
#------------------------------------------------------------------------------
xmonadver() {
   xmonad --version
}

smail() { 
    local -A counts; local i

    for i in "${MAILDIR}"/*/new/*
        { (( counts[${i:h:h:t}]++ )) }
    for i in ${(k)counts}
        { print -n $counts[$i]' ' }
}

# OUTPUT FORMAT
#------------------------------------------------------------------------------
while true; do

    print -n " ^fg(#616161)$(xmonadver) ^fg(#C4C4C4)- As above so below   ^fg($BCOLOR)^ca(1, ${DZMAIL})^i(${ICONPATH}/sm4tik/mail.xbm)   ^fg(#F7D981)${$(smail):-"^fg(#616161)0 "} ^ca()\n"

    sleep $SLEEP
done | $DIR/dzen2 -h 16 -x 900 -y 1034 -ta r -fn '-*-liberation mono-*-r-*-*-10-*-*-*-*-*-*-*' -e ''
