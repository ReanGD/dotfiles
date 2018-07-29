operation=$(echo -e "reboot\npoweroff\nsuspend\nhibernate\ntv_on\ntv_off" | rofi -dmenu)
res=$?

if [ $res -ne 0 ]
then
	exit $res  
fi

case "$operation" in
	"reboot" | "poweroff" | "suspend" | "hibernate")
	systemctl $operation
	;;
	"tv_on")
	xrandr --output HDMI-0 --primary --auto --output HDMI-1 --left-of HDMI-0 --auto
	;;
	"tv_off")
	xrandr --output HDMI-1 --off
	;;
esac