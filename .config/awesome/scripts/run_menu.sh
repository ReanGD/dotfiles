res=$(echo -e "reboot\npoweroff\nsuspend\nhibernate" | rofi -dmenu)
if [ $? -eq 0 ];
then
  systemctl $res
fi