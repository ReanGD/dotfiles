local awful = require("awful")

local autostart = {}

function autostart.run()
    awful.spawn("/home/rean/.config/bin/firefox", { tag = "web" })
    awful.spawn("subl3", { tag = "doc" })
    awful.spawn("doublecmd", { tag = "cmdr" })
end

return autostart
