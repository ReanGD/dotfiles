local base = require("wibox.widget.base")

return setmetatable({
    base = base;
    textclock = require("widget.textclock");
    keyboard = require("widget.keyboard");
}, {__call = function(_, args) return base.make_widget_declarative(args) end})
