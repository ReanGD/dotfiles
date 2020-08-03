local wibox = require("wibox")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local systray = {}

-- Module functions
--------------------------------------------------------------------------------

-- Constructor
--------------------------------------------------------------------------------
function systray:init(args)
    args = args or {}

    self.widget = wibox.widget.systray()
end

return systray
