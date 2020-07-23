local gfs = require("gears.filesystem")
local beautiful = require("beautiful")
local utils = require("src.utils")

local env = {}

function env:init(args)
    args = args or {}

    local theme = args.theme or "default"
    local locale = args.locale or os.getenv("LANG")
    self.modkey = args.modkey or "Mod4"
    self.terminal = args.terminal or "urxvt"
    self.home = os.getenv("HOME")
    self.hostname = utils.output_read("uname -n")
    self.theme_dir = gfs.get_configuration_dir() .. "themes/" .. theme
    self.scripts_dir = gfs.get_configuration_dir() .. "sripts/"

    os.setlocale(locale)
    beautiful.init(env.theme_dir .. "/theme.lua")
end

return env
