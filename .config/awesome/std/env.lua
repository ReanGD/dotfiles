local gears = require("gears")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local subprocess = require("std.subprocess")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local env = {}

-- Constructor
--------------------------------------------------------------------------------
function env:init(args)
	args = args or {}

	local theme = args.theme or "default"
	local locale = args.locale or os.getenv("LANG")
	self.modkey = args.modkey or "Mod4"
	-- TODO: remove this (it is for extenal code)
	self.mod = self.modkey
	self.terminal = args.terminal or "urxvt"
	self.home = os.getenv("HOME")
	self.hostname = subprocess.output_read("uname -n")
	self.theme_dir = gfs.get_configuration_dir() .. "themes/" .. theme
	self.scripts_dir = gfs.get_configuration_dir() .. "sripts/"

	os.setlocale(locale)
	beautiful.init(env.theme_dir .. "/theme.lua")
end

-- Wallpaper setup
--------------------------------------------------------------------------------
env.wallpaper = function(s)
	if beautiful.wallpaper then
		if not env.desktop_autohide and gfs.file_readable(beautiful.wallpaper) then
			gears.wallpaper.maximized(beautiful.wallpaper, s, true)
		else
			gears.wallpaper.set(beautiful.bg_normal)
		end
	end
end

return env
