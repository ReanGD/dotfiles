local gears = require("gears")
local beautiful = require("beautiful")
local subprocess = require("std.subprocess")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local env = {}

local gfs = gears.filesystem

-- Module functions
--------------------------------------------------------------------------------
function env:wallpaper_setup(screen)
	if beautiful.wallpaper then
		if not self.desktop_autohide and gfs.file_readable(beautiful.wallpaper) then
			gears.wallpaper.maximized(beautiful.wallpaper, screen, true)
		else
			gears.wallpaper.set(beautiful.bg_normal)
		end
	end
end

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
	self.mixer = args.mixer or "pavucontrol"
	self.home = os.getenv("HOME")
	self.hostname = subprocess.output_read("uname -n")
	self.theme_dir = gfs.get_configuration_dir() .. "themes/" .. theme
	self.scripts_dir = gfs.get_configuration_dir() .. "sripts/"

	os.setlocale(locale)
	beautiful.init(env.theme_dir .. "/theme.lua")
end

return env
