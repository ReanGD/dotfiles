local awful = require("awful")
local beautiful = require("beautiful")

local utils = require("src.utils")

local env = {}

function env:init()
    os.setlocale(os.getenv("LANG"))
    
    self.modkey = "Mod4"
    self.terminal = "urxvt"
    self.home = os.getenv("HOME")
    self.hostname = utils.output_read("uname -n")
    self.is_notebook = (self.hostname == "archmini" or self.hostname == "archnote")
    self.scripts_dir = awful.util.getdir("config") .. "/scripts/"

    -- theme setup (/usr/share/awesome/themes/)
    self.themedir = awful.util.getdir("config").."config/"
    beautiful.init(self.themedir .. "theme-config.lua")
end

return env
