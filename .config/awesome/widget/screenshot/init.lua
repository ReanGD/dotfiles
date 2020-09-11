local awful = require("awful")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local screenshot = {}

local os = os

-- Local functions
--------------------------------------------------------------------------------
local function exec(cmd)
	awful.util.spawn_with_shell(cmd)
end

-- Module functions
--------------------------------------------------------------------------------
function screenshot:screen()
    exec("flameshot full -c -p " .. self.flameshot_dir)
    -- exec("scrot " .. self.scrot_screenshot_path .. " --exec 'xclip -selection c -t image/png < $f'")
end

function screenshot:rect()
    exec("flameshot gui -p " .. self.flameshot_dir)
    -- exec("sleep 0.5 && scrot --select " .. self.scrot_screenshot_path .. " --exec 'xclip -selection c -t image/png < $f'")
end

function screenshot:focused_window()
    exec("scrot --focused " .. self.scrot_screenshot_path .. " --exec 'xclip -selection c -t image/png < $f'")
end

-- Constructor
--------------------------------------------------------------------------------
function screenshot:init(args)
	args = args or {}
	local dir = args.dir or os.getenv("HOME") .. "/tmp/"

	self.flameshot_dir = dir
	self.scrot_screenshot_path = dir .. "$(date +%F_%T).png"
end

return screenshot
