local setmetatable = setmetatable
local os = os
local io = io


local awful = require("awful")
local naughty = require("naughty")
local textbox = require("wibox.widget.textbox")
local timer = require("gears.timer")
local DateTime = require("lgi").GLib.DateTime
local utils = require("src.utils")
local beautiful = require("beautiful")

local textclock = { mt = {} }

local function show_calendar(env)
    if textclock.calendar_notification == nil then
        awful.spawn.easy_async(env.scripts_dir .. "calendar.sh",
            function(stdout, stderr, reason, exit_code)
                textclock.calendar_notification = naughty.notify{
                    text = string.gsub(
                                string.gsub(stdout, "27m", "</span>"),
                                "7m", "<span foreground='red'>"),
                    font = beautiful.mono_font,
                    timeout = 0,
                    width = auto,
                    destroy = function() textclock.calendar_notification = nil end
                }
            end
        )
    else
        naughty.destroy(textclock.calendar_notification)
    end
end

--- Create a textclock widget. It draws the time it is in a textbox.
--
-- @tparam[opt=" %a %b %d, %H:%M "] string format The time format.
-- @tparam[opt=60] number timeout How often update the time (in seconds).
function textclock.new(env, format, timeout)
    format = format or " %a %b %d, %H:%M "
    timeout = timeout or 60

    local w = textbox()
    local t
    function w._private.textclock_update_cb()
        w:set_markup(DateTime.new_now_local():format(format))
        t.timeout = timeout - os.time() % timeout
        t:again()
        return true -- Continue the timer
    end
    t = timer.weak_start_new(timeout, w._private.textclock_update_cb)
    t:emit_signal("timeout")

    w:connect_signal("button::release", function() show_calendar(env) end)

    return w
end

function textclock.mt:__call(...)
    return textclock.new(...)
end

return setmetatable(textclock, textclock.mt)
