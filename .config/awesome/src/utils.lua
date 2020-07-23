local io = io
local assert = assert

local awful = require("awful")

local utils = {}

function utils.file_write(path, data)
    local file = io.open(path, "w")

    if file then
        file:write(data)
        file:close()

        return true
    end

    return false
end

function utils.file_read(path)
    local file = io.open(path)

    if file then
        output = file:read("*a")
        file:close()
    else
        return nil
    end

    return output
end

function utils.table_merge(t1, t2)
    local ret = awful.util.table.clone(t1)

    for k, v in pairs(t2) do
        if type(v) == "table" and ret[k] and type(ret[k]) == "table" then
            ret[k] = table_.merge(ret[k], v)
        else
            ret[k] = v
        end
    end

    return ret
end

function utils.table_check(t, s)
    local v = t

    for key in string.gmatch(s, "([^%.]+)(%.?)") do
        if v[key] then
            v = v[key]
        else
            return nil
        end
    end

    return v
end

return utils
