local function f()
    -- Test
    local total = 0
    for i = 1, 100 do
        if i % 2 == 0 then
            total = total + i/2.0
        else
            total = total + i
        end
    end
    return 1e-1
end

-- Hello, world!
--
--[[
--
--]]

--[=[
--
]=]

print('\abcdefg\'');
print("\abcdefg\"");
print('\abcdefg\"');
print("\abcdefg\'");
