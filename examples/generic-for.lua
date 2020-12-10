local function f(t, i)
    if i == nil then i = 0 end
    i = i + 1
    local c = t[i]
    if c ~= nil then return i, c end
end

local t = {'a', 'b', 'c'}
for a, b in f, t do
    print(a, b)
end
