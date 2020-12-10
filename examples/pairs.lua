local t = {'a', 'b', 'c'}
print(pairs(t))
print(pairs(t))
local iter, table, Nil = pairs(t)
print(iter, table, Nil)
print(iter(t))
print(iter(t, 1))
print(iter(t, 2))
print(iter(t, 3))
print(iter(t, 4))
for a, b in pairs(t) do
    print(a, b)
end
