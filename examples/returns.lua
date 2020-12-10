local function foobar()
    return "foo", "bar"
end

local foo, bar = foobar()
print(foo)
print(bar)

bar, foo = foobar()
print(foo)
print(bar)

print(foobar())
