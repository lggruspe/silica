local function foo()
    local message = "foo"
    print(message)
end

local function bar()
    local message = "bar"
    print(message)
end

local function foobarbaz()
    local message = "baz"
    foo()
    bar()
    print(message)
end

foobarbaz()
