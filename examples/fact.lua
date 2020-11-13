local function factorial(n)
    if n < 1 then return 1 end
    return n * factorial(n - 1)
end

print(factorial(0))
print(factorial(1))
print(factorial(2))
print(factorial(3))
print(factorial(4))
print(factorial(5))
