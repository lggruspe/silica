local t = table.pack(1, 2, 3, 4, 5)
print(t, t.n)
for i, a in pairs(t) do
    print(i, a)
end


print(table.concat({'a', 'b', 'c'}, ', '))
