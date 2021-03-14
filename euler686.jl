# floor(128 / 10^(floor(log10(128)) - 1)) = 12

function is_leading(n, digits)
    ds  = floor(log10(digits))
    lgn = n*log10(2)
    digits == floor(10^(lgn - floor(lgn) + ds))
end


function get_powers_of_two(digits,n)
    twos = []
    for n=1:n
        if is_leading(n,digits)
            push!(twos,n)
        end
    end
    twos
end

# julia> @time n_123 = get_powers_of_two(123,5*10^8)
# 6.362069 seconds (1.76 M allocations: 43.830 MiB, 0.05% gc time)
# julia> print(n_123[678910])
# 193060223
