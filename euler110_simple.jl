import Primes

primes_local = primes(10000);
ppp(n) = reduce(*, big(1), primes_local[1:n])

n_divisors(n) = prod(e + big(1) for (p, e) in factor(n))
n_diophantine_reciprocals(n) = div(n_divisors(n^2) + 1, 2)

function search(n)
    for i = 2:10000
        if n_diophantine_reciprocals(n*i)  > 4000000
            return n*i
        end
    end
    Inf
end

function euler110()
    minimum([search(ppp(i)) for i in 12:14])
end
