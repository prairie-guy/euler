using Primes, Lazy

R(k) = div(BigInt(10)^k - 1 ,9)

ds = @>> Lazy.range(2) filter(x->gcd(x,10)==1);

function k_over(goal)
    d_,r_= (0,0)
    for d in dropwhile(d-> d<=goal, ds)
        println("($d_, $r_) $(isprime(d) ? "*" : "")")
        r_ > goal && return(d_,r_)
        @time for r in 2:d
            R_ = R(r)
            if R_%d == 0
                d_,r_ = d,r
                break
            end
        end
    end
    return("nothing")
end




