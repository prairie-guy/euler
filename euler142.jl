# Euler 142
# Find the smallest x + y + z with integers x > y > z > 0 such that
# x + y, x − y, x + z, x − z, y + z, y − z are all perfect squares.
#
using Chain

function perfect(n)
    sq = sqrt(n)
    sq == floor(sq)
end


function pairs(N)
    p=[]
    for x = N:-1:1
        for y = x-1:-1:1
            if perfect(x+y) && perfect(x-y)
                push!(p,[x,y])
            end
        end
    end
    p
end

function pairs2(N)
    p=[]
    for x = N:-1:1
        for y = x-1:-1:1
            if perfect(x^2 - y^2) && perfect(x+y) && perfect(x-y)
                push!(p,[x,y])
            end
        end
    end
    p
end

function squares(n)
    p=[]
    for x = n-1:-1:floor((n+1)/2)
        y = n - x
        if perfect(x+y) && perfect(x-y) && x != y
            push!(p, [n, [x,y], (x-y), sqrt(x-y)])
        end
    end
    p
end



function doubles(N)
    m = []
    for x = N:-1:1
        for y = x-1:-1:1
            for z = y-1:-1:1
                if perfect(x+y) && perfect(x-y) &&
                    perfect(y+z) && perfect(y-z)
                    push!(m,[[x,y,z], [x^2-y^2, y^2-z^2]])
                end
            end
        end
    end
    m
end

function is_xz(xyz)
    x = xyz[1]
    z = xyz[3]
    perfect(x+z) && perfect(x-z)
end

function triples(N)
    for xyz = doubles(N)
        if is_xz(xyz)
            print(xyz)
        end
    end
end


# import numpy as np
# def gen_prim_pyth_trips(limit=None):
#     u = np.mat(' 1  2  2; -2 -1 -2; 2 2 3')
#     a = np.mat(' 1  2  2;  2  1  2; 2 2 3')
#     d = np.mat('-1 -2 -2;  2  1  2; 2 2 3')
#     uad = np.array([u, a, d])
#     m = np.array([3, 4, 5])
#     while m.size:
#         m = m.reshape(-1, 3)
#         if limit:
#             m = m[m[:, 2] <= limit]
#         yield from m
#             m = np.dot(m, uad)

using using ResumableFunctions

function prim_pyth_trips(limit=None):
    # u = [[1,  2,  2], [-2, -1, -2], [2, 2, 3]]
    # a = [[1,  2,  2], [2,  1,  2], [2, 2, 3]]
    # d = [[-1, -2, -2], [2,  1,  2], [2, 2, 3]]

    u = [1  2  2; -2 -1 -2; 2 2 3]
    a = [1  2  2; 2  1  2; 2 2 3]
    d = [-1 -2 -2; 2  1  2; 2 2 3]
    uad = [u;;;a;;;d]
    m = [3 4 5]
    while m.size:
        m = m.reshape(-1, 3)
        if limit:
            m = m[m[:, 2] <= limit]
        yield from m
            m = np.dot(m, uad)
