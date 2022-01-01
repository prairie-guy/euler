# Euler 142
# Find the smallest x + y + z with integers x > y > z > 0 such that
# x + y, x − y, x + z, x − z, y + z, y − z are all perfect squares.
#
#

import numpy as np
from math import floor,sqrt, gcd
from collections import defaultdict

def odd(n):
    return (n%2) == 1

def perfect(n):
    sq = sqrt(n)
    return sq == floor(sq)

# def all_perfect(pt):
#     (z,y,x) = sorted(pt, reverse= True)
#     return perfect(z**2 - y**2) and perfect(z**2 - x**2) and  perfect(y**2 - x**2)

def all_perfect(x,y,z):
    (x,y,z) = sorted([x,y,z])
    return perfect(z+y) and perfect(z-y) and perfect(y+x) and perfect(y-x) and perfect(z+x) and perfect(z-x)


def all_perfect2(x,y,z):
    (x,y,z) = sorted([x,y,z])
    return perfect(z**2 - y**2) and perfect(z**2 - x**2) and perfect(y**2 - x**2) and all_perfect(x,y,z)

def pyth_trips(limit=None):
    u = np.mat(' 1  2  2; -2 -1 -2; 2 2 3')
    a = np.mat(' 1  2  2;  2  1  2; 2 2 3')
    d = np.mat('-1 -2 -2;  2  1  2; 2 2 3')
    uad = np.array([u, a, d])
    m = np.array([3, 4, 5])
    while m.size:
        m = m.reshape(-1, 3)
        if limit:
            m = m[m[:, 2] <= limit]
        yield from m
        m = np.dot(m, uad)

def all_pyth_trips(limit):
    for prim in pyth_trips(limit):
        i = prim
        for _ in range(limit//prim[2]):
            yield i
            i = i + prim
def m3(K):
    for (x,y,z) in all_pyth_trips(K):
        if all_perfect2(x,y,z):
            print(sum([x,y,z]),[x,y,z])


def m4(n):
    s = 20
    for m in range(3,n):
        if s < 0: break
        if perfect(m**4 - 6*m*m + 1):
            print(m,
                  [m**2 +1, 2*m, m**2 -1],
                  sum([m**2+1,2*m, m**2-1]))
            s = s - 1


def m5(k):
    s = 20
    for m in range(3,k):
        if s < 0: break
        for n in range(2,m-1):
            mn = m**4 + n**4 - 6 * m**2 * n**2
            #print(m,n,mn)
            if mn <0 or (odd(m) and odd(n)): break
            if perfect(mn):
                #if (n**2 -1) == 0: break
                print(m,n, [m**2 + n**2, m**2 - n**2, 2*m*n,],sum([m**2 + n**2, m**2 - n**2, 2*m*n,]))
                s = s - 1


def m6(N):
    for z in range(3,N):
        for x in range(1,z):
            y = z - x
            if y <= 1: break
            if perfect(z**2 - y**2) and perfect(z**2 - z**2) and perfect(y**2 - x**2):
                print([x,y,z], sum([x,y,z]))

#all_perfect
def m7(k):
    for z in range(3,k):
        for y in range(2,z):
            for x in range(1,y):
                if all_perfect(x,y,z):
                    print([x,y,z],[y-x,y+x], [z+y,z-y]  ,  sum([x,y,z]))


# all_perfect2
def m8(K):
    for x in range(1,K):
        for y in range(x+1,K):
            for z in range(y+1,K):
                if all_perfect2(x,y,z):
                    print([x,y,z],[y-x,y+x], [z+y,z-y]  ,  sum([x,y,z]))



# All keys
def make_pab0(K):
    pab = defaultdict(dict)
    for a in range(1,K):
        for b in range(a+1,K):
            if perfect(b+a) and perfect(b-a):
                pab[a][b] = [a,b]
    return pab


# Even keys and even values and key.values() > 2
# NOTE: I am not at all sur
def make_pab1(K):
    pab = defaultdict(dict)
    for a in range(2,K):
        for b in range(a+2,K,2):
            if perfect(b+a) and perfect(b-a):
                pab[a][b] = [a,b]
        if len(list(pab[a].values())) < 2: pab.pop(a)
    return pab



# Even keys key.values() > 2
def make_pab2(K):
    pab = defaultdict(dict)
    for a in range(2,K,2):
        for b in range(a+1,K):
            if perfect(b+a) and perfect(b-a):
                pab[a][b] = [a,b]
        if len(list(pab[a].values())) < 2: pab.pop(a)
    return pab

# Even keys key.values() > 2, MxN
def make_pab3(M,N):
    pab = defaultdict(dict)
    for a in range(2,M,2):
        for b in range(a+1,N):
            if perfect(b+a) and perfect(b-a):
                pab[a][b] = [a,b]
        if len(list(pab[a].values())) < 2: pab.pop(a)
    return pab

# Even keys key.values() > 2, (B,K), B < a < K
def make_pab4(B,K):
    B = floor(B/2) *2
    pab = defaultdict(dict)
    for a in range(B,K,2):
        for b in range(a+1,K):
            if perfect(b+a) and perfect(b-a):
                pab[a][b] = [a,b]
        if len(list(pab[a].values())) < 2: pab.pop(a)
    return pab


def print_pab(pab):
    for a in pab.keys():
        print(list(pab[a].values()))

def print_multiple_pab(pab):
    for a in pab.keys():
        if len(list(pab[a].values())) > 1:
            print(list(pab[a].values()))

def paired_ab(pab):
    for a in pab.keys():
        if len(pab[a].values()) > 1:
            pabs = list(pab[a].values())
            size = len(pabs)
            for i in range(0, size):
                for j  in range(i+1, size):
                    #print([pabs[i],pabs[j]])
                    a,b = pabs[i]
                    _,c = pabs[j]
                    #print(a,b,c)
                    if perfect(c+b) and perfect(c-b):
                        print(a,b,c)
