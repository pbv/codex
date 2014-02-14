
import random
import math


def raizes(a,b,c):
    delta = b*b-4*a*c
    x1 = (-b+math.sqrt(delta))/(2*a)
    x2 = (-b-math.sqrt(delta))/(2*a)
    return [x1,x2]

random.seed(0)

tsts = []
n = 0
while n<50:
    a = random.randint(1, n+1)
    b = random.randint(-n,n)
    c = random.randint(-n,n)
    delta = b*b-4*a*c
    if delta < 0:   # sem solução
        continue
    tsts.append((a,b,c))
    n = n+1



for a,b,c in tsts:
    print(">>> [x1,x2]=raizes(%d,%d,%d); print(round(x1,6), round(x2,6))"%(a,b,c))
    [x1,x2]=raizes(a,b,c); print(round(x1,6), round(x2,6))
