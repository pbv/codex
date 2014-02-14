
import random

def media(x,y):
    return (x+y)/2


random.seed(0)
tsts = []
for n in range(50):   # 50 testes
    x = random.randint(-n,n)
    y = random.randint(-n,n)
    tsts.append((x,y))


for x,y in tsts:
    print(">>> round(media(%d,%d),4)" % (x,y))
    print(round(media(x,y),4))

