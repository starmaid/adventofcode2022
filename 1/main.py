import copy
import numpy as np
import os
import math as m

os.chdir("1")

with open("input", 'r') as f:
    d = f.readlines()
    data = [0]
    n = 0
    for i in d:
        i = i.strip()
        if i == '':
            data.append(0)
            n = n + 1
        else:
            data[n] = data[n] + int(i)

print(data)
print(max(data))

#part 2

data = sorted(data,reverse=True)
print(sum(data[0:3]))