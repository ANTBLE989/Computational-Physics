a = [0,2]
b = [5,1]

mat = [[4,5,3,-4,3,8], 
 [2,0,6,5,4,9], 
 [7,5,6,8,2,6], 
 [1,4,7,8,9,11], 
 [1,3,10,5,5,1], 
 [12,7,3,4,4,3]]

x = list()
d = dict()

for elem in mat:
    for j in elem:
        x.append(j)

for c in x:
    if c not in d:
        d[c] = 1
    else:
        d[c] += 1
        
num = d.keys()
num.sort()

#if zero exist
lnum = 0
for k in range(len(num)-1):
    if num[k+1] > num[k]:
        lnum = num[k]
    if lnum - num[len(num)] == 1:
        lnum = num[len(num)]+1
#if middle number exist

#if middle number is missing 


