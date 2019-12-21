n ="7970521.5544"
nw = ""
nd = ""

if n.find(".") >= 0:
    nw = n[0:n.find(".")]
    nd = n[n.find(".")+1:len(n)]
else:
    nw = n
    nd = 0
    
x = list()
y = list()
l = list()
d1 = 0
for i in range(len(nw)+1):
    num1 = int(nw) % 10**(i+1) - d1
    if num1 != 0:
        x.append(num1)
    d1 = int(nw) % 10**(i+1)

d2 = 0

if int(nd) != 0:
    for i in range(len(nd)+1):
        num2 = (float(nd) % 10**(i+1) - d2)
        if num2 != 0:
            x.append(num2/10**len(nd))
        d2 = float(nd) % 10**(i+1)
    
x.sort()
x.reverse()

for k in range(len(x)):
    if x[k] < 1:
        s = str(x[k])
        l.append(s[1:len(s)])
    else:
        l.append(str(x[k]))


