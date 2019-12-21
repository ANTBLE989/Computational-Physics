def DifficultGame(N, S):
    if N == len(S):
        for i in S:
            print i
            for k in range(i):
                print k
                if k % 2 == 0:
                    w = "First"
                else:
                    w = "Second"
    else: 
        w = "null"
    return(w)

