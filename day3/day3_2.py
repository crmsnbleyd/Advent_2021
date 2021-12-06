def most_common_n(n, list):
    ret = ['0','1']
    freq1, freq0 = 0, 0
    for i in list:
        if i[1][n] == '1':
            freq1 += 1
        else:
            freq0 += 1
    return ret[freq1 >= freq0]

def reduce_to_largest(list):
    list = [*enumerate(list)]
    i = 0
    l = len(list[0][1])
    while (len(list)>1 and i < l):
        list = [(x,y) for (x,y) in list if y[i] == most_common_n(i, list)]
        i += 1
    return list[0]

def reduce_to_smallest(list):
    list = [*enumerate(list)]
    i = 0
    l = len(list[0][1])
    while (len(list)>1 and i < l):
        list = [(x,y) for (x,y) in list if y[i] != most_common_n(i, list)]
        i += 1
    return list[0]

with open ("3_1.txt", "r") as problem:
    lines = problem.readlines()
    print (len(lines[0]))
    print(reduce_to_smallest(lines))
    print(reduce_to_largest(lines))