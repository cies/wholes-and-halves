import random
from itertools import product


def evalWithHidden(hidden, turn):
    wholes = sum(len(set(i)) == 1 for i in zip(hidden,turn))
    halves = len(set(hidden).intersection(turn)) - wholes
    return wholes, halves

def allCombisWithRep():
    dig = list(range(10))
    combisWithRep = []
    for d in product(dig, repeat = 4):
        combisWithRep.append(d)
    return combisWithRep

def allCombisWithoutRep():
    combisWithRep = allCombisWithRep()
    combisWithoutRep = []
    for combi in combisWithRep:
        if len(set(combi)) == len(combi):
            combisWithoutRep.append(combi)
    return combisWithoutRep

def allPossibleResults():
    res = list(range(5))
    possibleResults = []
    for d in product(res, repeat = 2):
        if sum(d) <= 4:
            possibleResults.append(d)
    return possibleResults
    
def evalPossibleWH (turnResult):
    combis = []
    for i in range(5):
        for j in range(5):
            if (i + j * 0.5 == turnResult) and (i + j <=4):
                combis.append([i,j])
    return combis