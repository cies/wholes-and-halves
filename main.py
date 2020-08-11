import difflib
from itertools import product


def evalWithoutHidden(hidden, turn):
    solver = difflib.SequenceMatcher(a=hidden, b=turn)
    solve = solver.find_longest_match(0, len(solver.a), 0, len(solver.b))
    wholes = solve.b
    halves = solve.a
    return wholes, halves

def allCombisWithRep():
    dig = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    combisWithRep = []
    for d in product(dig, repeat=4):
        combisWithRep.append(''.join(d))
    return combisWithRep

def allCombisWithoutRep():
    combisWithRep = allCombisWithRep()
    combisWithoutRep = []
    for combi in combisWithRep:
        if len(set(combi)) == len(combi):
            combisWithoutRep.append(combi)
    return combisWithoutRep

def evalPossibleWH (turnResult):
    combis = []
    for i in range(5):
        for j in range(5):
            if (i + j * 0.5 == turnResult) and (i + j <=4):
                combis.append([i,j])
    return combis

