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

def weightPossibleResult(res):
    weight = allPossibleResults().index(res) - 1
    return weight
    
def evalPossibleWH (turnResult):
    combis = []
    for i in range(5):
        for j in range(5):
            if (i + j * 0.5 == turnResult) and (i + j <=4):
                combis.append([i,j])
    return combis


if __name__ == '__main__':
    
    #hidden = random.choice(allCombisWithoutRep())
    #allPossibleCombis = allCombisWithoutRep()
    hidden = [5,4,1,2]
    allPossibleCombis = [(1,2,3,4), (1,2,4,5), (1,2,8,9), (6,7,8,9), (5,4,1,2)]
    optionalCombis = []
    nextOptionalCombis=[]
    
    weightCombis = []
    
    turns = [
            { "combi": [0,1,2,3], "wholes": 0, "halves": 2 },
            { "combi": [1,2,3,4], "wholes": 0, "halves": 3 },
            { "combi": [5,6,4,2], "wholes": 1, "halves": 2 }
            ]
    
    for t in turns:
        for c in allPossibleCombis:
            wholes, halves = evalWithHidden(t["combi"], c)
            if wholes == t["wholes"] and halves == t["halves"]:
                optionalCombis.append(c)
    
    optionalCombis = list(dict.fromkeys(optionalCombis))
    
    for oC in allPossibleCombis:
        if oC in optionalCombis:           
            weightCombis.append(oC)
    
    #optionalCombis = weightCombis
    
    for pC1 in optionalCombis:
        weightCombisResult = []
        
        for pC2 in optionalCombis: 
            weightCombisResult.append(evalWithHidden(pC1,pC2))
        orderedWeightResult = [0] * (len(allPossibleResults()) - 1)
        firstMax = 0
        
        for w in weightCombisResult:
            wPR = int(weightPossibleResult(w))
            orderedWeightResult[wPR-1] = orderedWeightResult[wPR] + 1
        
        wNoZeros = len(allPossibleCombis) - max(orderedWeightResult) 
    
        if wNoZeros > firstMax:
            firstMax = wNoZeros
            nextOptionalCombis = pC1
      
    print(nextOptionalCombis)