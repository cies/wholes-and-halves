import random
from itertools import product


def evalWithHidden(hidden, turn):
    wholes = sum(len(set(i)) == 1 for i in zip(hidden,turn))
    halves = len(set(hidden).intersection(turn)) - wholes
    if halves < 0:
        halves = 0
    return wholes, halves

def allCombisWithRep():
    dig = list(range(10))
    combisWithRep = []
    for d in product(dig, repeat = 4):
        combisWithRep.append(list(d))
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
        if sum(d) <= 4 and list(d) != [3, 1]:
            possibleResults.append(list(d))
    return possibleResults

def weightPossibleResult(res):
    weight = allPossibleResults().index(res)
    return weight
    
def evalPossibleWH (turnResult):
    combis = []
    for i in range(5):
        for j in range(5):
            if (i + j * 0.5 == turnResult) and (i + j <=4):
                combis.append([i,j])
    return combis

def nextOptionalCombis(hidden, turn, allPossibleCombis):
    cleanedPossibleCombis = allPossibleCombis
    combiT = turn["combi"]
    resultT = turn["result"]

    possibleWH = evalPossibleWH (resultT)
    allNextTurnOptionalCombis = []
    uniqueNextTurnOptionalCombis = []
    
    for wh in possibleWH:
      wholesTurn = wh[0]
      halvesTurn = wh[1]

      optionalCombis = []
      checkedOptionalCombis = []
          
      firstMaxInList=0
          
      for c in allPossibleCombis:
          wholesOp, halvesOp = evalWithHidden(c,combiT)
          if wholesTurn == wholesOp and halvesTurn == halvesOp:
              checkedOptionalCombis.append(c)
          
      for nOC in cleanedPossibleCombis:
          if nOC in checkedOptionalCombis:           
              optionalCombis.append(nOC)
              
      cleanedPossibleCombis=optionalCombis

      for aPC in allPossibleCombis: 
          weightCombisResult = []
          orderedWeightResult = [0] * len(allPossibleResults())
              
          for cPC in cleanedPossibleCombis: 
              weightCombisResult.append(evalWithHidden(aPC,cPC))
      
          for wCR in weightCombisResult:
              i = weightPossibleResult(list(wCR))
              orderedWeightResult[i] = orderedWeightResult[i] + 1
                  
          wNoZeros = len(allPossibleCombis) - max(orderedWeightResult)
          if wNoZeros > firstMaxInList:
              firstMaxInList = wNoZeros
              allNextTurnOptionalCombis.append(aPC)
    
    for oC in allNextTurnOptionalCombis: 
      if oC not in uniqueNextTurnOptionalCombis: 
        uniqueNextTurnOptionalCombis.append(oC) 

    return uniqueNextTurnOptionalCombis
    

if __name__ == '__main__':
    hidden = [5, 4, 1, 2]
    allPossibleCombis = allCombisWithoutRep()
    
    turns = [
            { "combi": [0,1,2,3], "result": 1 }
            #{ "combi": [1,2,3,4], "result": 1,5 },
            #{ "combi": [5,6,4,2], "result": 2 }
            ]
    
    for turn in turns:
        print(nextOptionalCombis(hidden, turn, allPossibleCombis))