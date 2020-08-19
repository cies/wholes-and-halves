import random
from itertools import product
from typing import NamedTuple


class BasicRes(NamedTuple):
    # The turn eval result in basic rules, wholes and halves separate
    wholes: int
    halves: int
    def toAdvanced(self):
        return AdvancedRes(self.wholes + (self.halves // 2), bool(self.halves % 2))
    def __repr__(self):
        return str(self.wholes) + "|" + str(self.halves)  # not a dot to show its basicness

class AdvancedRes(NamedTuple):
    # The turn eval result in the advanced rules, only one or zero halves
    wholes: int
    hasHalve: bool
    def toBasicResults(self):
        # Returns a list of Res following BASIC rules from float representation of the result (which follows ADVANCED rules).
        # `advancedRes` is a float.
        basicResults = []  # NOTE: Please do not overload the use of the word "combis" here
        for r in allPossibleBasicResults:
            if r.toAdvanced == self:  # Cheap test first
                basicResults.append(r)
        return basicResults
    def __repr__(self):
        return str(self.wholes) + "." + ("1" if self.hasHalve else "0")  # dot shows it's advanced

# Unused as of yet as it's require type juggling. For now just use tuples.
# class Combi(NamedTuple):
#    p1: int
#    p2: int
#    p3: int
#    p4: int

def calculateAllCombisWithRep():
    digs = list(range(10))
    combisWithRep = []
    for dig in product(digs, repeat = 4):
        combisWithRep.append(list(dig))
    return combisWithRep

allCombisWithRep = calculateAllCombisWithRep()

def calculateAllCombisWithoutRep():
    # Returns a list with all combis
    combisWithoutRep = []
    for combi in allCombisWithRep:
        if len(set(combi)) == len(combi):  # Optimizable but not very hot
            combisWithoutRep.append(combi)
    return combisWithoutRep

allCombisWithoutRep = calculateAllCombisWithoutRep()

def calculateAllPossibleBasicResults():
    # Carefully ordered list of all possible results in BASIC W&Hs rules:
    # Returns: [Res(0, 0, Res(0, 1), Res(0, 2), ..., Res(2, 2), Res(3, 0), Res(4, 0)]
    possibleResults = []  # list of Res
    for d in product([0, 1, 2, 3, 4], repeat = 2):
        if sum(d) <= 4 and list(d) != [3, 1]:
            possibleResults.append(BasicRes(d[0], d[1]))
    return possibleResults

allPossibleBasicResults = calculateAllPossibleBasicResults()
lenWeightResults = len(allPossibleBasicResults)

def evalWithHidden(hidden, turn):
    # Returns: a Res representing the result under BASIC W&Hs rules
    wholes = sum(len(set(i)) == 1 for i in zip(hidden, turn))  # Optimizable and likely very hot
    halves = len(set(hidden).intersection(turn)) - wholes
    if halves < 0:
        halves = 0
    return BasicRes(wholes, halves)

def weightResult(res):
    # Assigns weight based on result's position in list of allPossibleBasicResults.
    # Returns an `int`.
    weight = allPossibleBasicResults.index(res)  # This may be optimized, data structure not optimal for fast `index()`
    return weight

def uniqueElementsList(list):
    # Returns a list of unique elements
    uniqueElements = []
    for element in list:
        if element not in uniqueElements:
            uniqueElements.append(element)
    return uniqueElements

def nextOptionalCombis(turnCombi, turnAdvancedRes, possibleCombis):
    allNextTurnOptionalCombis = []
    allCheckedOptionalCombis = []
    possibleCombis.remove(turnCombi)
    lenPossibleCombis = len(possibleCombis)

    for basicResult in basicFromAdvanced(turnAdvancedRes):

        # List of combis that comparing with the turn_combi have the same result (wholes+halves*0.5) as the turn_result
        checkedOptionalCombis = [nOC for nOC in possibleCombis if basicResult == evalWithHidden(nOC, turnCombi)]
        allCheckedOptionalCombis = allCheckedOptionalCombis + checkedOptionalCombis

        firstMaxInList = 0

        for aPC in possibleCombis:
            weightCombisResult = [evalWithHidden(aPC, cPC) for cPC in checkedOptionalCombis]  # List of Res (BASIC rules)
            orderedWeightResult = [0] * lenWeightResults  # List of Weights for possible basic results [Res(0, 0, Res(0, 1), Res(0, 2), ..., Res(2, 2), Res(3, 0), Res(4, 0)]

            # Sum of weights for [Res(0, 0, Res(0, 1), Res(0, 2), ..., Res(2, 2), Res(3, 0), Res(4, 0)] in the turn
            for wCR in weightCombisResult:
                weight = weightResult(wCR)
                orderedWeightResult[weight] = orderedWeightResult[weight] + 1

            # Minimax to find a next guess
            minNoZeros = lenPossibleCombis - max(orderedWeightResult)
            if minNoZeros > firstMaxInList:
                firstMaxInList = minNoZeros
                allNextTurnOptionalCombis.append(aPC)

    uniqueNextTurnOptionalCombis = uniqueElementsList(allNextTurnOptionalCombis)
    uniqueCheckedOptionalCombis = uniqueElementsList(allCheckedOptionalCombis)

    return uniqueNextTurnOptionalCombis, uniqueCheckedOptionalCombis

    

if __name__ == '__main__':
    print("Here's allPossibleBasicResults", allPossibleBasicResults)

    print("Our allCombisWithoutRep count is", len(allCombisWithoutRep))
    # print(len(allCombisWithoutRep), allCombisWithoutRep)
    
    # print("Let's see allCombisWithRep...")
    # print(len(allCombisWithRep), allCombisWithRep)
    
    print("Now let's try solving one...")
    hiddenCombi = [5, 4, 1, 6]
    possibleCombis = allCombisWithoutRep
    for i in range(5):  # while True:  # while loop gets stuck now, hence the for loop
        pickedTurnCombi = possibleCombis[0]
        turnBasicRes = evalWithHidden(hiddenCombi, pickedTurnCombi)
        turnAdvancedRes = advancedFromBasic(turnBasicRes)
        print("Picked turn is:",''.join(str(pickedTurnCombi)),"; the result of the turn is: ",turnBasicRes," that equals to ",str(turnAdvancedRes))
        possibleCombis, possibleCombis = nextOptionalCombis(pickedTurnCombi, turnAdvancedRes, possibleCombis)
        print("Possible combis are:",''.join(str(possibleCombis)))
        if turnBasicRes == Res(4, 0):
            print("Got it!")
            break
        if pickedTurnCombi == []:
            print("Oops, no more options... (error)")
            break
