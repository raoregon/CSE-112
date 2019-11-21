#! /usr/bin/env python3
import fileinput
import sys
import numbers

sList = []
symTable = {}

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self,op1,operator,op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1
        else:
            return self.op1 + " " + self.operator + " " +  self.op2

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        print("op:")
        op1 = self.op1
        op2 = self.op2

        print(op1)
        print(op2)

        if self.operator == "var":
            return symTable[op1]
        elif self.operator == "+":
            return symTable[op1] + symTable[op2]
        else:
            return 0

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs):
        self.keyword = keyword
        self.exprs = exprs

    def __str__(self):
        others = ""
        for exp in self.exprs:
            if exp.isdigit():
                print("FUCK")
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable):
        print ("Doing: " + str(self))

        exprsList = []
        exprs = self.exprs
        print("exprs:")
        print(exprs)
        if keyword == 'let':
            if len(exprs) > 3:
                internal = Expr(exprs[2], exprs[3], exprs[4]).eval(symTable)
                print(internal)
            print(exprs[2])
            exprs[2]
            print("Digit:")
            print(exprs[2])
            print(exprs[2].isdigit())
            if exprs[2].isdigit():
                print("yesssss")
                symTable[exprs[0]] = int(exprs[2])
            else:
                print("no")
                symTable[exprs[0]] = exprs[2]

        print("SymbolTable:")
        print(symTable)
        print(self.exprs)

        print(exprs)
        expr = Expr(exprs[0], exprs[1], exprs[2])


lineCount = 0
for line in fileinput.input(sys.argv[1:]):
    lines = line.strip()
    if 'let' in lines:
        stringList = lines.split()
        keyword = stringList[0]
        expression = []

        # check to make sure keyword is spelled correctly
        if keyword != "let":
            raise Exception('Syntax error on line {}'.format(lineCount))

        stringList.remove(stringList[0])
        for strings in stringList:
            expression.append(strings)
        statement = Stmt(keyword, expression)
        sList.append(statement)

    lineCount += 1

for listItems in sList:
    print listItems.perform(symTable)
