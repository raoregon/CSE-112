#! /usr/bin/env python3
"""
//assume in the command prompt the user has entered the argument, tl.txt, the name of
//the text file that contains a TL program in it
create sList, an empty list of Stmt objects
create symTable, an empty symbol table
for each line of statement in tl.txt:
    parse the line to an Stmt object //possibly with satellite Expr objects
    add the Stmt object to sList
    if this line is labeled:
        add (label, current line number) mapping to symTable
evaluate sList with symTable
"""
import fileinput
import sys

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
        op1 = self.op1
        op2 = self.op2

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
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable):
        print ("Doing: " + str(self))

        exprs = self.exprs
        print("exprs:")
        print(exprs)

        if self.keyword == 'let':
            if len(exprs) > 3:
                internal = Expr(exprs[2], exprs[3], exprs[4]).eval(symTable)
                exprs[2] = str(internal)

            # This a digit, right? if not, set w/e is in exprs[0] to this new variable
            print(exprs[2].isdigit())

            if exprs[2].isdigit():
                symTable[exprs[0]] = float(exprs[2])
            else:
                symTable[exprs[0]] = exprs[2]

        print("SymbolTable:")
        print(symTable)
        print(exprs)

        if self.keyword == 'print':
            print(symTable[exprs[0]])

# just a counter so we can see what line we're on when we throw errors
lineCount = 0

# Takes in our .txt file, and for each line in the file, we run the for loop:
for line in fileinput.input(sys.argv[1:]):

    # lines.strip() takes the line as a string and removes the whitespace and throws it into a
    # list called "lines"
    lines = line.strip()

    if 'let' in lines:
        # takes the lines and turns them into a list of each string
        # ie let x = 5 becomes ['let', 'x', '=', '5']
        stringList = lines.split()

        # sets keyword to the first item in our stringList list
        keyword = stringList[0]

        # creates an empty array that we will fill in depending on the length of the expression
        expression = []

        # check to make sure keyword is spelled correctly
        if keyword != "let":
            sys.exit('Syntax error on line {}'.format(lineCount))

        # now that we know the keyword, we get rid of it
        stringList.remove(stringList[0])

        # now we add the rest of the strings into expression
        for strings in stringList:
            expression.append(strings)

        # our statement has been built, so now we add it to the list of statements
        statement = Stmt(keyword, expression)
        sList.append(statement)

    elif 'print' in lines:
        stringList = lines.split()
        keyword = stringList[0]
        expression = []

        stringList.remove(stringList[0])

        for strings in stringList:
            expression.append(strings)

        statement = Stmt(keyword, expression)
        sList.append(statement)

    lineCount += 1

for statements in sList:
    statements.perform(symTable)

