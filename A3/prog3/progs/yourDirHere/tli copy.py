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

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self, op1, operator, op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1
        else:
            return self.op1 + " " + self.operator + " " + self.op2

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        op1 = self.op1
        op2 = self.op2

        if is_number(str(op1)):
            symTable[op1] = float(op1)
        if is_number(str(op2)):
            symTable[op2] = float(op2)

        if self.operator == "var":
            return symTable[op1]
        elif self.operator == "string":
            op1 = str(op1.strip('"'))
            return op1
        elif self.operator == "constant":
            return op1
        elif self.operator == "+":
            return symTable[op1] + symTable[op2]
        elif self.operator == "-":
            return symTable[op1] - symTable[op2]
        elif self.operator == "*":
            return symTable[op1] * symTable[op2]
        elif self.operator == "/":
            return symTable[op1] / symTable[op2]
        elif self.operator == "<":
            return symTable[op1] < symTable[op2]
        elif self.operator == ">":
            return symTable[op1] > symTable[op2]
        elif self.operator == "<=":
            return symTable[op1] <= symTable[op2]
        elif self.operator == ">=":
            return symTable[op1] >= symTable[op2]
        elif self.operator == "==":
            return symTable[op1] == symTable[op2]
        elif self.operator == "!=":
            return symTable[op1] != symTable[op2]
        else:
            print('Syntax error on line {}.'.format(symTable["sListLineCount"] + 1))
            return sys.exit()

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
        exprs = self.exprs
        if self.keyword == 'let':
            tempExprs = ""
            if len(exprs) > 3:
                tempExprs = exprs[2]
                internal = Expr(exprs[2], exprs[3], exprs[4]).eval(symTable)
                exprs[2] = str(internal)

            if str(exprs[2]).isdigit():
                symTable[exprs[0]] = float(exprs[2])
            else:
                while not is_number(str(exprs[2])):
                    exprs[2] = symTable[exprs[2]]

                if is_number(str(exprs[2])):
                    symTable[exprs[0]] = float(exprs[2])
            if len(tempExprs) > 0:
                exprs[2] = tempExprs
        # Print
        elif self.keyword == 'print':
            finalPrint = ""
            for index, expressions in enumerate(exprs):
                expressionsList = expressions.split()
                if len(expressionsList) == 3:
                    evaluatedExpression = Expr(expressionsList[0],expressionsList[1],
                                               expressionsList[2]).eval(symTable)
                    symTable[expressions] = str(evaluatedExpression)

            for index,expressions in enumerate(exprs):
                if exprs[index] in symTable:
                    evaluatedExpression = Expr(exprs[index],'var').eval(symTable)
                elif exprs[index] not in symTable and '"' in exprs[index]:
                    evaluatedExpression = Expr(exprs[index],'string').eval(symTable)
                elif exprs[index] not in symTable and '"' not in exprs[index]:
                    evaluatedExpression = Expr(exprs[index],'constant').eval(symTable)
                finalPrint += str(evaluatedExpression) + " "

            print(finalPrint[:-1])
        # Input
        elif self.keyword == 'input':
            for expressions in exprs:
                newInput = input()
                if str(newInput).isdigit():
                    newInput = float(newInput)
                    symTable[expressions] = newInput
                else:
                    sys.exit('Illegal or missing input')
        # Goto:
        # While reading in strings into statements during main, we check if a word is a label
        # If it is, then the word is added into the symbol table along with the line number
        # As we perform if, we know that the first 3 expressions will be the actual expression
        # that we need to evaluate to see if the if statement is true or false. Since our third
        # expression is always 'goto' we can just ignore it with our evaluations.
        #
        # So, line by line this is what we perform:
        elif self.keyword == 'if':
            # take the label that we will be jumping to
            label = exprs[4]
            # evaluate the expression, if true, then we change which line we should be on using
            # our value "sListLineCount" that we saved into the symbol table ( this tells us
            # which statement line we are currently evaluating during perform). If our expression
            #  is true then this line number changes to the labeled line, otherwise,
            # it just iterates up one and continues to the next line
            if Expr(exprs[0], exprs[1], exprs[2]).eval(symTable):
                symTable["sListLineCount"] = int(symTable.get(label))
            else:
                symTable["sListLineCount"] = int(symTable["sListLineCount"] + 1)

        # since we are using "sListLineCount" to actually read which statements we are about to
        # perform, if we haven't used an if statement, then we just iterate up one
        if self.keyword != 'if':
            symTable["sListLineCount"] = int(symTable["sListLineCount"] + 1)


def main():

    symTable = {}
    symTable["sList"] = []
    # "sListLineCount" keeps track of which index in our list of statements we are currently
    # performing.
    # Ex: if 5 statements, sListLineCount will start at 0, then finish at 4
    symTable["sListLineCount"] = 0
    sList = symTable["sList"]

    # just a counter so we can see what line we're on when we throw errors
    lineCount = 0

    # Takes in our .txt file, and for each line in the file, we run the for loop:
    for line in fileinput.input(sys.argv[1:]):

        # lines.strip() takes the line as a string and throws it into a list called "lines"
        lines = line.strip()

        # takes the lines and turns them into a list of each string
        # ie let x = 5 becomes ['let', 'x', '=', '5']
        stringList = lines.split()

        if ':' in lines:
            symTable[stringList[0].strip(":")] = lineCount
            stringList.remove(stringList[0])

        # sets keyword to the first item in our stringList list
        keyword = stringList[0]

        # creates an empty array that we will fill in depending on the length of the expression
        expression = []
        # creates an array of legal keywords
        legalKeywords = ["let", "if", "print", "input"]

        # check to make sure keyword is spelled correctly
        if keyword not in legalKeywords:
            print('Syntax error on line {}.'.format(lineCount + 1))
            sys.exit()

        # now that we know the keyword, we get rid of it
        stringList.remove(stringList[0])

        # check if its a print line, if it is, adjust so prints properly\
        if keyword == "print":
            printExprs = ""
            for strings in stringList:
                printExprs += strings + " "

            printExprs = printExprs[:-1]
            printExprs = printExprs.split(' , ')
            for expressions in printExprs:
                expression.append(expressions)
        else:
            # now we add the rest of the strings into expression
            for strings in stringList:
                expression.append(strings)

        # our statement has been built, so now we add it to the list of statements
        statement = Stmt(keyword, expression)
        sList.append(statement)

        lineCount += 1

    count = 0
    while count < len(symTable["sList"]):

        sListLineCount = symTable["sListLineCount"]
        sList[sListLineCount].perform(symTable)
        count = int(sListLineCount) + 1


if __name__ == "__main__":
    main()

