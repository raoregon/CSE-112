import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// Create a class called Expr
class Expr(op1: Any, operator: Any, op2: Any){
	override def toString = op1 + " " + operator + " " + op2
	
	def eval(symTable: Map[Any,Any]) :Any = {
		
		if (operator == "var"){
			val result = symTable(op1)
			return result
		} else if (operator == "+"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first + second
			return result
		}
	}
}
//hide given case classes
//case class Var(name: String) extends Expr
//case class Str(name: String) extends Expr
//case class Constant(num: Double) extends Expr
//case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

// Create a class called Stmt
class Stmt(keyword: String, exprs: Array[String]){

	def copy(newKeyword: String = keyword, newExprs: Array[String] = exprs): Stmt =
		new Stmt(newKeyword, newExprs)
		
	var others = ""
	exprs.foreach(exp => others = others + " " + exp.mkString(" "))
	
	override def toString = keyword + others
	
	def perform(symTable: Map[Any,Any]){
		
		println("keyword: " + keyword )
		println("exprs: " + exprs.mkString(" "))
		
		if (keyword == "let"){
			var tempExprs = ""
			var test = exprs(2).toDouble
			symTable.addOne(exprs(0) -> test)
		}else if (keyword == "print"){
			var finalPrint = ""
			var evaluatedExpression = new Expr(exprs(0),"var",None).eval(symTable)
			finalPrint = finalPrint + evaluatedExpression.toString()
			
			println(finalPrint)
			
		}
		var updateCount = symTable("sListLineCount").asInstanceOf[Integer]
		updateCount += 1
		symTable.update("sListLineCount", updateCount)
	}
}
//hide given caseclasses
//case class Let(variable: String, expr: Expr) extends Stmt
//case class If(expr: Expr, label: String) extends Stmt
//case class Input(variable: String) extends Stmt
//case class Print(exprList: List[Expr]) extends Stmt

object TLI {
	//hiding given eval
    //def eval(expr: Expr, symTab: Map[String, Double]): Double = expr match {
    //    case BinOp("+",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
    //    case Var(name) => symTab(name)
    //    case Constant(num) => num
	//case _ => 0 // should really throw an error
    //}

    def main(args: Array[String]) {
    	// symbolTable dictionary
    	var symTable:Map[Any,Any] = Map()
    	
    	// sList  is our list of Statements
    	var sList = Array[Stmt]()
    	
    	// just a counter so we can see what line we're on when we throw errors
    	var lineCount = 0
    	
    	// keeps track of which index in our list of statements we are currently
    	// performing.
    	symTable.addOne("sListLineCount" -> 0)
    	
    	// Takes in our .txt file, and for each line in the file, we run the for loop:
    	var filename = "test.txt"
    	
		for (line <- Source.fromFile(filename).getLines) {
    		// read in each line and remove all blank spaces and drop in ","
    		var afterLine = line.replace(" ", ",")
    		
    		// create a an empty list
    		var stringList = Array[String]()
    		
    		// add in the strings from afterLine but separating elements whenver it sees ","
    		stringList = afterLine.split(",").map(_.toString).distinct
    		
    		// set keyword to the first element in stringList
    		var keyword = stringList(0)
    		
    		// create an array called expression
    		var expression = Array[String]()
    		
    		// create an array called legalKeywords, they have all valid keywords
    		var legalKeywords = Array("let", "if", "print", "input")
    		
    		// create a boolean of whether or not keyword is in the array of keywords
    		val res = legalKeywords.contains(keyword)
    		
    		// if boolean res does not equal true, print an error
    		if (res == false) {
    			println("Syntax error on line " + (lineCount + 1) + ".")
    			System.exit(0)
    		}

			// remove keyword from stringList and set it to a new array called stringLists
    		var stringLists = stringList.filter(! _.contains(keyword))
    		println(stringLists.mkString(" "))
    		
    		//if (keyword == "print") {
    		//	var printExprs = ""
    		//	stringLists.foreach(strings => printExprs += strings + " " )
    		//	
    		//	var printExprsAfter = printExprs.split(",").map(_.toString).distinct
    		//	
    		//	println("so exprs is: ")
    		//	println(printExprsAfter.mkString(" "))
    		//}
    		
    		stringLists.foreach(strings => expression = expression:+ strings)
    		
    		println("expressions are: " + expression.mkString(" "))
    		
    		println("expression: " + expression.mkString(" "))
			println("keyword: " + keyword)
			var statement = new Stmt(keyword, expression)
			
			sList = sList:+ statement
			
    		lineCount += 1
		}
    	symTable.addOne("sList" -> sList)
    	var count = 0
    	var sListLen = sList.size
    	
    	
    	while (count < sListLen){
    		var sListLineCount = symTable("sListLineCount").asInstanceOf[Integer]
    		sList(sListLineCount).perform(symTable)
    		count = sListLineCount + 1
    	}
    }
}
