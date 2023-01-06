import scala.collection.{mutable => m}
import scala.util.control.Breaks._

object ShuntingYard {

  /**
   * Tells whether a character is an operator.
   */
  private def isOperator(c: Char): Boolean = {
    c match {
      case '*' | '+' | '/' | '^' | '-' | 'm' => true
      case _ => false
    }
  }

  /**
   * Given a string, it refines it by removing whitespace and replacing unary minus operator with 'm'
   */
  private def refine(str: String): String = {
    val newStr = str.replace(" ", "")
    val res = new StringBuilder

    // Refine with unary '-' as m
    for (i <- 0 until newStr.length) {
      if (i == 0 && newStr.charAt(0) == '-') {
        res += 'm'
      } else if (isOperator(newStr.charAt(i)) && !newStr.charAt(i - 1).isDigit && newStr.charAt(i - 1) != ')') {
        // Checks for unary operator
        if (newStr.charAt(i) == '-') {
          res += 'm'
        } else {
          throw new Exception("Incorrect Equation Syntax")
        }
      } else {
        res += newStr.charAt(i)
      }
    }

    res.toString
  }

  /**
   * Retrives the index where the the number ends
   */
  def getNumber(start: Int, str: String): Int = {
    var curr = start

    breakable {
      while (str.charAt(curr).isDigit) {
        curr += 1
        if (curr >= str.length) {
          break
        }
      }
    }

    curr
  }

  val rules: Map[String, (Int, String)] = Map.apply(
    "+" -> (0, "left"),
    "-" -> (0, "left"),
    "/" -> (5, "left"),
    "*" -> (5, "left"),
    "^" -> (10, "right"),
  )

  def getPrec(c: Char): Int = {
    val (p, _) = rules.getOrElse(c.toString, (0, ""))

    p
  }

  def checkAssoc(c: Char, asoc: String): Boolean = {
    val (_, a) = rules.getOrElse(c.toString, (0, ""))

    a.equals(asoc)
  }


  def infixToPostfix(tokens: String): m.Queue[String] = {
    val output = m.Queue[String]()
    val stack = m.Stack[String]()

    for (i <- 0 until tokens.length) {

      val token = tokens.charAt(i)

      // If it a digit
      if (token.isDigit) {

        val num = tokens.substring(i, getNumber(i, tokens))
        output.enqueue(num)

      } else if (isOperator(token)) {

        // It is an operator
        while (
          (stack.nonEmpty && isOperator(stack.top.charAt(0))) &&
            (
              (checkAssoc(token, "left") && getPrec(token) <= getPrec(stack.top.charAt(0))) ||
                (checkAssoc(token, "right") && getPrec(token) < getPrec(stack.top.charAt(0)))

              )) {

          val op = stack.pop()
          output.enqueue(op)
        }
        stack.push(token.toString)
      } else if (token == '(') {
        stack.push(token.toString)
      } else if (token == ')') {
        while (stack.top != "(") {
          output.enqueue(stack.pop())
        }
      }
    }

    while (stack.nonEmpty) {
      output.enqueue(stack.pop())
    }

    output
  }

  // ===================== \\

  def main(args: Array[String]): Unit = {
    val str = "1+1 - 1 * 3"

    print(infixToPostfix(refine(str)))
  }
}