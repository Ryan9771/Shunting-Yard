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
  private def getNumber(start: Int, str: String): Int = {
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

  // Set of rules to indicate precedence and associativity
  private val rules: Map[String, (Int, String)] = Map.apply(
    "+" -> (0, "left"),
    "-" -> (0, "left"),
    "/" -> (5, "left"),
    "*" -> (5, "left"),
    "^" -> (10, "right"),
  )

  /**
   * Returns the precedence of an operator
   */
  private def getPrec(c: Char): Int = {
    val (p, _) = rules.getOrElse(c.toString, (0, ""))

    p
  }

  /**
   * Checks if an operator is of given associativity
   */
  private def checkAssoc(c: Char, asoc: String): Boolean = {
    val (_, a) = rules.getOrElse(c.toString, (0, ""))

    a.equals(asoc)
  }


  /**
   * The Shunting Yard algorithm that converts infix operators to prefix
   */
  private def infixToPostfix(tokens: String): m.Queue[String] = {
    val output = m.Queue[String]()

    try {
      val stack = m.Stack[String]()

      for (i <- 0 until tokens.length) {

        val token = tokens.charAt(i)

        // If it is a digit
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
          if (stack.top == "(") {
            stack.pop()
          } else {
            throw new UnsupportedOperationException("Incorrect equation form")
          }
        } else {
          throw new UnsupportedOperationException("Incorrect equation form")
        }
      }

      while (stack.nonEmpty) {
        output.enqueue(stack.pop())
      }

    } catch {
      case e => println("Exception: Incorrect syntax")
    }

    output
  }

  /**
   * Returns true if str is a number.
   */
  private def isNumber(str: String): Boolean = {
    try {
      str.toInt
      true
    } catch {
      case e => false
    }
  }

  /**
   * Evaluates an operator along with its arguments.
   */
  private def performOperation(i: Int, j: Int, str: String): Int = {
    str match {
      case "+" => i + j
      case "-" => j - i
      case "*" => i * j
      case "^" => scala.math.pow(j, i).toInt
      case "/" => j / i
      case _ => throw new UnsupportedOperationException("Unsupported Syntax")
    }
  }


  /**
   * Evaluates the postfix queue.
   */
  private def postFixToValue(queue: m.Queue[String]): Int = {
    val stack = m.Stack[Int]()
    while (queue.nonEmpty) {
      val token = queue.dequeue()

      if (isNumber(token)) {
        stack.push(token.toInt)
      } else {
        // It is an operator
        val x1 = stack.pop()
        val x2 = stack.pop()

        stack.push(performOperation(x1, x2, token))
      }
    }

    stack.pop()
  }

  private def shuntingYard(str: String): Int = {

    postFixToValue(infixToPostfix(refine(str)))
  }

  // ===================== \\

  def main(args: Array[String]): Unit = {
    // Should be 4 4 2 * 1 5 - / +
    val str = "4 + 4 * 2 / ( 1 - 5 )"

    println(refine(str))
    println(infixToPostfix(refine(str)))
    println(shuntingYard(str))
  }
}