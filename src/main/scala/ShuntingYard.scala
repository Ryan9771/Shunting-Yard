object ShuntingYard {



  def refine(str: String): String = {
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

  private def isOperator(c : Char): Boolean = {
    c match {
      case '*' | '+' | '/' | '^' | '-' | 'm' => true
      case _ => false
    }
  }

  def main(args: Array[String]): Unit = {
    val str = "-2 ^ -4 + 5"
    print(refine(str))
  }
}