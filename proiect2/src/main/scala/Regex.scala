import scala.collection.mutable.Stack
object Regex {
  case class Node(var left: Node, var value: Char, var right: Node)

  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:String): List[Either[Char,Char]] = {

    /* the function gets a start index and goes back in string str to find lookFor character */
    def findPair(startPos: Int, lookFor: Char, str: String): Int = {
      if (startPos < 0) -1
      else if (str.charAt(startPos) == lookFor) startPos
      else findPair(startPos - 1, lookFor, str)
    }

    // function returns number of appearances of lookFor in str starting from index i
    def findNumApp(i: Int, str: String, lookFor: Char): Int = {
      if (i < 0) 0
      else 1 + findNumApp(str.indexOf(lookFor, i + 1), str, lookFor)
    }

    // replace '+' operator in regex
    def replacePlus(str: String): String = {
      /*
        -> search for '+' character that isn't escaped, replace whole structure before with kleene star structure
        -> i is index of '+' in string
        -> str is string to be processed
       */
      def searchAndReplace(i: Int, str: String): String = {
        if (i > 0) { // str contains '+'
           if (str.charAt(i - 1) == '\'' && (i + 1) <= str.length - 1 && str.charAt(i + 1) == '\'') searchAndReplace(str.indexOf('+', i + 1), str)
            else if (str.charAt(i - 1) == ')') {
              val s = str.substring(findPair(i - 1, '(', str), i + 1)
              val newS = str.replace(s, s.dropRight(1) ++ s.dropRight(1) ++ "*")
              searchAndReplace(newS.indexOf('+', i + 1), newS)
            } else if (str.charAt(i - 1) == ']') {
              val s = str.substring(findPair(i - 1, '[', str), i + 1)
              val newS = str.replace(s, s.dropRight(1) ++ s.dropRight(1) ++ "*")
              searchAndReplace(newS.indexOf('+', i + 1), newS)
            } else {
              val s = str.substring(i - 1, i + 1)
              val newS = str.replace(s, s.dropRight(1) ++ s.dropRight(1) ++ "*")
              searchAndReplace(newS.indexOf('+', i + 1), newS)
            }
        }
        else str // str does not contain '+' anymore
      }
      searchAndReplace(str.indexOf("+"), str)
    }

    // same as replacePlus, but replaces '?'
    def replaceQuestion(str: String): String = {
      def searchAndReplace(i: Int, str: String): String = {
        if (i > 0) {
          if (str.charAt(i - 1) == '\'' && (i + 1) <= str.length - 1 && str.charAt(i + 1) == '\'') searchAndReplace(str.indexOf('?', i + 1), str)
            else if (str.charAt(i - 1) == ')') {
              val s = str.substring(findPair(i - 1, '(', str), i + 1)
              val newS = str.replace(s, "(" ++ s.dropRight(1) ++ "|eps)")
              searchAndReplace(newS.indexOf('?', i + 1), newS)
            } else if (str.charAt(i - 1) == ']') {
              val s = str.substring(findPair(i - 1, '[', str), i + 1)
              val newS = str.replace(s, "(" ++ s.dropRight(1) ++ "|eps)")
              searchAndReplace(newS.indexOf('?', i + 1), newS)
            } else {
              val s = str.substring(i - 1, i + 1)
              val newS = str.replace(s, "(" ++ s.dropRight(1) ++ "|eps)")
              searchAndReplace(newS.indexOf('?', i + 1), newS)
            }
          }
        else str
      }
      searchAndReplace(str.indexOf("?"), str)
    }

    // function determines if a character is an operator or a simple character
    def operatorOrCharacter(str: List[Char], l: List[Either[Char,Char]]): List[Either[Char,Char]] = {
      val operators = List('(',')', '|', '*') // list of valid operators
      str match {
        case Nil => l
        case x::xs => if(operators.contains(x)) operatorOrCharacter(xs, Left(x) :: l) // is operator
                      else if (x != '\'') operatorOrCharacter(xs, Right(x) :: l) // is simple character
                      else operatorOrCharacter(xs, l)
      }
    }
    operatorOrCharacter(replaceQuestion(replacePlus(s)).replace("[a-z]", "(" + ('a' to 'z').toList.mkString("|") + ")")
      .replace("[A-Z]", "(" + ('A' to 'Z').toList.mkString("|") + ")")
      .replace("[0-9]", "(" + ('0' to '9').toList.mkString("|") + ")").replace("eps", "ε").toList, List()).reverse
  }

  // this function explicitly adds a '.' as concat operator where it is needed
  def addConcat(list: List[Either[Char,Char]]): List[Either[Char,Char]] = {
    /*
      -> the aux function takes a list to iterate, a newList as an accumulator and return value, and prev as the previous
      element from list l;
      -> if both previous and current element are characters we add concat between them
      -> if we have structures like "character(",  ")character" or "*character" we add concat between them
      -> if we have structures like ")(" or "*(" we also add concat between them
      -> if none of the above, we do not add concat
     */
    def aux(l: List[Either[Char,Char]], newList: List[Either[Char,Char]], prev: Either[Char, Char]): List[Either[Char,Char]] = {
      l match {
        case Nil => newList
        case x :: xs => if (prev.isRight && x.isRight) aux(xs, newList ++ List(Left('.')) ++ List(x), x)
        else {
          if (x.isRight) {
            prev match {
              case Left(a) => if(a == ')' || a == '*') aux(xs, newList ++ List(Left('.')) ++ List(x), x)
              else aux(xs, newList ++ List(x), x)
            }
          } else if (prev.isRight) {
            x match {
              case Left(a) => if (a =='(') aux(xs, newList ++ List(Left('.')) ++ List(x), x)
              else aux(xs, newList ++ List(x), x)
            }
          } else {
            prev match {
              case Left(a) => x match {
                case Left(b) => if ((a == ')' && b == '(') || (a == '*' && b == '(')) aux(xs, newList ++ List(Left('.')) ++ List(x), x)
                else aux(xs, newList ++ List(x), x)
              }
            }
          }
        }
      }
    }
    aux(list.tail, List(list.head), list.head)
  }

  // function creates tree depending on what type the operator is
  def createTree(op: Char, treeStack: Stack[Node]): Node = {
    op match {
      case '*' => Node(treeStack.pop(), op, null)
      case _ => {
        val t1 = treeStack.pop()
        val t2 = treeStack.pop()
        Node(t1, op, t2)
      }
    }
  }

  // preorder traversal of final tree
  def preorder(node: Node): String = {
    if (node == null) ""
    else {
      node.value match {
        case '*' => "STAR " ++ preorder(node.left) ++ preorder(node.right)
        case '.' => "CONCAT " ++ preorder(node.left) ++ preorder(node.right)
        case '|' => "UNION " ++ preorder(node.left) ++ preorder(node.right)
        case 'ε' => "eps " ++ preorder(node.left) ++ preorder(node.right)
        case ' ' => "\' \' " ++ preorder(node.left) ++ preorder(node.right)
        case _ => node.value.toString ++ " " ++ preorder(node.left) ++ preorder(node.right)
      }
    }
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val priority: Map[Char, Int] = Map(')' -> 0, '|' -> 1, '.' -> 2, '*' -> 3, '(' -> 4) // the priority of the operators
    val operatorStack = new Stack[Char]
    val treeStack = new Stack[Node]
    val list = addConcat(preprocess(str)).reverse // list containing the preprocessed string
    for (i <- list) { // for every element in list
        i match {
          case Right(x) => treeStack.push(Node(null, x, null)) // if it's a simple character create AST and push to stack
          case Left(x) => if (operatorStack.isEmpty) operatorStack.push(x) // if it's the first operator in stack add it
          else if (priority(x) > priority(operatorStack.top) || x == ')') operatorStack.push(x) // if current operator can be added to stack add it
          else {
            // we will create ASTs until we can add operator to stack
            while(!operatorStack.isEmpty && priority(x) <= priority(operatorStack.top)) {
              var op = operatorStack.pop()
              if (op == '(') { // if we find '(' we will create ASTs until we find the ')' pair
                while (op != ')') {
                  op = operatorStack.pop()
                  while (op != ')') {
                    treeStack.push(createTree(op, treeStack))
                    op = operatorStack.pop()
                  }
                }
              } else {
                treeStack.push(createTree(op, treeStack))
              }
            }
            operatorStack.push(x) // add operator
          }
        }
    }

    // finish creating the final AST using the same algorithm
    while (!operatorStack.isEmpty) {
      var op = operatorStack.pop()
      if (op == '(') {
        op = operatorStack.pop()
        while (op != ')') {
          treeStack.push(createTree(op, treeStack))
          op = operatorStack.pop()
        }
      } else {
        treeStack.push(createTree(op, treeStack))
      }
    }
    // preorder traversal from which we delete the last character which is a space
    preorder(treeStack.pop()).dropRight(1)
  }
}
