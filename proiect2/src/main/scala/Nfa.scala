class Nfa[A] (states: Set[A], alphabet: Set[Char], delta: Map[(A, String), Set[A]], q0: A, F: Set[A]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    new Nfa[B](states.map(f), alphabet, delta.map(trans => ((f(trans._1._1), trans._1._2), trans._2.map(f))), f(q0), F.map(f))
  } // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    if (!delta.contains(state, c.toString)) Set() // there is no transition from state A on character c
        // the next states will be the states on which we reach with c and all their epsilon closures
    else delta(state, c.toString) ++ delta(state, c.toString).map(epsClosure).fold(Set())((x,y)=>x++y)
  } // TODO implement next

    /*
      -> if |word| = 0, then we check if the initial state or any of the states from it's epsclosure is final
      -> else we map the next function on the epsilon closure of q0 and then map aux on the result an check if
      true is contained at least once(at least one state is final)
     */
  def accepts(str: String): Boolean = {
    // check if we can get to a final state consuming the word
    def aux(states: Set[A], word: String): Boolean = {
      if (states.nonEmpty) {
        if (word == "") isInFinal(states)
        else aux(next(states.head, word.head), word.tail) || aux(states.tail, word)
      } else false
    }
    if (str.length == 0) isInFinal(epsClosure(q0))
    else epsClosure(q0).map(state => next(state, str.head)).map(states => aux(states, str.tail)).contains(true)
  } // TODO implement accepts

  // returns true if at least one state from states is final
  def isInFinal(states: Set[A]): Boolean = {
    if (states.nonEmpty) isFinal(states.head) || isInFinal(states.tail)
    else false
  }
  def getStates : Set[A] = this.states // TODO implement getStates

  def getAlphabet: Set[Char] = this.alphabet

  def getDelta: Map[(A, String), Set[A]] = this.delta

  def getInitial: A = this.q0

  def getFinal: Set[A] = this.F

  def isFinal(state: A): Boolean = F.contains(state);  // TODO implement isFinal

  // return epsilon closure for state
  def epsClosure(state: A): Set[A] = {
    /*
      -> the function aux returns the epsilon closure for a set of states
      -> if we have a transition from a state on epsilon, we add the state to the returned set,
      we add the epsilon closure for current state
      -> else we just add the current state
     */
    def aux(states: Set[A]): Set[A] = {
      if (states.nonEmpty) {
        if (delta.contains(states.head, "eps")) aux(delta(states.head, "eps")) ++ aux(states.tail) + states.head
        else aux(states.tail) + states.head
      } else Set()
    }
    if (delta.contains(state, "eps")) aux(delta(state, "eps")) + state
    else Set(state)
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  // create NFA from atom
  def fromAtom(atom: String): Nfa[Int] = {
    atom match {
      case "void" => new Nfa(Set(0,1), Set(), Map(), 0, Set(1))
      case "eps" => new Nfa(Set(0), Set(), Map(), 0, Set(0))
      case "space" => new Nfa(Set(0,1), Set(' '), Map((0,' '.toString)->Set(1)), 0, Set(1))
      case c => if(c.length == 1) new Nfa(Set(0,1), Set(c.toCharArray.head), Map((0,c)->Set(1)), 0, Set(1))
                else new Nfa(Set(0,1), Set(c.toList.apply(1)), Map((0,c.toList.apply(1).toString)->Set(1)), 0, Set(1)) // escaped character
    }
  }

  def nfaStar(nfa: Nfa[Int]): Nfa[Int] = {
    val initial: Int = nfa.map(_+1).getInitial
    val finals: Set[Int] = nfa.map(_+1).getFinal
    val states: Set[Int] = nfa.map(_+1).getStates
    val delta: Map[(Int, String), Set[Int]] = nfa.map(_+1).getDelta
    val alphabet: Set[Char] = nfa.getAlphabet
    val newDelta:Map[(Int, String), Set[Int]] = delta + ((finals.head, "eps") -> Set(initial, states.size + 1), (0, "eps") -> Set(initial, states.size + 1))
    new Nfa[Int](states ++ Set(0, states.size + 1), alphabet, newDelta, 0, Set(states.size + 1))
  }

  def nfaConcat(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    val states1 = nfa1.getStates
    val states2 = nfa2.map(_+states1.size).getStates
    val initial2 = nfa2.map(_+states1.size).getInitial
    val finals1 = nfa1.getFinal
    val delta = nfa1.getDelta ++ nfa2.map(_+states1.size).getDelta
    val newDelta = delta ++ Map((finals1.head, "eps")->Set(initial2))
    new Nfa[Int](states1 ++ states2, nfa1.getAlphabet ++ nfa2.getAlphabet, newDelta, nfa1.getInitial, nfa2.map(_+states1.size).getFinal)

  }

  def nfaUnion(nfa1: Nfa[Int], nfa2:Nfa[Int]): Nfa[Int] = {
    val states1 = nfa1.map(_+1).getStates
    val states2 = nfa2.map(_+1 + states1.size).getStates
    val initial1 = nfa1.map(_+1).getInitial
    val initial2 = nfa2.map(_+1+states1.size).getInitial
    val finals1 = nfa1.map(_+1).getFinal
    val finals2 = nfa2.map(_+1+states1.size).getFinal
    val delta = nfa1.map(_+1).getDelta ++ nfa2.map(_+1 + states1.size).getDelta
    val f = states1.size + states2.size + 1
    val alphabet = nfa1.getAlphabet ++ nfa2.getAlphabet
    val newDelta:Map[(Int, String), Set[Int]] = delta ++ Map((0, "eps")->Set(initial1, initial2), (finals1.head, "eps")->Set(f), (finals2.head, "eps")->Set(f))
    new Nfa[Int](Set(0) ++ states1 ++ states2 ++ Set(f), alphabet, newDelta, 0, Set(f))
  }

  def nfaPlus(nfa: Nfa[Int]): Nfa[Int] = {
    nfaConcat(nfa, nfaStar(nfa))
  }

  def nfaMaybe(nfa: Nfa[Int]): Nfa[Int] = {
    nfaUnion(nfa, Nfa.fromAtom("eps"))
  }

  def getIndices(list: List[String]): Int = {
    def aux(l: List[String], x: Int, count: Int): Int = {
      if (x == 1) count
      else l match {
        case Nil => count
        case y::xs => y match {
          case "CONCAT" => aux(xs, x + 1, count + 1)
          case "UNION" => aux(xs, x + 1, count + 1)
          case "STAR" => aux(xs, x, count + 1)
          case "PLUS" => aux(xs, x, count + 1)
          case "MAYBE" => aux(xs, x, count + 1)
          case _ => aux(xs, x - 1, count + 1)
        }
      }
    }
    aux(list, 2, 0)
  }

  def checkForSpace(list: List[String]): List[String] = {
    def aux(l: List[String]): List[String] = {
      if (l.contains("'")) l.take(l.indexOf("'")) ++ List("space") ++ l.drop(l.indexOf("'") + 2)
      else l
    }
    if (aux(list).contains("'")) checkForSpace(aux(list))
    else aux(list)
  }

  def fromPrenex(str: String): Nfa[Int] = {
    val list: List[String] = str.split(" ").toList
    val newList = checkForSpace(list)

    newList match {
      case Nil => fromAtom("void")
      case x::Nil => fromAtom(x)
      case x::xs => x match {
        case "STAR" => nfaStar(fromPrenex(xs.mkString(" ")))
        case "PLUS" => nfaPlus(fromPrenex(xs.mkString(" ")))
        case "MAYBE" => nfaMaybe(fromPrenex(xs.mkString(" ")))
        case "CONCAT" => nfaConcat(fromPrenex(xs.take(getIndices(xs)).mkString(" ")), fromPrenex(xs.drop(getIndices(xs)).mkString(" ")))
        case "UNION" => nfaUnion(fromPrenex(xs.take(getIndices(xs)).mkString(" ")), fromPrenex(xs.drop(getIndices(xs)).mkString(" ")))
        case _ => fromAtom(x)
      }
    }
  } // TODO implement Prenex -> Nfa transformation.

  // You can add more methods to this object
}