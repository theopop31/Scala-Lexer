class Dfa[A] (states: Set[A], alphabet: Set[Char], delta: Map[(A, String), A], q0: A, F: Set[A]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    new Dfa[B](states.map(f), alphabet, delta.map(trans => ((f(trans._1._1), trans._1._2), f(trans._2))), f(q0), F.map(f))
  } // TODO implement map

  def next(state:A, c: Char): A = {
    if (delta.contains(state, c.toString)) delta(state,c.toString)
    else state
  } // TODO implement next

  def accepts(str: String): Boolean = {
    def aux(state: A, word: List[Char]): Boolean = {
      if (word.isEmpty) isFinal(state)
      else if (delta.contains(state, word.head.toString)) aux(delta(state, word.head.toString), word.tail)
      else false
    }
    aux(q0, str.toCharArray.toList)
  } // TODO implement accepts

  def getStates : Set[A] = this.states // TODO implement getStates

  def isFinal(state: A): Boolean = F.contains(state)  // TODO implement isFinal
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  def epc(states: Set[Int], nfa: Nfa[Int]): Set[Int] = {
    if (states.nonEmpty) nfa.epsClosure(states.head) ++ epc(states.tail, nfa)
    else Set()
  }

  def newDelta(deltaDfa: Map[(Set[Int], String), Set[Int]], statesDfa: List[Set[Int]]):  Map[(Int, String), Int] = {
    if (deltaDfa.isEmpty) Map()
    else newDelta(deltaDfa.tail, statesDfa) ++ Map((statesDfa.toList.indexOf(deltaDfa.head._1._1), deltaDfa.head._1._2) ->statesDfa.toList.indexOf(deltaDfa.head._2))
  }

  def newStates(statesDfaSize: Int, count: Int): Set[Int] = {
    if (count == 0) Set()
    else newStates(statesDfaSize, count - 1) ++ Set(statesDfaSize - count)
  }

  def newFinals(statesDfa: List[Set[Int]], finals: Set[Int], copy: List[Set[Int]]): Set[Int] = {
    if (statesDfa.isEmpty) Set()
    else if (statesDfa.head.intersect(finals).nonEmpty) newFinals(statesDfa.tail, finals, copy) ++ Set(copy.toList.indexOf(statesDfa.head))
    else newFinals(statesDfa.tail, finals, copy)
  }

  def fromPrenex(str: String): Dfa[Int] = {
    val nfa: Nfa[Int] = Nfa.fromPrenex(str)
    val initialNfa: Set[Int] = nfa.epsClosure(nfa.getInitial)
    val deltaNfa = nfa.getDelta
    var deltaDfa: Map[(Set[Int], String), Set[Int]] = Map()
    var statesDfa: List[Set[Int]] = List(initialNfa)
    val alphabet: Set[Char] = nfa.getAlphabet
    val finals: Set[Int] = nfa.getFinal

    var j : Int = 0
    while (j < statesDfa.length) {
      for (i <- alphabet) {
        var states: Set[Int] = Set()
        for (k <- statesDfa(j)) {
          if (deltaNfa.contains(k,i.toString)) states = states ++ epc(deltaNfa(k, i.toString), nfa)
        }
        if (states.nonEmpty) {

          deltaDfa = deltaDfa ++ Map((statesDfa(j), i.toString)->states)
        }
        if (!statesDfa.contains(states)) {
          statesDfa = statesDfa ++ Set(states)
        }
      }
      j = j + 1
    }
    val newDelta: Map[(Int, String), Int] = Dfa.newDelta(deltaDfa, statesDfa)
    val newStates: Set[Int] = Dfa.newStates(statesDfa.size, statesDfa.size)
    val newInitial: Int = statesDfa.toList.indexOf(initialNfa)
    val newFinals: Set[Int] = Dfa.newFinals(statesDfa, finals, statesDfa)
    new Dfa[Int](newStates, alphabet, newDelta, newInitial, newFinals)
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  // You can add more methods to this object
}
