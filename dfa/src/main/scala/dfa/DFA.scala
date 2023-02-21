package dfa // leave this line in the file

case class State (val label: String)
case class Transition (val from: State, val to: State, val symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition], 
          val start: State,  val accept: Set[State]):
    
    //Function to determine whether a given string is accepted by the DFA
    def accepts(input: String): Boolean =
        var currentState = start

        //For every character in the input string, step along the DFA once
        var char = ' '
        for (char <- input) currentState = stepOnce(currentState, char)
        
        accept.contains(currentState)

    
    // Helper function: takes a state and a symbol, and returns a state
    def stepOnce(currentState: State, currentSymbol: Char) : State = 
        //First, grab the transition to take
        val currentTransition = transitions.find( t => {t.from == currentState 
                                                 && t.symbol == currentSymbol})
        .get // Scala returns an Option[Transition], which can either be a 
             // Transition or a None. get will result in a type Transition,
             // or will return an error if there is none (which we want).

        //Then, return the state that transition leads to
        currentTransition.to

end DFA
