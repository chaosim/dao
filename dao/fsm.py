class StateMachine: 
  def __init__(self, words):
    self.start_state = self._new_state = 0
    self.tran_dict = {}
    self.tran_dict[0] = {}
    self.state2length = {} #stop state to word length
    self.words = set()
    for w in words: self.add(w)
    
  def add(self, word):
    self.words.add(word)
    state = 0
    for c in word[:-1]:      
      if c in self.tran_dict[state]: 
        state = self.tran_dict[state][c]
        state = abs(state)
      else: 
        newState = self.new_state()
        self.tran_dict[state][c] = newState
        self.tran_dict[newState] = {}
        state = newState
    c = word[-1]
    if c in self.tran_dict[state]:
      end_state = self.tran_dict[state][c]
      if end_state>0: 
        self.tran_dict[state][c] = -end_state # negative state means stop state
    else: 
      end_state = newState = self.new_state()
      self.tran_dict[state][c] = -newState
      self.tran_dict[newState] = {}
    self.state2length[abs(end_state)] = len(word)
  
  def new_state(self):
    self._new_state += 1
    return self._new_state
  
  def go(self, state, char):
    return self.tran_dict.get(state, {}).get(char)

  def match(self, string):
    '''max match, return matched length'''
    state = self.start_state
    result = 0
    i = 1
    for c in string:
      state = self.go(state, c)
      if state is None: return result
      if state<0: 
        result = i
        state = -state
      i += 1
    return result

FSM = StateMachine
