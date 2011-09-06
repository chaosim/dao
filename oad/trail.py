# -*- coding: utf-8 -*-

from oad.term import subst

class Trail(object):
  INITSIZE = 2
  def __init__(self, prev=None):
    self.vars = [None]*Trail.INITSIZE
    self.bindings = [None]*Trail.INITSIZE
    self.index = 0
    self.prev = prev
    self.discarded = False

  def add(self, var):
    """ Remember the current state of a variable to be able to backtrack it
    to that state. Usually called just before a variable changes. """
    # if the variable doesn't exist before the last choice point, don't
    # trail it (variable shunting)
    created_in = var.created_after_choice_point
    if created_in is not None and created_in.discarded:
      created_in = created_in._find_not_discarded()
      var.created_after_choice_point = created_in
    if self is created_in: return
    index = self.index
    if index>=len(self.vars): self._double_size()
    self.vars[index] = var
    self.bindings[index] = subst[var]
    self.index = index + 1

  def _find_not_discarded(self):
    while self is not None and self.discarded: self = self.prev
    return self

  def _double_size(self):
    vars = [None] * (len(self.vars) * 2)
    bindings = [None] * len(vars)
    for index in range(self.index):
      vars[index] = self.vars[index]
      bindings[index] = self.bindings[index]
    self.vars = vars
    self.bindings = bindings

  def branch(self): return Trail(self)    

  def revert_upto(self, trail, discard_choicepoint=False):
    """ Revert to the trail corresponding to a choice point."""
    previous = self
    while self is not trail:
      self._revert()
      previous = self
      self = self.prev
    if discard_choicepoint: return trail
    return previous

  def _revert(self):
    for index in range(self.index):
      if self.bindings[index] is not None:
        subst[self.vars[index]] = self.bindings[index]
      else: del subst[self.vars[index]]
      self.vars[index] = None
      self.bindings[index] = None
    self.index = 0

  def discard(self, current_trail):
    """ Remove a trail that is no longer needed (usually due to a cut) 
    from a chain of frames. """
    self.discarded = True
    if current_trail.prev is self:
      targetpos = 0
      # check whether variables in the current trail no longer need to be
      # traced, because they originate in the discarded trail
      for index in range(current_trail.index):
        var = current_trail.vars[index]
        binding = current_trail.bindings[index]
        if var.created_after_choice_point is self:
          var.created_after_choice_point = self.prev
          current_trail.vars[index] = None
          current_trail.bindings[index] = None
        else:
          current_trail.vars[targetpos] = var
          current_trail.bindings[targetpos] = binding
          targetpos += 1
      current_trail.index = targetpos

      # move the variable bindings from the discarded trail to the current trail
      for index in range(self.index):
        var = self.vars[index]
        currbinding = subst[var]
        binding = self.bindings[index]
        subst[var] = binding
        current_trail.add(var)
        subst[var] = currbinding
      current_trail.prev = self.prev
      self.vars = None
      self.bindings = None
      self.index = -1
      self.prev = current_trail
    else: return self
    return current_trail

  def __repr__(self): return "<Head %r trailed vars>" % (self.index, )
