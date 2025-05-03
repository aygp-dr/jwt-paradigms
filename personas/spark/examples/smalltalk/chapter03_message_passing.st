"A simple example in Smalltalk"
| stack |
stack := OrderedCollection new.  "Creating an object via message passing"
stack add: 'first item'.        "Sending a message with an argument"
stack add: 'second item'.
stack removeLast.               "Another message"
stack isEmpty                   "Query via message"
  ifFalse: [Transcript show: stack first]  "Control flow via message passing"
