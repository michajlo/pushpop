start:
  Jsr hello_world

  Push 2
  Jsr increment_by_two
  Push "print"
  CallBIF

  Jsr exit_0

hello_world:
  Push "I'm in a subroutine!"
  Push "print"
  CallBIF
  Ret

increment_by_two:
  Push 2
  Add
  Ret

exit_0:
  Push 0
  Push "exit"
  CallBIF
