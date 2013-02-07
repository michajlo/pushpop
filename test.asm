start:
  Jsr hello_world

  Push 2
  Jsr increment_by_two
  Push "print"
  CallBIF

  Push 3
  Jsr sum
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

sum:
  Push 0
sum_loop:
  LPush 1
  JmpZ sum_ret
  Add
  LPush 1
  Push -1
  Add
  Assign 1
  Jmp sum_loop
sum_ret:
  Pop
  Ret
