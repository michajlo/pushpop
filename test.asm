start:
  Jsr hello_world

  Push 2
  Jsr increment_by_two
  Push "print"
  CallBIF

  Push 1234     ; place canary
  Push 3
  Jsr sum
  Push "print"
  CallBIF
  Push -1234    ; check that stack not changed
  Add
  JmpZ exit_0
  Jmp exit_1

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

exit_1:
  Push 1
  Push "exit"
  CallBIF

sum:
  Push 0        ; sum = 0
sum_loop:
  LPush 1       ; load i
  Push 0
  CmpGt         ; check i > 0
  JmpF sum_ret  ; if true goto end
  LPush 1       ; load i
  Add           ; sum = sum + i
  LPush 1
  Push -1
  Add
  Assign 1      ; i = i - 1
  Jmp sum_loop
sum_ret:
  Assign 0      ; place sum for return
  Ret
