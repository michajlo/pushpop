data {
  helloWorld = "hello world"
  in_sub = "i'm in a subroutine!"

  bif_print = "print"
  bif_exit = "exit"

}

code {
  LPush helloWorld
  LPush bif_print
  CallBIF

  Push 1
  LPush bif_print
  CallBIF

  Jsr 10
  Push 0
  LPush bif_exit
  CallBIF

  LPush in_sub
  LPush bif_print
  CallBIF
  Ret
}
