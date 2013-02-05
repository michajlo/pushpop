data {
  helloWorld = "hello world"
  bif_print = "print"
}

code {
  LPush helloWorld
  LPush bif_print
  CallBIF

  Push 1
  LPush bif_print
  CallBIF

  Push 0
}
