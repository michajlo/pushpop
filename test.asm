Push "hello world"
Push "print"
CallBIF

Push 1
Push "print"
CallBIF

Jsr subroutine
Push 0
Push "exit"
CallBIF

subroutine:
Push "I'm in a subroutine!"
Push "print"
CallBIF
Ret
