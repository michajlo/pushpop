package org.michajlo.pushpop.vm

class Stack {
  var ptr: Int = -1
  var stack: Array[Any] = new Array[Any](1024)

  def push(value: Any) {
    ptr += 1
    stack(ptr) = value
  }

  def pop(): Any = {
    val retVal = stack(ptr)
    ptr -= 1
    retVal
  }

  def assign(offset: Int, value: Any) {
    stack(ptr - offset) = value
  }

  def empty = ptr == -1
}
