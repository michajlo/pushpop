package org.michajlo.pushpop.vm

/**
 * The heart and soul of the pushpop vm, this is where
 * all state is stored.
 *
 * This guy can use some love
 */
class Stack {
  var ptr: Int = -1
  var stack: Array[Any] = new Array[Any](1024)

  /**
   * Push value onto the stack, increments the stack pointer
   *
   * @param value the value to push
   */
  def push(value: Any) {
    ptr += 1
    stack(ptr) = value
  }

  /**
   * Pop a value off the stack, decrements the stack pointer
   *
   * If the stack is empty fails hard
   *
   * @return value at the top of the stack
   */
  def pop(): Any = {
    val retVal = stack(ptr)
    ptr -= 1
    retVal
  }

  /**
   * Is the stack empty?
   */
  def empty = ptr == -1
}
