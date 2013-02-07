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
   * Get the value at the top of the stack without popping it
   *
   * @return value on the top of the stack
   */
  def peek: Any = stack(ptr)

  /**
   * Get a value from the stack, relative to the current stack pointer
   *
   * @param offset into the stack to get the value from
   *
   * @return value in the stack at offset
   */
  def get(offset: Int) = stack(ptr - offset)

  /**
   * Assign value to offest in the stack
   *
   * @param offset offset to assign to
   * @param value value to assign to offset
   */
  def assign(offset: Int, value: Any) {
    stack(ptr - offset) = value
  }

  /**
   * Is the stack empty?
   */
  def empty = ptr == -1
}
