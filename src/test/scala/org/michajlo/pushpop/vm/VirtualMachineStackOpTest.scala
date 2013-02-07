package org.michajlo.pushpop.vm

import org.scalatest.FunSpec

import Asm._

class VirtualMachineStackOpTest extends FunSpec {

  it ("must push a value on Push") {
    val vm = new VirtualMachine

    vm.run(List(Push(100)))

    assert(100 === vm.dataStack.pop())
  }

  it ("must pop a value from the stack on Pop") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Pop))

    assert(vm.dataStack.empty)
  }

  it ("must load the value from the proper offset in the stack and push it on LPush") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(11), LPush(1)))

    assert(10 === vm.dataStack.pop())
  }

  it ("must pop the value on the stack and assign it to offset on Assign") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(11), Push(12), Assign(1)))

    assert(11 === vm.dataStack.pop())
    assert(12 === vm.dataStack.pop())
  }
}