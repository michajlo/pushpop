package org.michajlo.pushpop.vm
import org.scalatest.FunSpec

import Asm._

class VirtualMachineControlFlowTest extends FunSpec {

  it ("must properly push the next insn ptr on Jsr") {
    val vm = new VirtualMachine

    vm.run(List(Push("exit"), CallBIF, Nop, Nop, Jsr(0), Nop, Nop), 2)

    assert(vm.insnPtrStack.pop() === 5)
  }

  it ("must properly return to the last insn ptr on the stack on Ret") {
    val vm = new VirtualMachine

    vm.insnPtrStack.push(3)

    vm.run(List(Ret, Push("exit"), CallBIF, Nop, Nop, Push(1234)))

    assert(vm.dataStack.pop() === 1234)
  }

  it ("must unconditionally jump on Jmp") {
    val vm = new VirtualMachine

    vm.run(List(Jmp(3), Push("exit"), CallBIF, Push(10)))

    assert(vm.dataStack.pop() === 10)
  }

  it ("must properly jump based on the top of the stack on JmpZ") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), JmpZ(4), Push(0), JmpZ(7), Push(-1), Push("exit"), CallBIF, Push(42)))

    assert(vm.dataStack.pop() == 42)
  }

  it ("must jump on JmpT if the top of the stack is true, popping the top") {
    val vm = new VirtualMachine

    vm.run(List(Push(42), Push(true), JmpT(4), Push(0)))

    assert(42 === vm.dataStack.pop())
  }

  it ("must not jump on JmpT if the top of the stack is false, popping the top") {
    val vm = new VirtualMachine

    vm.run(List(Push(42), Push(false), JmpT(6), Push(0)))

    assert(0 === vm.dataStack.pop())
  }

  it ("must jump on JmpF if the top of the stack is false, popping the top") {
    val vm = new VirtualMachine

    vm.run(List(Push(42), Push(false), JmpF(4), Push(0)))

    assert(42 === vm.dataStack.pop())
  }

  it ("must not jump on JmpF if the top of the stack is true popping the top") {
    val vm = new VirtualMachine

    vm.run(List(Push(42), Push(true), JmpF(6), Push(0)))

    assert(0 === vm.dataStack.pop())
  }
}