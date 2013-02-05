package org.michajlo.pushpop.vm
import org.scalatest.FunSpec

class VirtualMachineTest extends FunSpec {

  it ("must add properly") {
    val vm = new VirtualMachine

    vm.run(List(Asm.Push(1), Asm.Push(2), Asm.Add))

    assert(3 === vm.stack.pop())
    assert(vm.stack.empty)
  }

  it ("must subtract properly") {
    val vm = new VirtualMachine

    vm.run(List(Asm.Push(2), Asm.Push(1), Asm.Sub))

    assert(1 === vm.stack.pop())
    assert(vm.stack.empty)
  }

  it ("must multiply properly") {
    val vm = new VirtualMachine

    vm.run(List(Asm.Push(7), Asm.Push(8), Asm.Mul))

    assert(56 === vm.stack.pop())
    assert(vm.stack.empty)
  }

  it ("must divide properly") {
    val vm = new VirtualMachine

    vm.run(List(Asm.Push(10), Asm.Push(5), Asm.Div))

    assert(2 === vm.stack.pop())
    assert(vm.stack.empty)
  }
}