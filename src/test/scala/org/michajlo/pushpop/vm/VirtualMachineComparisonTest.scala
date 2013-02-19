package org.michajlo.pushpop.vm

import org.scalatest.FunSpec
import Asm._

class VirtualMachineComparisonTest extends FunSpec {

  it ("must properly compare gt when true") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(9), CmpGt))

    assert(true === vm.dataStack.pop())
  }

  it ("must properly compare gt when false") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(10), CmpGt))

    assert(false === vm.dataStack.pop())
  }

  it ("must properly compare gte when true") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(10), CmpGte))

    assert(true === vm.dataStack.pop())
  }

  it ("must properly compare gte when false") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(11), CmpGte))

    assert(false === vm.dataStack.pop())
  }

  it ("must properly compare lt when true") {
    val vm = new VirtualMachine

    vm.run(List(Push(9), Push(10), CmpLt))

    assert(true === vm.dataStack.pop())
  }

  it ("must properly compare lt when false") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(10), CmpLt))

    assert(false === vm.dataStack.pop())
  }

  it ("must properly compare lte when true") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(10), CmpLte))

    assert(true === vm.dataStack.pop())
  }

  it ("must properly compare lte when false") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(9), CmpLte))

    assert(false === vm.dataStack.pop())
  }

  it ("must properly compare eq when true") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(10), CmpEq))

    assert(true === vm.dataStack.pop())
  }

  it ("must properly compare eq when false") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(11), CmpEq))

    assert(false === vm.dataStack.pop())
  }

  it ("must properly compare neq when true") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(11), CmpNeq))

    assert(true === vm.dataStack.pop())
  }

  it ("must properly compare neq when false") {
    val vm = new VirtualMachine

    vm.run(List(Push(10), Push(10), CmpNeq))

    assert(false === vm.dataStack.pop())
  }
}