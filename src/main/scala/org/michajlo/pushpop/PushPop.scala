package org.michajlo.pushpop
import java.io.FileReader
import java.io.File
import org.michajlo.pushpop.asm.PlaintextAsmParser
import org.michajlo.pushpop.vm.VirtualMachine

/**
 * Simple runner, invoke via:
 *
 *   scala ... org.michajlo.pushpop.PushPop run <asm_file>
 */
object PushPop {

  def main(args: Array[String]) = args match {
    case Array("run", asmFile) =>
      val fileReader = new FileReader(new File(asmFile))
      val insns = PlaintextAsmParser.parse(fileReader)
      val vm = new VirtualMachine
      vm.run(insns)
      if (vm.dataStack.empty) 0 else vm.dataStack.pop() match {
        case n: Int => n
        case _ => 0
      }
  }
}