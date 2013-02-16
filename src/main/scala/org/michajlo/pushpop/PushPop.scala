package org.michajlo.pushpop

import java.io.FileReader
import java.io.File
import org.michajlo.pushpop.asm.PlaintextAsmParser
import org.michajlo.pushpop.vm.VirtualMachine
import org.michajlo.pushpop.lang.Reifier
import org.michajlo.pushpop.lang.LangParser
import java.io.FileWriter

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

    case Array("compile", sourceFile) =>
      val fileReader = new FileReader(new File(sourceFile))
      val ast = LangParser.parse(fileReader)
      val asm = Reifier.reify(ast, Nil)._1
      val out = new FileWriter(sourceFile + ".asm", false)
      asm.foreach(op => { out.append(op); out.append("\n") })
      out.close()
      0
  }
}