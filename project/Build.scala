import sbt._
import Keys._

object MyBuild extends Build {

  lazy val makeDist = TaskKey[Unit]("make-dist")

  def doMakeDist(srcDir: File, cp: Classpath, jar: File, targetDir: File, vers: String) {
    val outDir = targetDir / ("pushpop-" + vers)
    val libDir = outDir / "lib"

    IO.copyFile(jar, libDir / jar.getName, preserveLastModified=true)
    cp.files.foreach( f => IO.copyFile(f, libDir / f.getName, preserveLastModified=true) )

    val binDir = outDir / "bin"
    IO.copyDirectory(srcDir / "main" / "bin", binDir, preserveLastModified=true)
    IO.listFiles(binDir).foreach( f => if (f.isFile) f.setExecutable(true, false) )
  }

  def makeDistTask = makeDist <<= (sourceDirectory, managedClasspath in Runtime, packageBin in Compile, target, version) map {
    (srcDir, cp, jar, out, vers) => doMakeDist(srcDir, cp, jar, out, vers)
  }

  lazy val root = Project(
    "root",
    file("."),
    settings = Defaults.defaultSettings ++ Seq(makeDistTask)
  )
}
