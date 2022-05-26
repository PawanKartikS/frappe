package frappe.cli

import frappe.core.lang.Klass
import org.apache.bcel.classfile.{ClassParser, JavaClass}

import java.io.File
import scala.collection.mutable

class Args(args: Array[String]) {
  var mainKlass: String                             = _
  var maxThreshold: Int                             = 4
  var klasses: mutable.ArrayBuffer[(String, Klass)] = mutable.ArrayBuffer[(String, Klass)]()

  def parse(): Unit = {
    args.foreach { arg =>
      if (arg.startsWith("-cp=")) {
        for (klass <- getKlassesInDirectory(new File(arg.stripPrefix("-cp=")))) {
          val path = klass.getPath
          klasses.addOne(path, new Klass(new ClassParser(path).parse()))
        }

      } else if (arg.startsWith("-main=")) {
        mainKlass = arg.stripPrefix("-main=")
      } else if (arg.startsWith("-threshold=")) {
        maxThreshold = arg.stripPrefix("-threshold=").toInt
      }
    }
  }

  private def getKlassesInDirectory(path: File): Array[File] = {
    val files = path.listFiles.filter(f => f.getPath.endsWith(".class"))
    files ++ files.filter(f => f.isDirectory).flatMap(getKlassesInDirectory)
  }
}
