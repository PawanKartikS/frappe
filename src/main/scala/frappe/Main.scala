package frappe

import frappe.cli.Args
import frappe.core.VirtualMachine

object Main {
  def main(args: Array[String]): Unit = {
    val vmargs = new Args(args)
    vmargs.parse()
    new VirtualMachine(vmargs).run()
  }
}
