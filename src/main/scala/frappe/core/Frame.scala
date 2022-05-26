package frappe.core

import scala.collection.mutable

class Frame(args: Array[Any]) {
  private val locals = new Array[Any](16) // TODO: Raise limit?
  private val stack  = new mutable.Stack[Any]()
  args.reverse.copyToArray(locals)

  def getLocals: Array[Any]           = locals
  def getLocalAt[T](pos: Int): T      = locals(pos).asInstanceOf[T]
  def getOperands: mutable.Stack[Any] = stack
  def load(pos: Int): Unit            = stack.push(locals(pos))
  def store(pos: Int): Unit           = locals(pos) = stack.pop()
  def set(pos: Int, value: Any): Unit = locals(pos) = value
  def push(value: Any): Unit          = stack.push(value)
  def peek: Any                       = stack.top
  def pop[T]: T                       = stack.pop().asInstanceOf[T]
  def pop(n: Int): Array[Any]         = {
    val arr = new Array[Any](n)
    for (i <- 0 until n) {
      arr(i) = pop
    }

    arr
  }
}
