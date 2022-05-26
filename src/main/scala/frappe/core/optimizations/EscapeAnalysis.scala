package frappe.core.optimizations

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

/*
 * Oracle's spec sheet:
 * NoEscape     = 1 An object does not escape method or thread and it is
                    not passed to call. It could be replaced with scalar.
 * ArgEscape    = 2 An object does not escape method or thread but it is
                    passed as argument to call or referenced by argument
                    and it does not escape during call.
 * GlobalEscape = 3 An object escapes the method or thread.
 */

sealed trait Escapee
case object None   extends Escapee
case object Arg    extends Escapee
case object Global extends Escapee

/* Preliminary escape analysis */
object EscapeAnalysis {
  def perform(code: Array[Byte]): List[(Int, Escapee)] = {
    val esc = getArgEscapees(code).map(e => (e, Arg)).toList
    getGlobalEscapee(code) match {
      case -1 => esc
      case e  => esc :+ (e, Global)
    }
  }

  /** Consider the snippet:
   *  {{{
   *    foo.bar(baz);
   *  }}}
   *
   *  One conclusion might be that baz is ArgEscape. However by looking at
   *  the bytecode generated, we can conclude that
   *  {{{
   *    aload_x        // load `foo` onto the stack, i.e. `objectref`
   *    aload_y        // load `baz` onto the stack, i.e. arg to method
   *    invokevirtual ...
   *  }}}
   *
   *  both foo and baz are arg escapees.
   */
  private def getArgEscapees(code: Array[Byte]): ArrayBuffer[Int] = {
    val buffer        = new ArrayBuffer[Int]()
    val globalEscapee = getGlobalEscapee(code)

    for (i <- code.indices) {
      if ((code(i) & 0xff) == 0xb6) {
        breakable {
          for (j <- i - 1 to 0 by -1) {
            // TODO: Handle aload_0, i.e. this, i.e. return this in builder patterns.
            code(j) match {
              case possibleAload
                  if (possibleAload == 0x19) || ((possibleAload > 0x2b && possibleAload <= 0x2d) &&
                    (possibleAload != globalEscapee)) =>
                buffer += getSlot(code, j)
              case _ => break() // Stop when we see a non-aload instruction.
            }
          }
        }
      }
    }

    buffer
  }

  private def getGlobalEscapee(code: Array[Byte]): Int = code.last & 0xff match {
    case 0xb0 => getSlot(code, code.length - 2)
    case _    => -1
  }
  private def getSlot(code: Array[Byte], i: Int): Int  = code(i) & 0xff match {
    case 0x2b => 1                              // aload_1
    case 0x2c => 2                              // aload_2
    case 0x2d => 3                              // aload_3
    case 0x19 => code(i + 1) << 8 | code(i + 2) // aload
    case _    => -1
  }
}
