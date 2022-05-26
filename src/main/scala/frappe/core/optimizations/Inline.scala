package frappe.core.optimizations

import scala.collection.mutable

/* TODO: Inline non-getters as well. */
class Inline {
  private val cache = mutable.HashMap[Int, Array[Byte]]()

  def get(key: Int): Option[Array[Byte]]           = cache.get(key)
  def inline(key: Int, inlined: Array[Byte]): Unit = cache.put(key, inlined)
  def inlined(key: Int): Boolean                   = cache.contains(key)
  def possible(code: Array[Byte]): Boolean         =
    code.length == 5 &&
      (code(0) & 0xff) == 0x2a &&
      (code(1) & 0xff) == 0xb4 &&
      (code(4) & 0xff match {
        case 0xac | 0xad | 0xae | 0xaf | 0xb0 => true
        case _                                => false
      })

  /** Consider the snippet:
   *  {{{
   *     0: aload_0
   *     1: getfield      #7
   *     4: ireturn
   *  }}}
   *
   *  The above bytecode is of a getter method. Getters are commonly
   *  inlined by JVM.
   *
   *  Before inlining:
   *  {{{
   *    0: aload_0
   *    1: invokevirtual #13    // Invoking a getter here.
   *    4: istore_1
   *    5: return
   *  }}}
   *
   *  After inlining:
   *  {{{
   *    0: aload_0
   *    1: getfield      #7
   *    4: istore_1
   *    5: return
   *  }}}
   */
  def perform(
      precede:   Array[Byte], /* Method that is invoking the to-be inlined */
      succeed:   Array[Byte], /* Method that is to be inlined */
      partition: Int          /* Position at which the method is to be inlined */
    ): Array[Byte] = {
    val inlined = new Array[Byte](precede.length + 3)

    precede.copyToArray(inlined, 0, partition)
    succeed.slice(1, 4).copyToArray(inlined, partition)
    precede.slice(partition + 3, precede.length).copyToArray(inlined, partition + 3)

    inlined
  }
}
