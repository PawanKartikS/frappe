/** file: VirtualMachine.scala Contains code responsible for executing the
 *  specified bytecode.
 *
 *  Note: There's a lot of stuff pending such as C1 compilation to native
 *  arch, scalar opt., etc.
 */

package frappe.core

import frappe.cli.Args
import frappe.core.bytecode.Opcode
import frappe.core.lang.Klass
import frappe.core.optimizations.{C1, EscapeAnalysis, Inline}
import org.apache.bcel.classfile._

import scala.collection.mutable

/** Instantiates a VM and begins bytecode execution.
 *  {{{
 *    new VirtualMachine(vmargs).run()
 *  }}}
 */
class VirtualMachine(vmargs: Args) {
  private val klasses = vmargs.klasses                          /* Path <-> Klass */
  private val frames  = mutable.Stack[Frame]()                  /* For methods */
  private val pools   = mutable.Stack[(String, ConstantPool)]() /* Class <-> CP */
  private val c1Cache = new C1( /*maxThreshold=*/ 4)
  private val inline  = new Inline()                            /* Inline getters */
  val PSVM_SIGNATURE  = "([Ljava/lang/String;)V"                /* To identify PSVM */

  private def context(args: Array[Any])(f: Frame => Unit): Unit = {
    val frame = new Frame(args)

    frames.push(frame)
    f(frame)
    frames.pop()
  }

  private def loadKlass(name: String): Option[Klass] = klasses
    .find(c => c._1.endsWith(s"$name.class"))
    .map(mv => mv._2)

  private def getMethod(
      code:    Array[Byte],
      i:       Int,
      parent:  Int,
      _static: Boolean
    ): Method = {

    /** To invoke a method, irrespective of static or non-static, we need
     *  information such as class name, method name, and descriptor. This
     *  lets us track where the method exists along with the correct
     *  overload.
     */
    var methodInDifferentKlass = false
    val offset                 = code(i + 1) << 8 | code(i + 2)
    val top                    = pools.top
    val pool                   = top._2
    val ref                    = pool.getConstant(offset).asInstanceOf[ConstantMethodref]
    val nameAndType            = pool
      .getConstant(ref.getNameAndTypeIndex)
      .asInstanceOf[ConstantNameAndType]

    /* Info that'd help us resolve method and its overload correctly. */
    val targetMethodName = nameAndType.getName(pool)
    val targetSignature  = nameAndType.getSignature(pool)
    val klassName        = pool.getConstant(ref.getClassIndex)
      .asInstanceOf[ConstantClass]
      .getBytes(pool)

    var klass: Klass = null
    if (klassName == top._1) {
      klass =
        if (!_static) {
          frames.top.getLocals(0).asInstanceOf[Klass] // aload_0
        } else {
          loadKlass(klassName).get
        }
    } else {
      methodInDifferentKlass = true
      loadKlass(klassName) match {
        case Some(k) => klass = k
        case None    =>
          throw new IllegalArgumentException(
            "VM: Could not find class " + klassName
          );
      }

      pools.push((klass.name, klass.pool))
    }

    var callee: Method = null
    val key            = s"${ref.getClassIndex}-$targetMethodName-$targetSignature"
    c1Cache.get(key) match {
      case Some(c) => // Method was cached, use it.
        println("VM: Invoking cached method")
        callee = c
      case None    => // Method wasn't cached, manually resolve it.
        klass.getMethod(targetMethodName, targetSignature) match {
          case Some(method) => callee = method
          case None         => callee = null
        }

        c1Cache.incrementThreshold(key)
        if (c1Cache.exceedsThreshold(key)) {
          printf("VM: Caching method %s\n", targetMethodName)
          c1Cache.cache(key, callee)
        }
    }

    val toBeCalled = callee.getCode.getCode
    if (
      !methodInDifferentKlass &&     /* Temp: Only for now */
      !inline.inlined(parent) &&     /* No previously inline */
      inline.possible(toBeCalled) && /* Temp: Inline getters only */
      c1Cache.exceedsThreshold(key)  /* Temp: Inline only if C1'd */
    ) {
      val inlined = inline.perform(code, toBeCalled, /*partition=*/ i)
      inline.inline(parent, inlined)
      printf("VM: Inlined method to %d opcodes\n", inlined.length)
    }

    callee
  }

  def exec(
      code:   Array[Byte], /* Method's bytecode */
      i:      Int,         /* Position of the current opcode to execute */
      parent: Int,         /* Method's hashcode */
      caller: Frame,       /* Frame of the method that invoked the current method */
      callee: Frame        /* Frame of current method */
    ): Int = {
    val opcode = Opcode.from(code(i)) match {
      case Some(v) => v
      case None    =>
        throw new IllegalArgumentException("Invalid opcode " + code(i).toHexString)
    }

    printf("VM: Executing opcode at %d: %s\n", i, opcode.toString)

    opcode match {
      case Opcode.NOP         =>
      case Opcode.ACONST_NULL => callee.push(null)
      case Opcode.ICONST_M1   => callee.push(-1)
      case Opcode.ICONST_0    => callee.push(0)
      case Opcode.ICONST_1    => callee.push(1)
      case Opcode.ICONST_2    => callee.push(2)
      case Opcode.ICONST_3    => callee.push(3)
      case Opcode.ICONST_4    => callee.push(4)
      case Opcode.ICONST_5    => callee.push(5)
      case Opcode.LCONST_0    => callee.push(0L)
      case Opcode.LCONST_1    => callee.push(1L)
      case Opcode.FCONST_0    => callee.push(0f)
      case Opcode.FCONST_1    => callee.push(1f)
      case Opcode.FCONST_2    => callee.push(2f)
      case Opcode.DCONST_0    => callee.push(0d)
      case Opcode.DCONST_1    => callee.push(1d)
      case Opcode.BIPUSH      =>
        callee.push(code(i + 1).asInstanceOf[Int])
        return i + 2

      case (Opcode.LDC
          | Opcode.LDC2_W) =>
        val pool = pools.top._2
        if (opcode == Opcode.LDC) {
          val index = code(i + 1)
          callee.push(
            pool.getConstant(index).asInstanceOf[ConstantString].getBytes(pool)
          )
          return i + 2
        } else {
          val index = code(i + 1) << 8 | code(i + 2)
          callee.push(
            pool.getConstant(index).asInstanceOf[ConstantDouble].getBytes
          )
          return i + 3
        }

      case (Opcode.ILOAD
          | Opcode.LLOAD
          | Opcode.FLOAD
          | Opcode.DLOAD
          | Opcode.ALOAD) =>
        callee.push(callee.getLocals(code(i + 1)))

      case (Opcode.ILOAD_0
          | Opcode.LLOAD_0
          | Opcode.FLOAD_0
          | Opcode.DLOAD_0
          | Opcode.ALOAD_0) =>
        callee.push(callee.getLocals(0))

      case (Opcode.ILOAD_1
          | Opcode.LLOAD_1
          | Opcode.FLOAD_1
          | Opcode.DLOAD_1
          | Opcode.ALOAD_1) =>
        callee.push(callee.getLocals(1))

      case (Opcode.ILOAD_2
          | Opcode.LLOAD_2
          | Opcode.FLOAD_2
          | Opcode.DLOAD_2
          | Opcode.ALOAD_2) =>
        callee.push(callee.getLocals(2))

      case (Opcode.ILOAD_3
          | Opcode.LLOAD_3
          | Opcode.FLOAD_3
          | Opcode.DLOAD_3
          | Opcode.ALOAD_3) =>
        callee.push(callee.getLocals(3))

      case (Opcode.IALOAD
          | Opcode.LALOAD
          | Opcode.FALOAD
          | Opcode.DALOAD
          | Opcode.AALOAD
          | Opcode.BALOAD
          | Opcode.CALOAD
          | Opcode.SALOAD) =>
        callee.load(code(i + 1))

      case (Opcode.ISTORE
          | Opcode.LSTORE
          | Opcode.FSTORE
          | Opcode.DSTORE
          | Opcode.ASTORE) =>
        callee.store(code(i + 1))
        return i + 2

      case (Opcode.ISTORE_0
          | Opcode.ISTORE_1
          | Opcode.ISTORE_2
          | Opcode.ISTORE_3) =>
        callee.store(opcode.id - 0x3b)

      case (Opcode.LSTORE_0
          | Opcode.LSTORE_1
          | Opcode.LSTORE_2
          | Opcode.LSTORE_3) =>
        callee.store(opcode.id - 0x3f)

      case (Opcode.FSTORE_0
          | Opcode.FSTORE_1
          | Opcode.FSTORE_2
          | Opcode.FSTORE_3) =>
        callee.store(opcode.id - 0x43)

      case (Opcode.DSTORE_0
          | Opcode.DSTORE_1
          | Opcode.DSTORE_2
          | Opcode.DSTORE_3) =>
        callee.store(opcode.id - 0x47)

      case (Opcode.ASTORE_0
          | Opcode.ASTORE_1
          | Opcode.ASTORE_2
          | Opcode.ASTORE_3) =>
        callee.store(opcode.id - 0x4b)

      case (Opcode.IASTORE
          | Opcode.LASTORE
          | Opcode.FASTORE
          | Opcode.DASTORE
          | Opcode.AASTORE
          | Opcode.BASTORE
          | Opcode.CASTORE
          | Opcode.SASTORE) =>
        val v   = callee.pop[Any]
        val pos = callee.pop[Int]
        val arr = callee.pop[Array[Any]]
        arr(pos) = v

      case Opcode.POP  => callee.pop
      case Opcode.POP2 =>
        callee.pop
        callee.pop

      case Opcode.DUP  => callee.push(callee.peek)
      case Opcode.SWAP =>
        val a = callee.pop
        val b = callee.pop

        callee.push(a)
        callee.push(b)

      case Opcode.IINC =>
        val pos = code(i + 1)
        val old = callee.getLocalAt[Int](pos)
        callee.getLocals(pos) = old + code(i + 2)
        return i + 3

      case (Opcode.IFEQ
          | Opcode.IFNE
          | Opcode.IFLT
          | Opcode.IFGE
          | Opcode.IFGT
          | Opcode.IFLE) =>
        val i1 = callee.pop[Int]
        if (
          (opcode == Opcode.IFEQ && i1 == 0)
          || (opcode == Opcode.IFNE && i1 != 0)
          || (opcode == Opcode.IFLT && i1 < 0)
          || (opcode == Opcode.IFGE && i1 >= 0)
          || (opcode == Opcode.IFGT && i1 > 0)
          || (opcode == Opcode.IFLE && i1 <= 0)
        )
          return code(i + 1) << 8 | code(i + 2)

      case (Opcode.IF_ICMPEQ
          | Opcode.IF_ICMPNE
          | Opcode.IF_ICMPLT
          | Opcode.IF_ICMPGE
          | Opcode.IF_ICMPGT
          | Opcode.IF_ICMPLE) =>
        val i1     = callee.pop[Int]
        val i2     = callee.pop[Int]
        val offset = code(i + 1) << 8 | code(i + 2)
        if (
          (opcode == Opcode.IF_ICMPEQ && i1 == 2)
          || (opcode == Opcode.IF_ICMPNE && i1 != i2)
          || (opcode == Opcode.IF_ICMPLT && i2 < i1)
          || (opcode == Opcode.IF_ICMPGE && i2 >= i1)
          || (opcode == Opcode.IF_ICMPGT && i2 > i1)
          || (opcode == Opcode.IF_ICMPLE && i2 <= i1)
        ) return offset + i
        else return i + 3;

      case Opcode.GOTO =>
        return i + (code(i + 1) << 8 | code(i + 2))

      case Opcode.RETURN =>
        return code.length + 1

      case (Opcode.LRETURN
          | Opcode.IRETURN
          | Opcode.FRETURN
          | Opcode.DRETURN
          | Opcode.ARETURN) =>
        caller.push(callee.pop)

      case (Opcode.GETFIELD
          | Opcode.PUTFIELD) =>
        val offset       = code(i + 1) << 8 | code(i + 2) // Field to be accessed
        val top          = pools.top
        val currentKlass = top._1                         // Name of the current class we're in
        val pool         = top._2                         // Current class' constant pool
        val fieldRef     = pool.getConstant(offset).asInstanceOf[ConstantFieldref]
        val fieldKlass   = pool                           // Class in which the field exists
          .getConstant(fieldRef.getClassIndex)
          .asInstanceOf[ConstantClass]
          .getBytes(pool)

        assert(currentKlass == fieldKlass)
        val field = pool.getConstant(fieldRef.getNameAndTypeIndex)
          .asInstanceOf[ConstantNameAndType]
          .getName(pool)

        if (opcode == Opcode.GETFIELD) {
          callee.push(callee.pop[Klass].getField(field))
        } else {
          val value = callee.pop[Any]
          callee.pop[Klass].setField(field, value)
        }

        return i + 3

      case Opcode.INVOKESPECIAL =>
        callee.pop
        return i + 3

      case (Opcode.INVOKESTATIC
          | Opcode.INVOKEVIRTUAL) =>
        val _static      = opcode == Opcode.INVOKESTATIC
        val stackLength  = pools.length
        val method       = getMethod(code, i, parent, _static)
        var numArguments = method.getArgumentTypes.length
        if (opcode == Opcode.INVOKEVIRTUAL) {
          numArguments += 1
        }

        exec(method, callee.pop(numArguments))
        if (pools.length > stackLength)
          pools.pop()

        return i + 3

      case Opcode.NEW =>
        val offset = code(i + 1) << 8 | code(i + 2)
        val pool   = pools.top._2
        callee.push(
          loadKlass(
            pool.getConstant(offset).asInstanceOf[ConstantClass].getBytes(pool)
          ).get
        )
        return i + 3

      case (Opcode.ANEWARRAY
          | Opcode.NEWARRAY) =>
        val jump =
          if (opcode == Opcode.ANEWARRAY) {
            i + 3
          } else {
            i + 2
          }

        val length = callee.pop[Int]
        val array  = new Array[Any](length)
        callee.push(array)

        return jump

      case Opcode.ARRAYLENGTH => callee.push(callee.pop[Array[Any]].length)
      case Opcode.IFNULL      =>
        if (callee.pop == null) {
          return code(i + 1) << 8 | code(i + 2)
        }

      case Opcode.IFNONNULL =>
        if (callee.pop != null) {
          return code(i + 1) << 8 | code(i + 2)
        }
    }

    i + 1
  }

  def exec(method: Method, args: Array[Any]): Unit = {
    val parent = method.hashCode        /* Used for identification when inlining */
    var code   = method.getCode.getCode /* Bytecode */

    inline.get(parent) match {
      case Some(inlined) if frames.length <= 2 => code = inlined
      case _                                   =>
    }

    var i        = 0
    val len      = code.length
    val caller   = frames.top
    val escapees = EscapeAnalysis.perform(code)
    context(args) { callee =>
      while (i < len) {
        i = exec(code, i, parent, caller, callee)
      }

      println(s"VM: End of method ${callee.getLocals.mkString("Array(", ", ", ")")}")
    }
  }

  def run(): Unit = {
    loadKlass(vmargs.mainKlass) match {
      case Some(klass) =>
        pools.push((klass.name, klass.pool))
        klass.getMethod("main", PSVM_SIGNATURE) match {
          case Some(psvm) =>
            frames.push(new Frame(Array()))
            exec(psvm, Array())
          case None       =>
        }
      case None        =>
        throw new IllegalArgumentException("VM: Could not find klass " + vmargs.mainKlass)
    }
  }
}
