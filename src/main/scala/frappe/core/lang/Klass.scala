package frappe.core.lang

import org.apache.bcel.classfile.{ConstantPool, JavaClass, Method}

import scala.collection.mutable

class Klass(javaClass: JavaClass) {
  val name: String                         = javaClass.getClassName
  val pool: ConstantPool                   = javaClass.getConstantPool
  val fields: mutable.HashMap[String, Any] = mutable.HashMap[String, Any]()
  val methods: Array[Method]               = javaClass.getMethods

  def getMethod(name: String, signature: String): Option[Method] =
    methods.find(m => m.getName == name && m.getSignature == signature)
  def getField(name: String): Any                                = fields.getOrElse(name, null)
  def setField(name: String, value: Any): Unit                   = fields.update(name, value)
}
