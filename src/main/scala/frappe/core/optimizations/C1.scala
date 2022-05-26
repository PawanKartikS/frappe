package frappe.core.optimizations

import org.apache.bcel.classfile.Method

import scala.collection.mutable

class C1(maxThreshold: Int) {
  private val _threshold = mutable.HashMap[String, Int]()    // Track thresholds
  private val _cache     = mutable.HashMap[String, Method]() // C1 cache

  def cache(key: String, method: Method): Unit = _cache.put(key, method)
  def cached(key: String): Boolean             = _cache.contains(key)
  def exceedsThreshold(key: String): Boolean   = _threshold.getOrElse(key, 0) >
    maxThreshold
  def get(key: String): Option[Method]         = _cache.get(key)
  def incrementThreshold(key: String): Unit    = _threshold.put(
    key,
    _threshold
      .getOrElse(key, 0) + 1
  )
}
