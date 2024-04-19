package ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance: Unit =
  /* Linear sequences: List, ListBuffer */
  var list: List [Int] = List(1, 2, 3, 4, 5)  // Immutable.
  list.foreach(println)
  list = list :+ 6
  list = list.take(5)

  val listBuffer = ListBuffer[Int](1, 2, 3, 4, 5) // Mutable.
  listBuffer.foreach(println)
  listBuffer.addOne(6)
  listBuffer.remove(5)

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  var vector: Vector[Int] = Vector(1, 2, 3, 4, 5) // Immutable.
  vector.foreach(println)
  vector = vector :+ 6
  vector = vector.take(5)

  var array: Array[Int] = Array(1, 2, 3, 4, 5)  // Mutable.
  array.foreach(println)
  array(0) = 0
  array = array.take(5)

  val arrayBuffer: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer[Int](1, 2, 3, 4, 5) // Mutable.
  arrayBuffer.foreach(println)
  arrayBuffer.addOne(6)
  arrayBuffer.remove(5)

  /* Sets */
  var immutableSet: Set[Int] = Set(1, 2, 3, 4, 5) // Immutable.
  immutableSet.foreach(println)
  immutableSet = immutableSet + 6
  immutableSet = immutableSet.take(4)

  val mutableSet: mutable.Set[Int] = mutable.Set(1, 2, 3, 4, 5) // Mutable.
  mutableSet.foreach(println)
  mutableSet.add(6)
  mutableSet.remove(5)

  /* Maps */
  var immutableMap: Map[Int, String] = Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e")  // Immutable.
  immutableMap.foreach(println)
  immutableMap = immutableMap + (6 -> "f")
  immutableMap = immutableMap.take(4)

  val mutableMap: mutable.Map[Int, String] = mutable.Map[Int, String](1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e") // Mutable.
  mutableMap.foreach(println)
  mutableMap.addOne(6 -> "f")
  mutableMap.remove(6)

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 100000).toList
  val vec = (1 to 100000).toVector
  val set = (1 to 100000).toSet
  val map = (1 to 100000).map(x => (x, x)).toMap

  // Performance tests.
  assert(measure("list last")(lst.last) > measure("vector last")(vec.last))
  assert(measure("set last")(set.last) > measure("vector last")(vec.last))
  assert(measure("map last")(map.last) > measure("vector last")(vec.last))
  assert(measure("map last")(map.last) > measure("set last")(set.last))