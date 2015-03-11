package necc

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class Consumable[T](ts: Seq[T]) {
  val buf = mutable.Queue[T](ts: _*)

  def +=(t: T): Unit = buf += t
  def ++=(ts: Seq[T]): Unit = buf ++= ts

  def consumeAll: Seq[T] = {
    val ts = buf.toVector
    buf.clear()
    ts
  }

  def consume(): T = buf.dequeue()
  
  def consumeAll[R](f: T => R): Unit = {
    buf.foreach(f)
    buf.clear()
  }
  
  def consume(f: T => Unit): Unit = f(buf.dequeue())
}

object Consumable {
  def apply[T](ts: T*) = new Consumable[T](ts)
}
