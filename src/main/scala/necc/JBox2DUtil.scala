package necc

import org.jbox2d.common.MathUtils
import org.jbox2d.dynamics.Body
import org.jbox2d.dynamics.contacts.{Contact, ContactEdge}

object JBox2DUtil {
  implicit class RichBody(body: Body) {
    def contactsStream = {
      def fromCE(ce : ContactEdge): Stream[Contact] = ce.contact match {
        case null => Stream.empty
        case c => c #:: fromCE(ce.next)
      }
      fromCE(body.getContactList)
    }

    def contacts = {
      def fromCE(ce: ContactEdge): List[Contact] = ce.contact match {
        case null => Nil
        case c => c :: fromCE(ce.next)
      }
      fromCE(body.getContactList)
    }
  }

  type Vec2 = org.jbox2d.common.Vec2
  implicit class RichVec2(v: Vec2) {
    def +(other: Vec2) = v.add(other)
    def +=(other: Vec2) = v.addLocal(other)
    def +=(x: Float, y: Float) = v.addLocal(x, y)
    def -(other: Vec2) = v.sub(other)
    def -=(other: Vec2) = v.subLocal(other)
    def -=(x: Float, y: Float) = v.addLocal(-x, -y)
    def *(a: Float) = v.mul(a)
    def *=(a: Float) = v.mulLocal(a)
    def dot(other: Vec2) = org.jbox2d.common.Vec2.dot(v, other)
  }
  object Vec2 {
    def apply(x: Float, y: Float) = new Vec2(x, y)
    def zero = Vec2(0, 0)
    def unitX = Vec2(1, 0)
    def unitY = Vec2(0, 1)
  }

  type Vec3 = org.jbox2d.common.Vec3
  implicit class RichVec3(v: Vec3) {
    def +(other: Vec3) = v.add(other)
    def +=(other: Vec3): Unit = v.addLocal(other)
    def -(other: Vec3) = v.sub(other)
    def -=(other: Vec3): Unit = v.subLocal(other)
    def *(a: Float) = v.mul(a)
    def *=(a: Float): Unit = v.mulLocal(a)
  }
  object Vec3 {
    def apply(x: Float, y: Float, z: Float) = new Vec3(x, y, z)
    val zero = Vec3(0, 0, 0)
    val unitX = Vec3(1, 0, 0)
    val unitY = Vec3(0, 1, 0)
    val unitZ = Vec3(0, 0, 1)
  }

  class EdgeVec2(p0: Vec2, p1: Vec2) {
    def mid = (p0 + p1) *= 0.5f
    def angle = MathUtils.atan2(p1.y - p0.y, p1.x - p0.x)
    def lengthSq = (p1.x + p0.x)*(p1.x + p0.x) + (p1.y + p0.y)*(p1.y + p0.y)
    def length = { val ls = lengthSq; ls * ls }

    def map(f: Vec2 => Vec2) = new EdgeVec2(f(p0), f(p1))
  }
}
