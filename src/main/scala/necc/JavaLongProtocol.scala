package necc

import sbinary.{Format, DefaultProtocol}

object JavaLongProtocol extends DefaultProtocol {
  implicit lazy val JavaLongFormat: Format[java.lang.Long] = wrap[java.lang.Long, Long](Long2long, long2Long)
}
