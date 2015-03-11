package com.anji.neat

import sbinary.{Format, DefaultProtocol}
import necc.JavaLongProtocol._
import necc.ChromosomeMaterialProtocol._

object NeuronGeneProtocol extends DefaultProtocol {
  implicit lazy val NeuronGeneFormat: Format[NeuronGene] = asProduct3[NeuronGene, java.lang.Long, NeuronType, String](
    (innovation, nt, act) => new NeuronGene(nt, innovation, act)
  )(g => (g.getInnovationId, g.getType, g.getActivationType))
}
