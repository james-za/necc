package necc

import com.anji.neat._
import org.jgapcustomised.impl.{IntegerAllele, BooleanAllele}
import org.jgapcustomised.{Gene, Allele, ChromosomeMaterial}
import sbinary.{DefaultProtocol, Format}
import necc.JavaLongProtocol._
import NeuronGeneProtocol.NeuronGeneFormat

import scala.collection.mutable
import scala.collection.JavaConverters._

object ChromosomeMaterialProtocol extends DefaultProtocol {
  implicit lazy val ChromosomeMaterialFormat: Format[ChromosomeMaterial] = wrap[ChromosomeMaterial, mutable.Set[Allele]](
    _.getAlleles.asScala,
    mats => {
      val set = new java.util.TreeSet[Allele]
      for (mat <- mats)
        set.add(mat)
      new ChromosomeMaterial(set)
    }
  )

  implicit lazy val AlleleFormat: Format[Allele] = asUnion(BooleanAlleleFormat, ConnectionAlleleFormat, IntegerAlleleFormat, NeuronAlleleFormat)
  implicit lazy val BooleanAlleleFormat: Format[BooleanAllele] = wrap[BooleanAllele, Boolean](
    _.booleanValue,
    bool => {
      val allele = new BooleanAllele()
      allele.setValue(if (bool) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
      allele
    }
  )
  implicit lazy val ConnectionAlleleFormat: Format[ConnectionAllele] = asProduct2[ConnectionAllele, ConnectionGene, Double] {
    (gene, value) =>
      val allele = new ConnectionAllele(gene)
      allele.setWeight(value)
      allele
  } { allele =>
    val gene = new ConnectionGene(allele.getInnovationId, allele.getSrcNeuronId, allele.getDestNeuronId)
    (gene, allele.getWeight)
  }
  implicit lazy val IntegerAlleleFormat: Format[IntegerAllele] = wrap[IntegerAllele, String](_.getPersistentRepresentation, str => {
    val allele = new IntegerAllele
    allele.setValueFromPersistentRepresentation(str)
    allele
  })
  implicit lazy val NeuronAlleleFormat: Format[NeuronAllele] = asProduct2[NeuronAllele, NeuronGene, Double] {
    (gene, value) => new NeuronAllele(gene, value)
  } { allele =>
    val gene = new NeuronGene(allele.getType, allele.getInnovationId, allele.getActivationType)
    (gene, allele.getBias)
  }

  implicit lazy val GeneFormat: Format[Gene] = asUnion(ConnectionGeneFormat, NeuronGeneFormat/*, ParamGeneFormat*/)
  implicit lazy val ConnectionGeneFormat: Format[ConnectionGene] = asProduct3[ConnectionGene, java.lang.Long, java.lang.Long, java.lang.Long](
    (innovation, src, dst) => new ConnectionGene(innovation, src, dst)
  )(g => (g.getInnovationId, g.getSrcNeuronId, g.getDestNeuronId))
  
  implicit lazy val NeuronTypeFormat: Format[NeuronType] = wrap[NeuronType, Int] ({
    case NeuronType.HIDDEN => 0
    case NeuronType.INPUT => 1
    case NeuronType.OUTPUT => 2
  }, {
    case 0 => NeuronType.HIDDEN
    case 1 => NeuronType.INPUT
    case 2 => NeuronType.OUTPUT
  })
}
