package necc

import com.amd.aparapi.Kernel
import com.ojcoleman.ahni.nn.BainNN
import com.ojcoleman.ahni.nn.BainNN.Topology
import com.ojcoleman.ahni.util.Point
import com.ojcoleman.bain.NeuralNetwork
import com.ojcoleman.bain.base._
import com.ojcoleman.bain.neuron.rate.ClampedLinearNeuronCollection
import com.ojcoleman.bain.synapse.rate.FixedSynapseCollection
import sbinary.{DefaultProtocol, Format}

object BainNNProtocol extends DefaultProtocol {

  implicit lazy val BainNNFormat: Format[BainNN] = asProduct8 {
    (nn: NeuralNetwork, inDim: Array[Int], outDim: Array[Int], sps: Int,
     topo: Topology, name: String, mcl: Int, coords: Array[Point]) =>
      val b = new BainNN(nn, inDim, outDim, sps, topo, name, mcl)
      if(coords.nonEmpty) {
        b.enableCoords()
        coords.zipWithIndex foreach { case (p, i) => b.setCoords(i, p.x, p.y, p.z) }
      }
      b.reset()
      b
  } {
    b =>
      val coords = if(b.coordsEnabled)
        (0 until b.getNeuronCount).map(b.getCoord).toArray
      else
        Array[Point]()
      (b.getNeuralNetwork, b.getInputDimension, b.getOutputDimension,
        b.getStepsPerStep, b.getTopology, b.getName, b.getMaxCycleLength,
        coords)
  }

  implicit lazy val PointFormat: Format[Point] = asProduct3 {
    (x: Double, y: Double, z: Double) => new Point(x, y, z)
  } {
    p => (p.x, p.y, p.z)
  }

  implicit lazy val TopologyFormat: Format[Topology] = wrap[Topology, Int](
    v => if(v == null) -1 else v.ordinal(),
    i => if(i == -1) null else Topology.values()(i))

  implicit lazy val NeuralNetworkFormat: Format[NeuralNetwork] = asProduct4 {
    (timeRes: Int,
     neurons: NeuronCollection[_ <: NeuronConfiguration],
     synapses: SynapseCollection[_ <: SynapseConfiguration],
     execMode: Kernel.EXECUTION_MODE) =>
      val nn = new NeuralNetwork(timeRes, neurons, synapses, execMode)
      neurons.setNetwork(nn)
      synapses.setNetwork(nn)
      nn
  } {
    nn => (nn.getTimeResolution,
      nn.getNeurons.asInstanceOf[NeuronCollection[_ <: NeuronConfiguration]],
      nn.getSynapses.asInstanceOf[SynapseCollection[_ <: SynapseConfiguration]],
      nn.getPreferredExecutionMode)
  }

  implicit lazy val KernelExecutionModeFormat: Format[Kernel.EXECUTION_MODE] = wrap[Kernel.EXECUTION_MODE, Int](
    v => if(v == null) -1 else v.ordinal(),
    i => if(i == -1) null else Kernel.EXECUTION_MODE.values()(i))

  implicit lazy val ClampedLinearNeuronCollectionFormat: Format[ClampedLinearNeuronCollection] = asProduct5 {
    (size: Int, bias: Array[Double], spikings: Array[Boolean],
     configs: Array[NeuronConfiguration], ccIndices: Array[Int]) =>
      val c = new ClampedLinearNeuronCollection(size)
      bias.zipWithIndex.foreach { case (v, i) => c.setBias(i, v) }
      spikings.zipWithIndex.foreach { case (s, i) => if (s) c.spiked(i) }
      configs.foreach(c.addConfiguration(_))
      ccIndices.zipWithIndex.foreach { case (cc, i) => c.setComponentConfiguration(i, cc) }
      c.init()
      c
  } {
    c =>
      val size = c.getSize
      val bias = (0 until size).map(c.getBias).toArray
      val configs = (0 until c.getConfigurationCount).map(c.getConfiguration).toArray
      val ccIndices = (0 until size).map(c.getComponentConfigurationIndex).toArray
      (size, bias, c.getSpikings, configs, ccIndices)
  }

  implicit lazy val NeuronCollectionFormat: Format[NeuronCollection[_ <: NeuronConfiguration]] = asUnion(
    ClampedLinearNeuronCollectionFormat)

  implicit lazy val NeuronConfigurationFormat: Format[NeuronConfiguration] = asUnion()

  implicit lazy val FixedSynapseCollectionFormat: Format[FixedSynapseCollection] = asProduct6 {
    (size: Int, weights: Array[Double], preIndices: Array[Int], postIndices: Array[Int],
      configs: Array[SynapseConfiguration], ccIndices: Array[Int]) =>
      val c = new FixedSynapseCollection(size)
      weights.zipWithIndex.foreach { case (w, i) => c.setEfficacy(i, w) }
      preIndices.zipWithIndex foreach { case (p, i) => c.setPreNeuron(i, p) }
      postIndices.zipWithIndex foreach { case (p, i) => c.setPostNeuron(i, p) }
      configs.foreach(c.addConfiguration)
      ccIndices.zipWithIndex foreach { case (cc, i) => c.setComponentConfiguration(i, cc) }
      c.init()
      c
  } {
    c =>
      val size = c.getSize
      val preIndices = (0 until size).map(c.getPreNeuron).toArray
      val postIndices = (0 until size).map(c.getPostNeuron).toArray
      val configs = (0 until c.getConfigurationCount).map(c.getConfiguration).toArray
      val ccIndices = (0 until size).map(c.getComponentConfigurationIndex).toArray
      (size, c.getEfficacies, preIndices, postIndices, configs, ccIndices)
  }

  implicit lazy val SynapseCollectionFormat: Format[SynapseCollection[_ <: SynapseConfiguration]] = asUnion(
    FixedSynapseCollectionFormat)

  implicit lazy val SynapseConfigurationFormat: Format[SynapseConfiguration] = asUnion()
}
