package necc.hyperneat

import necc.experiment.Experiment
import necc.experiment.Experiment.{LoadBlocksTask, RadialEncoding}

class RadialLoadBlocksFF extends RadialFF {
  def experiment = Experiment(RadialEncoding, LoadBlocksTask)
}