package necc.hyperneat

import necc.experiment.Experiment
import necc.experiment.Experiment.{GridEncoding, LoadBlocksTask}

class GridLoadBlocksFF extends GridFF {
  def experiment = Experiment(GridEncoding, LoadBlocksTask)
}