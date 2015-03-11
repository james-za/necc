package necc.hyperneat

import necc.experiment.Experiment
import necc.experiment.Experiment.{GridEncoding, PlaceBlocksTask}

class GridPlaceBlocksFF extends GridFF {
  def experiment = Experiment(GridEncoding, PlaceBlocksTask)
}