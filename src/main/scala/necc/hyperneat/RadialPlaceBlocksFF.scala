package necc.hyperneat

import necc.experiment.Experiment
import necc.experiment.Experiment.{PlaceBlocksTask, RadialEncoding}

class RadialPlaceBlocksFF extends RadialFF {
  def experiment = Experiment(RadialEncoding, PlaceBlocksTask)
}