package necc.hyperneat

import necc.experiment.Experiment
import necc.experiment.Experiment.{CompleteTask, RadialEncoding}

class RadialCompleteFF extends RadialFF {
  def experiment = Experiment(RadialEncoding, CompleteTask)
}