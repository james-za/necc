package necc.hyperneat

import necc.experiment.Experiment
import necc.experiment.Experiment.{CompleteTask, GridEncoding}

class GridCompleteFF extends GridFF {
  def experiment = Experiment(GridEncoding, CompleteTask)
}