package template

object TiStats {
  
  def tiStatsInitial = new TiStats(0)
  
}

class TiStats private (val getSteps : Int) {
  
  def incSteps : TiStats = new TiStats(getSteps + 1)
  
}
