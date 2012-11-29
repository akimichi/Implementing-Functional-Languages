package template

object TiStats {
  
  val tiStatsInitial = new TiStats(0, 0)
  
}

class TiStats private (val getSteps : Int, val maxHeap : Int) {
  
  def admin(heapSize : Int) : TiStats = new TiStats(getSteps + 1, Math.max(heapSize, maxHeap))
  
}
