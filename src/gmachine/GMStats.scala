package gmachine

object GMStats {
  
  val gmStatsInitial = new GMStats(0, 0)
  
}

class GMStats private (val getSteps : Int, val maxHeap : Int) {
  
  def admin(heapSize : Int) : GMStats = new GMStats(getSteps + 1, Math.max(heapSize, maxHeap))
  
}
