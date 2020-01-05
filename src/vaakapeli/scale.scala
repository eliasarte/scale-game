package vaakapeli {
  
  class Scale {
    
    private var parts = Vector[ScalePart]()
    private var isBalanced = true
    private var totalWeight = 0
    private var middle = (0, 0)
    
    def location = middle
    def getParts = parts
    def getMiddle = parts(parts.size/2)
    def getWeight = totalWeight
    def getBalance = isBalanced
    
    def countPieces = parts.map(_.getPieces).flatten.size
    
    def addParts(part: Vector[ScalePart]) = parts = part
    
    def removePieces(who: Player) = parts.foreach(_.removePieces(who))
    
    def updateLocation(x: Int, y: Int) = {
      middle = (x, y)
      parts.foreach(_.updateDistance)
    }
    
    def countPoints(who: Player): Int = {
      var result = 0
      for(i <- 0 until parts.size) {
        result += parts(i).countPoints(who)
      }  
    result
    }
    
    def calculateBalance: Boolean = {
      totalWeight = 0
      parts.foreach(_.updateWeight)
      for(i <- 0 until parts.size) {
        totalWeight += parts(i).getWeight
      }
      isBalanced = (parts.size / 2) >= scala.math.abs(totalWeight)
      isBalanced
    }
  }
}