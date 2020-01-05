package vaakapeli {
  
  class Piece {
    private var ownerP: Option[Player] = None
    private var location: Option[ScalePart] = None
    
    def setOwner(who: Player) = ownerP = Some(who)
    
    def owner = ownerP
    
    def getLocation = location
    
    def setLocation(loc: Option[ScalePart]) = location = loc
  }
}