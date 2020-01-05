package vaakapeli {
  
  import scala.collection.mutable.Buffer
  
  class ScalePart(scale: Scale) {
    private var pieces = Buffer[Piece]()
    private var belongsTo = scale
    private var distance = 0
    private var scaleOn = false
    private var scaleOnPart: Option[Scale] = None
    private var totalWeight = 0
    private var owner: Option[Player] = None
    private var location = (0, 0)
    
    def getLocation = location
    def getDistance = distance
    def getWeight = totalWeight   
    def getPieces = pieces
    def belong = belongsTo
    def getScaleOn = scaleOn
    def getScaleOnPart = scaleOnPart
    
    def updateScaleOn(on: Boolean) = scaleOn = on
    
    def updateScaleOnPart(on: Scale) = scaleOnPart = Some(on)
    
    def changePiecesOwner(who: Player) = {
      owner = Some(who)
      pieces.foreach(_.setOwner(who))
    }
    
    def addPiece(piece: Piece) = {
      pieces += piece
      scaleOn = true
      piece.setLocation(Some(this))
    }
    
    def removePiece(piece: Piece) = {
      pieces = pieces.dropRight(1)
      if(pieces.isEmpty) scaleOn = false
      updateWeight
    }
    
    def removePieces(who: Player) = {
      pieces = pieces.filterNot(_.owner == Some(who))
      scaleOn = false
      updateWeight
    }
    
    //updates the distance to the middle part after created.
    def updateDistance = {
      val leftSide = belongsTo.getParts.splitAt(belongsTo.getParts.size / 2)._1.reverse
      val rightSide = belongsTo.getParts.splitAt(belongsTo.getParts.size / 2)._2.takeRight(belongsTo.getParts.size/2)
      val middle = belongsTo.getParts.splitAt(belongsTo.getParts.size / 2)._2(0)
      distance = if(middle == this) 0 else if(leftSide.indexOf(this) != -1) leftSide.indexOf(this) * -1 - 1 else rightSide.indexOf(this) + 1
      if(middle == this) scaleOn = true
      location = (scale.location._1 + distance, scale.location._2)
    }
    
    def countPoints(who: Player): Int = {
      if(owner != None && owner.get == who) pieces.size * scala.math.abs(distance)
      else if(scaleOnPart != None) scaleOnPart.get.countPoints(who) * scala.math.abs(distance)
      else 0
    }
    
    def updateWeight = totalWeight = if(scaleOnPart != None) scaleOnPart.get.countPieces * distance else pieces.size * distance
  } 
}