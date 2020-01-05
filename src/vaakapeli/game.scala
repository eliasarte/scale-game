package vaakapeli {
  
  import scala.collection.mutable.Buffer
  import scala.collection.mutable.Queue
  import scala.util.Random
  import java.awt.{Color,Graphics2D,BasicStroke}
  
  class Game {
    private val grid = Array(
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
			   
		private val players = Buffer[Player]()
    private var turn = Queue[Player]()
    private var pieces = Buffer[Piece]()
    private var scales = Buffer[Scale]()
    private var hardmode = false
    private var randomScale = 0
    private val colorMap = Map(Color.RED -> 4, Color.BLUE -> 5, Color.GREEN -> 6, Color.YELLOW -> 7, Color.BLACK -> 8)

    def apply(x: Int, y: Int): Int = grid(18 * y + x)
    
    def emptyBad(x: Int, y: Int) = grid(18 * y + x) = 0
  
    //called each turn to update game state
    def update(x: Int, y: Int) = {
      grid(18 * y + x) = colorMap(getTurn.getColor)
      val piece = pieces.filter(_.owner == None)(0)
      val possibleScales = scales.filter(_.location._2 == y + 1)
      var scalePart: Option[ScalePart] = None
      var scale: Option[Scale] = None
      for(i <- 0 until possibleScales.size) {
        val found = possibleScales(i).getParts.find(_.getLocation == (x, y + 1))
        if(found != None) {
          scale = Some(possibleScales(i))
          scalePart = found
        }
      }
      placePiece(piece, scale.get, scalePart.get)
      spawnNewScale
    }
    
    //bunch of simple methods to get variables or to update them
    def setHardmode = hardmode = true
    def updateScales(x: Int, y: Int) = grid(18 * y + x) = 2
    def badLocation(x: Int, y: Int) = grid(18 * y + x) = 3
    def getPlayers = players
    def setSpawnRate(n: Int) = randomScale = n
    def getTurn = turn(0)
    def getPlayersString = if(players.isEmpty) "None" else players.map(_.getName).toBuffer.mkString(", ")
    def getScales = scales
    def getPieces = pieces
    def piecesLeft = pieces.filter(_.owner == None).size
    
    def countPoints = {
      for(i <- 0 until players.size) {
        var result = 0
        for(j <- 0 until scales.size) {
          result += scales(j).countPoints(players(i))
        }
        players(i).updatePoints(result)
      }
    }
    
    //returns string that is used to show point situation in GUI
    def pointsString = {
      val result = Buffer[String]()
      if(players.isEmpty) result += "Empty"
      else {
        for(i <- 0 until players.size) {
          result += players(i).getName + " " + players(i).getPoints + " points"
        }
      }
      result.mkString(", ")
    }
    
    def createScale(size: Int) = {
      val parts = Buffer[ScalePart]()
      val newScale = new Scale
      newScale.updateLocation(size + scala.util.Random.nextInt(17 - size*2) + scala.util.Random.nextInt(2), 15)
      for(i <- 0 to size*2) {
        parts += new ScalePart(newScale)
      }
      newScale.addParts(parts.toVector)
      newScale.getParts.foreach(_.updateDistance)
      scales += newScale
      newScale
    }
    
    def createPieces(many: Int) = {
      for(i <- 0 until many) {
        pieces += new Piece
      }
    }
    
    def createPlayer(name: String, color: Color) = {
      val newPlayer = new Player(name, color)
      players += newPlayer
      turn += newPlayer
    }
    
    //updates scaleparts to have scale on them so we filter those parts out when spawning new scale somewhere
    def scaleOn = {
      var allParts = scales.map(_.getParts).flatten
      var middles = scales.map(_.getMiddle)
      for(i <- 0 until allParts.size) {
        val location = allParts(i).getLocation
        if(allParts.find(_.getLocation == (location._1, location._2 - 3)) != None) {
          allParts(i).updateScaleOn(true)
          val found = middles.find(_.getLocation == (location._1, location._2 - 3))
          if(found != None) allParts(i).updateScaleOnPart(found.get.belong)
        }
        if(allParts.find(_.getLocation == (location._1 - 1, location._2 - 3)) != None) allParts(i).updateScaleOn(true)
        if(allParts.find(_.getLocation == (location._1 + 1, location._2 - 3)) != None) allParts(i).updateScaleOn(true)
      }
    } 
    
    def spawnNewScale: Unit = {
      val rand = new Random
      val spawn = rand.nextInt(100) + 1 <= randomScale
      var noSpace = false
      var tries = 0
      scaleOn
      if(spawn) {
        var newScaleSize = rand.nextInt(3) + 1
        var allParts = scales.map(_.getParts).flatten //gets all parts
                        .filter(_.getLocation._2 > 3) //not too high
                        .filterNot(_.getLocation._1 < newScaleSize) //not too left
                        .filterNot(_.getLocation._1 > 17 - newScaleSize) //not too right
                        .filter(_.getPieces.isEmpty) //no piece on part
        var middles = scales.map(_.getMiddle) //cant set scale in middle
        var scaleOns = scales.map(_.getParts).flatten.filter(_.getScaleOn) //remove reserved spaces
        allParts = (allParts -- middles) -- scaleOns
        var location = (0, 0)
        def findLocation: Unit = {
          if(!allParts.isEmpty) {
            var part = allParts(rand.nextInt(allParts.size))
            location = part.getLocation
            for(i <- - newScaleSize - 1 to newScaleSize + 1) {
              if(this(location._1 + i, location._2 - 3) != 0) {
                tries += 1
                if(tries < 50) {
                  allParts = allParts.filterNot(_ == part)
                  if(!allParts.isEmpty) findLocation
                  else noSpace = true
                }
                else noSpace = true
              }
            }
          }
        }
        findLocation
        if(!noSpace && location != (0, 0)) {
          createScale(newScaleSize)
          scales.last.updateLocation(location._1, location._2 - 3)
          scaleOn
        }
        else if(newScaleSize > 1) {
          newScaleSize -= 1
          findLocation
        }
      }
    }
    
    def placePiece(piece: Piece, scale: Scale, part: ScalePart) = {
      val foundScale = scales.find(_ == scale)
      if(foundScale != None) {
        val foundPart = foundScale.get.getParts.find(_ == part)
        if(foundPart != None && piece.owner == None) {
          foundPart.get.addPiece(piece)
          scales.foreach(_.calculateBalance)
          if(scales.find(_.getBalance == false) != None) {
            if(hardmode) { //dropping all pieces
              foundPart.get.changePiecesOwner(getTurn)
              dropPieces(getTurn)
            }
            else { //dropping single piece
              pieces = pieces.filterNot(_ == piece)
              foundPart.get.removePiece(piece)
              val y = foundPart.get.getLocation._2 - 1
              val x = foundPart.get.getLocation._1
              grid(18 * y + x) = colorMap(turn.last.getColor)
              if(foundPart.get.getPieces.size == 0) emptyBad(piece.getLocation.get.getLocation._1, piece.getLocation.get.getLocation._2 - 1)
            }
          }
          else foundPart.get.changePiecesOwner(getTurn)
          countPoints
          advanceTurn
        }
      }
    }
    
    //used to drop all pieces in hardmode
    def dropPieces(player: Player) = {
      val toDrop = pieces.filter(_.owner == Some(player))
      for(i <- 0 until toDrop.size) {
        emptyBad(toDrop(i).getLocation.get.getLocation._1, toDrop(i).getLocation.get.getLocation._2 - 1)
      }
      pieces = pieces.filterNot(_.owner == Some(player))
      scales.foreach(_.removePieces(player))  
    }
    
    def advanceTurn = turn = (turn ++ Queue(turn.dequeue()))
    
    //returns winner string for GUI use
    def getWinner = {
      val playersCopy = Buffer[Player]()
      players.copyToBuffer(playersCopy)
      val winners = Buffer[Player]()
      val winner = playersCopy.maxBy(_.getPoints)
      winners += winner
      playersCopy -= winner
      def findWinner: Unit = {
        val next = playersCopy.maxBy(_.getPoints)
        if(winner.getPoints == next.getPoints) {
          winners += next
          playersCopy -= next
          if(!playersCopy.isEmpty) findWinner
        } 
      }
      if(!playersCopy.isEmpty) findWinner
      winners.map(_.getName).mkString(", ")
    }
  }
}