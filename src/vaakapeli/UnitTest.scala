package vaakapeli 

import org.junit.Test
import org.junit.Assert._
import java.awt.{Color,Graphics2D,BasicStroke}


class UnitTest {

  val game = new Game
  
  game.createScale(5)
  game.createPieces(10)
  game.createPlayer("Player1", Color.RED)
  game.createPlayer("Player2", Color.BLACK)
  
  val pieces = game.getPieces
  val scales = game.getScales
  val location = scales(0).getMiddle.getLocation //middle location -5, +5
  
  @Test def pieceSet = {
    assert(game.getTurn == game.getPlayers(0))
    game.placePiece(pieces(0), scales(0), scales(0).getParts(0))
    assert(game.getTurn == game.getPlayers(1))
    assert(!scales(0).getParts(0).getPieces.isEmpty)
  }
  
  @Test def pieceSet2 = {
    assert(game.getTurn == game.getPlayers(0))
    game.placePiece(pieces(0), scales(0), scales(0).getParts(0))
    assert(game.getTurn == game.getPlayers(1))
    game.placePiece(pieces(1), scales(0), scales(0).getParts(10))
    assert(game.getTurn == game.getPlayers(0))
    game.placePiece(pieces(2), scales(0), scales(0).getParts(0))
    assert(!scales(0).getParts(0).getPieces.isEmpty)
    assert(!scales(0).getParts(10).getPieces.isEmpty)
    assert(scales(0).getParts(0).getPieces.size == 2)
  }

  @Test def invalidPiece = {
    game.placePiece(pieces(0), scales(0), scales(0).getParts(0))
    assert(!scales(0).getParts(0).getPieces.isEmpty) //should contain piece
    game.placePiece(pieces(0), scales(0), scales(0).getParts(1))
    assert(scales(0).getParts(1).getPieces.isEmpty) //should be empty because piece was invalid (pieces(0))
  }

  //only 1 scale in hardmode
  @Test def balancePoints1 = {
    game.setHardmode
    game.update(location._1 + 5, location._2 - 1) //balance = 5
    assert(scales(0).getBalance)
    assert(scales(0).getWeight == 5)
    assert(game.getPlayers(0).getPoints == 5)
    
    game.update(location._1 - 5, location._2 - 1) //balance = 0
    assert(scales(0).getBalance)
    assert(scales(0).getWeight == 0)
    assert(game.getPlayers(1).getPoints == 5)
    
    game.update(location._1 - 5, location._2 - 1) //balance = -5
    assert(scales(0).getBalance)
    assert(scales(0).getWeight == -5)
    assert(game.getPlayers(0).getPoints == 15)
    assert(game.getPlayers(1).getPoints == 0)
    
    game.update(location._1 - 5, location._2 - 1) //balance = -10
    assert(!scales(0).getBalance)                 //not balanced
    assert(scales(0).getWeight == -10)
    assert(game.getPlayers(0).getPoints == 5)
    assert(game.getPlayers(1).getPoints == 0)
    
    game.update(location._1 - 5, location._2 - 1) //balance = 0
    assert(scales(0).getBalance)                  //balanced
    assert(scales(0).getWeight == 0)
    assert(game.getPlayers(0).getPoints == 10)
    assert(game.getPlayers(1).getPoints == 0)
  }
  
  //two scales in hardmode
  @Test def balancePoints2 = {
    game.setHardmode
    game.update(location._1 + 5, location._2 - 1) //balance = 5
    game.update(location._1 - 5, location._2 - 1) //balance = 0
    game.setSpawnRate(100)
    game.spawnNewScale
    val location2 = scales(1).getMiddle.getLocation
    game.update(location2._1 + scales(1).getParts.size/2, location2._2 - 1)
    val distance = scala.math.abs(scales(0).getMiddle.getLocation._1 - scales(1).getMiddle.getLocation._1) 
    assert(game.getPlayers(0).getPoints == 5 + scales(1).getParts.size/2 + scales(1).getParts.size/2 * distance)
  }
  
  //normalmode
  @Test def balancePoints3 = {
    game.update(location._1 + 5, location._2 - 1) //balance = 5
    game.update(location._1 - 5, location._2 - 1) //balance = 0
    game.update(location._1 + 5, location._2 - 1) //balance = 5
    game.update(location._1 + 5, location._2 - 1) //balance = 10
    assert(!scales(0).getBalance)                 //not balanced
    assert(scales(0).getParts.last.getPieces.size == 2)
    assert(game.getPlayers(0).getPoints == 10)
    assert(game.getPlayers(1).getPoints == 5)
  }
}
