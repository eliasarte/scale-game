package vaakapeli {
  
import java.awt.{Color,Graphics2D,BasicStroke}
  
  class Player(n: String, c: Color) {
    
    private val name = this.n
    private val color = this.c
    private var points = 0
    
    def updatePoints(n: Int) = points = n
    
    def getName = name
    
    def getColor = color
    
    def getPoints = points
  }
}