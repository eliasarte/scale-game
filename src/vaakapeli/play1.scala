package vaakapeli

import scala.swing._
import scala.swing.event._
import javax.swing._
import java.awt.{Color,Graphics2D,BasicStroke}
import java.awt.geom._

class Canvas(val running: Game) extends Component {
  
  override def paintComponent(g: Graphics2D) = {
     
    val d = new Dimension(100, 100) //size for squares / 3
    val squareSide = d.height.min(d.width)
    val wid = squareSide / 3
    val x0 = (d.width - squareSide)/2
    val y0 = (d.height - squareSide)/2
    g.setColor(Color.BLACK)
    //vertical lines
    for(x <- 1 to 18) {
      g.draw(new Line2D.Double(x0 + x * wid, y0, x0 + x * wid, y0 + squareSide * 6 - 6))
    }
    //horizontal lines
    for(y <- 1 to 18) {
      g.draw(new Line2D.Double(x0, y0 + y * wid, x0 + squareSide * 6 - 6, y0 + y * wid))
    }
    //drawing scales, player pieces...
    for(x <- 0 until 18;
        y <- 0 until 18)
    {
	      running(x, y) match {
	        case 2 => { //scaleparts
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.setColor(Color.LIGHT_GRAY)
	          g.fill(new Rectangle2D.Double(x1 + 1, y1 + 1, wid - 1, wid - 1))
	        }
	        case 3 => { //bad locations
	          g.setColor(Color.RED)
	          g.setStroke(new BasicStroke(3f))
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.draw(new Line2D.Double(x1 + 10, y1 + 10, x1 + wid - 10, y1 + wid - 10))
	          g.draw(new Line2D.Double(x1 + 10, y1 + wid - 10, x1 + wid - 10, y1 + 10))
	        }
	        case 4 => { //red piece
	          g.setColor(Color.RED)
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.fill(new Rectangle2D.Double(x1 + 5, y1 + 15, wid - 9, wid - 15))
	          val pieces = running.getScales.map(_.getParts).flatten.filter(_.getLocation == (x, y + 1))(0).getPieces.size
	          g.setColor(Color.WHITE)
	          g.drawString(pieces.toString(), x1 + 13, y1 + 27)
	        }
	        case 5 => { //blue piece
	          g.setColor(Color.BLUE)
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.fill(new Rectangle2D.Double(x1 + 5, y1 + 15, wid - 9, wid - 15))
	          val pieces = running.getScales.map(_.getParts).flatten.filter(_.getLocation == (x, y + 1))(0).getPieces.size
	          g.setColor(Color.WHITE)
	          g.drawString(pieces.toString(), x1 + 13, y1 + 27)	          
	        }
	         case 6 => { //green piece
	          g.setColor(Color.GREEN)
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.fill(new Rectangle2D.Double(x1 + 5, y1 + 15, wid - 9, wid - 15))
	          val pieces = running.getScales.map(_.getParts).flatten.filter(_.getLocation == (x, y + 1))(0).getPieces.size
	          g.setColor(Color.WHITE)
	          g.drawString(pieces.toString(), x1 + 13, y1 + 27)	          
	        }
	        case 7 => { //yellow piece
	          g.setColor(Color.YELLOW)
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.fill(new Rectangle2D.Double(x1 + 5, y1 + 15, wid - 9, wid - 15))
	          val pieces = running.getScales.map(_.getParts).flatten.filter(_.getLocation == (x, y + 1))(0).getPieces.size
	          g.setColor(Color.BLACK)
	          g.drawString(pieces.toString(), x1 + 13, y1 + 27)	          
	        }
	        case 8 => { //black piece
	          g.setColor(Color.BLACK)
	          val x1 = x0 + x * wid
	          val y1 = y0 + y * wid
	          g.fill(new Rectangle2D.Double(x1 + 5, y1 + 15, wid - 9, wid - 15))
	          val pieces = running.getScales.map(_.getParts).flatten.filter(_.getLocation == (x, y + 1))(0).getPieces.size
	          g.setColor(Color.WHITE)
	          g.drawString(pieces.toString(), x1 + 13, y1 + 27)	          
	        }
	        case _ => //draw nothing
	      }
    }
  }
      
  //updates locations for all scales
  def drawScales = {
    for(i <- 0 until running.getScales.size) {
      running.updateScales(running.getScales(i).location._1, running.getScales(i).location._2)
      running.updateScales(running.getScales(i).location._1, running.getScales(i).location._2 + 1)
      running.updateScales(running.getScales(i).location._1, running.getScales(i).location._2 + 2)
      for(j <- 0 until running.getScales(i).getParts.size) {
          running.updateScales(running.getScales(i).getParts(j).getLocation._1, running.getScales(i).getParts(j).getLocation._2)
      }
    }
  }
}

  object FirstSwingApp extends SimpleSwingApplication {
    def top = new MainFrame {
      title = "Scale game"
      
      private var badL = (0, 0)
      private val colorMap = Map(Color.RED -> 4, Color.BLUE -> 5, Color.GREEN -> 6, Color.YELLOW -> 7, Color.BLACK -> 8)
      
      //counts real location in game grid
      def count(n: Int) = {
        var n0 = n
        var indexN = 0
        while(n0 >= 33) {
         n0 -= 33
          indexN += 1
        }
        indexN
      }
      
      //when mouse is clicked
      def mouseClick(x: Int, y: Int) = {
        if(canvas.running.getPlayers.isEmpty) console.text_=("Needs at least 1 player to play!")
        else if(canvas.running.piecesLeft == 0) console.text_=("Out of pieces. Winner(s):  " + canvas.running.getWinner)
        else {
          val col = count(x)
          val row = count(y)
          if(col < 18 && row < 18) {
            canvas.running.emptyBad(badL._1, badL._2)
            if(canvas.running(col, row) != 2 && canvas.running(col, row + 1) == 2 && canvas.running(col, row + 2) == 0) {
              val turnColor = colorMap(canvas.running.getTurn.getColor)
              val lastTurnPieces = canvas.running.getScales.map(_.getParts).flatten.filter(_.getLocation == (col, row + 1))(0).getPieces.size
              canvas.running.update(col, row)
              val thisTurnPieces = canvas.running.getScales.map(_.getParts).flatten.filter(_.getLocation == (col, row + 1))(0).getPieces.size
              if(thisTurnPieces > lastTurnPieces) console.text_=("Successfully placed piece at " + col + ", " + row)
              else console.text_=("Scale lost its balance due to piece at " + col + ", " + row)
              pieces.text_=("Pieces left: " + canvas.running.piecesLeft)
              points.text_=("Points: " + canvas.running.pointsString)
              turnNow.text_=("Turn: " + canvas.running.getTurn.getName)
            }
            else if(canvas.running(col, row) != 2) {
              badL = (col, row)
              console.text_=("Bad location")
              canvas.running.badLocation(col, row)
            }
            canvas.drawScales
            canvas.repaint()
          }
        }
      }
      
      //creating GUI elements
      val canvas = new Canvas(new Game) { preferredSize = new Dimension(593, 600) }
      canvas.running.createPieces(20)
      canvas.running.createScale(5)
      val addPlayer = new Button("Add player")
      val playerName = new TextField {columns = 1 }
      val console = new Label {text = " "}
      console.horizontalAlignment_=(Alignment.Left)
      val turnNow = new Label {text = "Turn: "}
      turnNow.horizontalAlignment_=(Alignment.Left)
      val players = new Label {text = "Players: " + canvas.running.getPlayersString}
      players.horizontalAlignment_=(Alignment.Left)
      val pieces = new Label {text = "Pieces left: " + canvas.running.piecesLeft}
      pieces.horizontalAlignment_=(Alignment.Left)
      val points = new Label {text = "Points: " + canvas.running.pointsString}
      points.horizontalAlignment_=(Alignment.Left)
      val mode = new CheckBox {text = "Hard mode"}
      mode.horizontalAlignment_=(Alignment.Left)
      val rate = new Label {text = "Scale spawn rate (can't be changed after creating a player)"}
      rate.horizontalAlignment_=(Alignment.Left)
      val randomRate = new Slider
      randomRate.majorTickSpacing_=(10)
      randomRate.min_=(0)
      randomRate.max_=(100)
      randomRate.snapToTicks_=(true)
      randomRate.paintTicks_=(true)
      randomRate.paintLabels_=(true)
      randomRate.labels_=(Map( 0 -> new Label("0%"),  10  -> new Label("10%"), 20 -> new Label("20%"),
                              30 -> new Label("30%"), 40  -> new Label("40%"), 50 -> new Label("50%"),
                              60 -> new Label("60%"), 70  -> new Label("70%"), 80 -> new Label("80%"),
                              90 -> new Label("90%"), 100 -> new Label("100%")                      ))
                              
      var colors = Vector("Red", "Blue", "Green", "Yellow", "Black")
      val colorsMap = Map("Red" -> Color.RED, "Blue" -> Color.BLUE, "Green" -> Color.GREEN, "Yellow" -> Color.YELLOW, "Black" -> Color.BLACK)
      var colorBox = new ComboBox(colors)
      colorBox.peer.setModel(new DefaultComboBoxModel)
      colorBox.peer.addItem("Red")
      colorBox.peer.addItem("Blue")
      colorBox.peer.addItem("Green")
      colorBox.peer.addItem("Yellow")
      colorBox.peer.addItem("Black")
      
      
      contents = new BoxPanel(Orientation.Vertical) {
          listenTo(mouse.clicks)
          reactions += {
            case MouseClicked(_, p, _, _, _) => mouseClick(p.x, p.y)
          }
        
        contents += canvas
        contents += new BorderPanel {
          add(console, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(rate, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(randomRate, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(turnNow, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(players, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(points, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(pieces, BorderPanel.Position.North)
        }
        contents += new BorderPanel {
          add(mode, BorderPanel.Position.West)
          add(colorBox, BorderPanel.Position.East)
        }
        contents += new BorderPanel {
          add(playerName, BorderPanel.Position.Center)
          add(addPlayer, BorderPanel.Position.East)
        }
      }
      
      //when addPlayer button is pressed
      listenTo(addPlayer)
        reactions += {
          case ButtonClicked(b) =>
            if(canvas.running.getPlayers.find(_.getName == playerName.text) == None && playerName.text != "" && colorBox.peer.getItemCount > 0) {
              canvas.running.createPlayer(playerName.text, colorsMap(colorBox.item))
              colorBox.peer.removeItem(colorBox.item)
              mode.enabled_=(false)
              if(mode.selected) canvas.running.setHardmode
              playerName.text_=("")
              console.text_=("Successfully added player")
              canvas.running.setSpawnRate(randomRate.value)
              randomRate.enabled_=(false)
              players.text_=("Players: " + canvas.running.getPlayersString)
              turnNow.text_=("Turn: " + canvas.running.getTurn.getName)
              canvas.drawScales
              canvas.repaint()
            }
            else console.text_=("Invalid player name or too many players already")
        }
    }
  }