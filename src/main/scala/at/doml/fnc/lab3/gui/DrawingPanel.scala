package at.doml.fnc.lab3.gui

import java.awt.{Color, Point}
import at.doml.fnc.lab3.{PointListTransformer, Preprocessor}
import scala.collection.mutable.ListBuffer
import scala.swing.event.{MouseDragged, MousePressed, MouseReleased}
import scala.swing.{Component, Graphics2D}

class DrawingPanel extends Component {

    private var drawEnabled = false
    private val points: ListBuffer[Point] = new ListBuffer()

    def reset(): Unit = {
        this.points.clear()
        this.repaint()
    }

    def getPoints: List[Point] = this.points.toList

    override def paintComponent(g: Graphics2D): Unit = {
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, this.size.width, this.size.height)
        g.setColor(Color.RED)

        if (this.points.size < 2) {
            this.points.foreach(p => g.drawLine(p.x, p.y, p.x, p.y))
        } else {
            for (i <- 0 until this.points.size - 1) {
                val ps = this.points(i)
                val pe = this.points(i + 1)

                g.drawLine(ps.x, ps.y, pe.x, pe.y)
            }
        }
    }

    listenTo(mouse.clicks, mouse.moves)
    reactions += {
        case MouseDragged(_, point, _) => {
            if (this.drawEnabled) {
                this.points += point
                this.repaint()
            }
        }
        case MousePressed(_, point, _, _, _) => {
            this.reset()
            this.drawEnabled = true
            this.points += point
            this.repaint()
        }
        case MouseReleased(_, point, _, _, _) => {
            this.drawEnabled = false
            val normalizedPoints = PointListTransformer.normalize(this.points.toList)
            val transformedPoints = PointListTransformer.transform(
                normalizedPoints, Preprocessor.numRepresentativePoints
            )
            val pointArray = transformedPoints.flatMap(t => Array(t._1, t._2)).toArray
            val out = Preprocessor.neuralNetwork(pointArray)

            println(out.mkString("[", ", ", "]"))
        }
    }
    focusable = true
}
