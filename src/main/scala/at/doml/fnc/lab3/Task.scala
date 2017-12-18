package at.doml.fnc.lab3

import java.awt.Dimension
import at.doml.fnc.lab3.gui.DrawingPanel
import scala.swing.BorderPanel.Position._
import scala.swing.{BorderPanel, Frame, MainFrame, SimpleSwingApplication}

object Task extends SimpleSwingApplication {

    override def startup(args: Array[String]): Unit = {
        Preprocessor(args)
        super.startup(args)
    }

    override def top: Frame = new MainFrame {

        private val drawingPanel = new DrawingPanel()

        title = "Simple Character Recognizer"
        contents = new BorderPanel() {
            layout(drawingPanel) = Center
        }
        size = new Dimension(640, 480)

        this.drawingPanel.requestFocusInWindow()
    }
}
