package at.doml.fnc.lab5

import scala.collection.mutable.ListBuffer
import scala.io.Source

class Dataset(file: Source) {

    val (x: Array[Double], y: Array[Double], c1: Array[Double], c2: Array[Double], c3: Array[Double], size: Int) = {
        val splitLines = file.getLines()
            .map(_.trim)
            .filter(!_.isEmpty)
            .map(_.split("\\s+"))
            .map(_.map(_.toDouble))

        def DoubleList() = ListBuffer[Double]()

        val out = (DoubleList(), DoubleList(), DoubleList(), DoubleList(), DoubleList())
        var size = 0

        for (splitLine <- splitLines) {
            out._1 += splitLine(0)
            out._2 += splitLine(1)
            out._3 += splitLine(2)
            out._4 += splitLine(3)
            out._5 += splitLine(4)
        }

        (out._1.toArray, out._2.toArray, out._3.toArray, out._4.toArray, out._5.toArray, out._1.length)
    }
}
