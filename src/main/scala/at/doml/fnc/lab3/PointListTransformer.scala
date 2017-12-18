package at.doml.fnc.lab3

import java.awt.Point
import scala.collection.mutable.ListBuffer

object PointListTransformer {

    type DoublePoint = (Double, Double)

    def normalize(points: List[Point]): List[DoublePoint] = {
        val sumPoint = points.reduce(
            (l, r) => new Point(l.x + r.x, l.y + r.y)
        )
        val middlePoint: DoublePoint = (sumPoint.x.toDouble / points.size, sumPoint.y.toDouble / points.size)
        val shiftedPoints: List[DoublePoint] = points.map(
            p => (p.x - middlePoint._1, p.y - middlePoint._2)
        )
        val (xMax, yMax): DoublePoint = shiftedPoints.reduce(
            (l, r) => (l._1.abs max r._1.abs, l._2.abs max r._2.abs)
        )
        val max = xMax max yMax
        val normalizedPoints: List[DoublePoint] = shiftedPoints.map(
            p => (p._1 / max, p._2 / max)
        )

        normalizedPoints
    }

    def transform(points: List[DoublePoint], numRepresentative: Int): List[DoublePoint] = {
        if (points.size < 2) {
            return List()
        }

        var length = 0.0
        for (i <- 0 until points.size - 1) {
            val start = points(i)
            val end = points(i + 1)

            length += distance(start, end)
        }

        val separation = length / numRepresentative
        val representativePoints = new ListBuffer[DoublePoint]()
        var currentDistance = 0.0
        var nextSeparation = separation
        var start = points.head
        var end = points(1)
        var index = 2

        representativePoints += points.head

        while (representativePoints.size < numRepresentative) {
            currentDistance += distance(start, end)

            if (currentDistance >= nextSeparation) {
                currentDistance -= distance(start, end)
                start = interpolate(start, end, (currentDistance - nextSeparation) / (nextSeparation + separation))
                representativePoints += start
                nextSeparation += separation
            } else {
                start = end
                end = points(index)
                index += 1
            }
        }

        representativePoints.toList
    }

    private def sq(x: Double): Double = x * x

    private def distance(start: DoublePoint, end: DoublePoint): Double = {
        Math.sqrt(sq(start._1 - end._1) + sq(start._2 - end._2))
    }

    private def interpolate(start: DoublePoint, end: DoublePoint, t: Double): DoublePoint = {
        (start._1 * (1.0 - t) + t * end._1, start._2 * (1.0 - t) + t * end._2)
    }
}
