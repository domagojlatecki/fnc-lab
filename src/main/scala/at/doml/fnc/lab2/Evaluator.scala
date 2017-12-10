package at.doml.fnc.lab2

import at.doml.fnc.lab2.genalg.FitnessEvaluator
import scala.io.Source
import scala.math.{cos, exp, sin}

class Evaluator[T](source: Source, converter: T => Array[Double]) extends FitnessEvaluator[T] {

    type IN_X = Double
    type IN_Y = Double
    type OUT = Double

    private val measures: List[(IN_X, IN_Y, OUT)] =
        (for {
            line <- source.getLines()
            if !line.trim.isEmpty
        } yield {
            val split = line.trim.split("\\s+", 3).map(_.toDouble)

            (split(0), split(1), split(2))
        }).toList

    private def sq(x: Double) = x * x

    private def function(beta0: Double, beta1: Double, beta2: Double, beta3: Double, beta4: Double): Double = {
        (for ((x, y, actual) <- measures) yield {
            val predicted = sin(beta0 + beta1 * x) + beta2 * cos(x * (beta3 + y)) *
                (1.0 / (1.0 + exp(sq(x - beta4))))
            sq(predicted - actual)
        }).sum / measures.size
    }

    override def evaluate(chromosome: T): Double = {
        val array = converter(chromosome)

        function(array(0), array(1), array(2), array(3), array(4))
    }
}
