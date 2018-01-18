package at.doml.fnc.lab5

import at.doml.fnc.lab2.genalg.FitnessEvaluator
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray

class Evaluator(neuralNetwork: NeuralNetwork, dataset: Dataset) extends FitnessEvaluator[DoubleArray] {

    override def evaluate(chromosome: DoubleArray): Double = {
        val weights = chromosome.underlying
        (for (i <- 0 until dataset.size) yield {
            val out = neuralNetwork(dataset.x(i), dataset.y(i), weights)
            sq(dataset.c1(i) - out._1) + sq(dataset.c2(i) - out._2) + sq(dataset.c3(i) - out._3)
        }).sum / dataset.size
    }

    private def sq(x: Double) = x * x
}
