package at.doml.fnc.lab5.crossover

import at.doml.fnc.lab2.genalg.CrossoverOperator
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray
import scala.util.Random

object UniformDoubleArrayCrossover extends CrossoverOperator[DoubleArray] {

    override def doCrossover(parents: (DoubleArray, DoubleArray)): DoubleArray = {
        val first = parents._1.underlying
        val second = parents._2.underlying
        val childArray = (first, second).zipped.map(
            (f, s) => if (Random.nextBoolean()) {
                f
            } else {
                s
            }
        )

        new DoubleArray(childArray)
    }
}
