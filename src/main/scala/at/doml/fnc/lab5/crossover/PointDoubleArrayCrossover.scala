package at.doml.fnc.lab5.crossover

import at.doml.fnc.lab2.genalg.CrossoverOperator
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray
import scala.util.Random

object PointDoubleArrayCrossover extends CrossoverOperator[DoubleArray] {

    override def doCrossover(parents: (DoubleArray, DoubleArray)): DoubleArray = {
        val size = parents._1.underlying.length
        val crossoverPoint = Random.nextInt(size + 1)
        val childArray = parents._1.underlying.view(0, crossoverPoint) ++
            parents._2.underlying.view(crossoverPoint, size)

        new DoubleArray(childArray.toArray)
    }
}
