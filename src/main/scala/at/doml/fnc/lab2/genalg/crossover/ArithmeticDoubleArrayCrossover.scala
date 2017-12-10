package at.doml.fnc.lab2.genalg.crossover

import at.doml.fnc.lab2.genalg.CrossoverOperator
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray

object ArithmeticDoubleArrayCrossover extends CrossoverOperator[DoubleArray] {

    override def doCrossover(parents: (DoubleArray, DoubleArray)): DoubleArray = {
        val array1 = parents._1.underlying
        val array2 = parents._1.underlying
        val childArray = new Array[Double](array1.length)

        for (i <- array1.indices) {
            childArray(i) = (array1(i) + array2(i)) / 2.0
        }

        new DoubleArray(childArray)
    }
}
