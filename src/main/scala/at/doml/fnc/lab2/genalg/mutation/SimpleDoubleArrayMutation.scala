package at.doml.fnc.lab2.genalg.mutation

import at.doml.fnc.lab2.genalg.MutationOperator
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray
import scala.util.Random

class SimpleDoubleArrayMutation(private val chance: Double,
                                private val mean: Double,
                                private val deviation: Double) extends MutationOperator[DoubleArray] {

    override def mutate(chromosome: DoubleArray): DoubleArray = {
        val array = chromosome.underlying.clone()

        for (i <- array.indices) {
            if (Random.nextDouble() <= chance) {
                array(i) = array(i) + mean + Random.nextGaussian() * deviation
            }
        }

        new DoubleArray(array)
    }
}
