package at.doml.fnc.lab2.genalg.selection

import at.doml.fnc.lab2.genalg.{Chromosome, SelectionOperator}
import scala.util.Random

object RouletteWheelSelection extends SelectionOperator {

    override def select[T](population: List[Chromosome[T]]): (List[Chromosome[T]], List[Chromosome[T]]) = {
        val max = population.last.fitness
        val fitnessValues = population.map(max - _.fitness)
        val sum = fitnessValues.sum

        if (sum.abs <= 10e-7) {
            val shuffled = Random.shuffle(population)
            return (shuffled.take(2), shuffled.drop(2))
        }

        val chances = fitnessValues.map(_ / sum)
        val firstIndex = selectIndex(chances)
        val secondIndex = selectIndex(chances)

        (
            List(population(firstIndex), population(secondIndex)),
            population.zipWithIndex.filter(t => t._2 != firstIndex && t._2 != secondIndex).unzip._1
        )
    }

    private def selectIndex(chances: List[Double]): Int = {
        val random = Random.nextDouble()
        var sum = 0.0
        var index = -1

        while (sum < random && index < chances.size) {
            index += 1
            sum += chances(index)
        }

        index
    }
}
