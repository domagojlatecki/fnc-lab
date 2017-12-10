package at.doml.fnc.lab2.genalg.selection

import at.doml.fnc.lab2.genalg.{Chromosome, SelectionOperator}
import scala.util.Random

object ThreeTournamentSelection extends SelectionOperator {

    override def select[T](population: List[Chromosome[T]]): (List[Chromosome[T]], List[Chromosome[T]]) = {
        val shuffled = Random.shuffle(population)

        (shuffled.take(3), shuffled.drop(3))
    }
}
