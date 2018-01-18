package at.doml.fnc.lab5.mutation

import at.doml.fnc.lab2.genalg.selection.RouletteWheelSelection
import at.doml.fnc.lab2.genalg.{Chromosome, MutationOperator}

class CompoundMutation[T](operators: List[MutationOperator[T]], chances: List[Double]) extends MutationOperator[T] {

    require(operators.length == chances.length)

    object ImpossibleMutation extends MutationOperator[T] {

        override def mutate(nothing: T): T = throw new IllegalStateException("impossible mutation operator called")
    }

    private val operatorsAsChromosomes = (operators ++ List(ImpossibleMutation), chances ++ List(0.0)).zipped
        .map((operator, chance) => new Chromosome(chance, operator))

    override def mutate(chromosome: T): T = RouletteWheelSelection.select(operatorsAsChromosomes)
        ._1.head.underlying.mutate(chromosome)
}
