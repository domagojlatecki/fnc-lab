package at.doml.fnc.lab2.genalg

import at.doml.fnc.lab2.genalg.GeneticAlgorithm.SelectionMode

class GeneticAlgorithm[T](private val populationSize: Int = 100,
                          private val maxIterations: Int = 10000,
                          private val fitnessThreshold: Double = 0.0,
                          private val elitism: Boolean = false,
                          private val verbose: Boolean = false,
                          private val selectionOperator: SelectionOperator,
                          private val crossoverOperator: CrossoverOperator[T],
                          private val mutationOperator: MutationOperator[T],
                          private val selectionMode: SelectionMode) {

    implicit val ordering: Ordering[Chromosome[T]] = Ordering.by(_.fitness)

    def apply(chromosomeGenerator: () => T, evaluator: FitnessEvaluator[T]): Chromosome[T] = {
        var iteration = 0
        var population: List[Chromosome[T]] = (0 until populationSize)
            .map(_ => chromosomeGenerator())
            .map(c => new Chromosome[T](evaluator.evaluate(c), c))
            .sorted
            .toList
        implicit var best: Chromosome[T] = population.head

        printIfVerbose("Initial")

        while (iteration < maxIterations && best.fitness > fitnessThreshold) {
            iteration += 1
            population = selectionMode.apply(
                population,
                selectionOperator,
                crossoverOperator,
                mutationOperator,
                evaluator,
                elitism
            ).sorted
            best = population.head

            printIfVerbose(s"Iteration $iteration")
        }

        best
    }

    private def printIfVerbose(prefix: String)(implicit best: Chromosome[T]): Unit = {
        if (this.verbose) {
            println(s"$prefix best: ${best.underlying}, error: ${best.fitness}")
        }
    }
}

object GeneticAlgorithm {

    sealed trait SelectionMode {

        def apply[T](population: List[Chromosome[T]],
                     selectionOperator: SelectionOperator,
                     crossoverOperator: CrossoverOperator[T],
                     mutationOperator: MutationOperator[T],
                     evaluator: FitnessEvaluator[T],
                     keepBest: Boolean): List[Chromosome[T]]
    }

    object Generational extends SelectionMode {

        override def apply[T](population: List[Chromosome[T]],
                              selectionOperator: SelectionOperator,
                              crossoverOperator: CrossoverOperator[T],
                              mutationOperator: MutationOperator[T],
                              evaluator: FitnessEvaluator[T],
                              keepBest: Boolean): List[Chromosome[T]] = {
            implicit val ordering: Ordering[Chromosome[T]] = Ordering.by(_.fitness)
            val newPopulation = for (_ <- population.indices) yield {
                val (selected, _) = selectionOperator.select(population)
                val child = crossoverOperator.doCrossover(selected.head.underlying, selected.last.underlying)

                mutationOperator.mutate(child)
            }

            (if (keepBest) {
                newPopulation.dropRight(1) ++ List(population.head.underlying)
            } else {
                newPopulation
            }).map(c => new Chromosome[T](evaluator.evaluate(c), c))
                .sorted
                .toList
        }
    }

    object Elimination extends SelectionMode {

        override def apply[T](population: List[Chromosome[T]],
                              selectionOperator: SelectionOperator,
                              crossoverOperator: CrossoverOperator[T],
                              mutationOperator: MutationOperator[T],
                              evaluator: FitnessEvaluator[T],
                              keepBest: Boolean): List[Chromosome[T]] = {
            implicit val ordering: Ordering[Chromosome[T]] = Ordering.by(_.fitness)
            val (selected, rest) = selectionOperator.select(population)
            val sortedSelected = selected.sorted
            val parents = sortedSelected.dropRight(1)
            val child = crossoverOperator.doCrossover((parents.head.underlying, parents.last.underlying))
            val mutatedChild = mutationOperator.mutate(child)
            val evaluatedChild = new Chromosome[T](evaluator.evaluate(mutatedChild), mutatedChild)

            (if (evaluatedChild <= sortedSelected.last) {
                rest ++ parents ++ List(evaluatedChild)
            } else {
                rest ++ sortedSelected
            }).sorted
        }
    }
}
