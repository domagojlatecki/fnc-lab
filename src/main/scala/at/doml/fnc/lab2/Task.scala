package at.doml.fnc.lab2

import at.doml.fnc.lab2.genalg.GeneticAlgorithm.{Elimination, Generational, SelectionMode}
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray
import at.doml.fnc.lab2.genalg.crossover.ArithmeticDoubleArrayCrossover
import at.doml.fnc.lab2.genalg.mutation.SimpleDoubleArrayMutation
import at.doml.fnc.lab2.genalg.selection.{RouletteWheelSelection, ThreeTournamentSelection}
import at.doml.fnc.lab2.genalg.{GeneticAlgorithm, SelectionOperator}
import scala.io.Source
import scala.util.Random

object Task {

    type PopulationSize = Int
    type MaxIterations = Int
    type FitnessThreshold = Double
    type MutationChance = Double
    type Mean = Double
    type Deviation = Double
    type Elitism = Boolean
    type Verbose = Boolean
    type Params = (PopulationSize, MaxIterations, FitnessThreshold, MutationChance, Mean, Deviation, Elitism, Verbose,
        SelectionMode, SelectionOperator)

    def main(args: Array[String]): Unit = {
        var file: String = ""
        var params: Params = (100, 10000, 0.0, 0.05, 0.0, 1.0, false, false, Generational, RouletteWheelSelection)

        if (args.length == 0) {
            printUsage()
        } else if (args.length == 1) {
            file = args(0)
        } else {
            file = args.last
            params = extractParams(args.dropRight(1))
        }

        val evaluator = new Evaluator[DoubleArray](
            Source.fromFile(file),
            _.underlying
        )
        val algorithm = new GeneticAlgorithm[DoubleArray](
            populationSize = params._1,
            maxIterations = params._2,
            fitnessThreshold = params._3,
            elitism = params._7,
            verbose = params._8,
            selectionOperator = params._10,
            crossoverOperator = ArithmeticDoubleArrayCrossover,
            mutationOperator = new SimpleDoubleArrayMutation(
                chance = params._4,
                mean = params._5,
                deviation = params._6
            ),
            selectionMode = params._9
        )
        val best = algorithm(
            () => new DoubleArray(randomArray()),
            evaluator
        )

        println(s"Solution: ${best.underlying}, fitness: ${best.fitness}")
    }

    private def printUsage(): Unit = {
        println("Usage: [params...] file")
        println("Params:")
        println("    -s <number>       Population size, default: 100")
        println("    -i <number>       Max number of iterations, default: 10000")
        println("    -t <decimal>      Fitness threshold, default: 0.0")
        println("    -c <decimal>      Mutation chance, default: 0.05")
        println("    -m <decimal>      Gaussian mean for mutation, default: 0.0")
        println("    -d <decimal>      Gaussian deviation for mutation, default: 1.0")
        println("    --verbose         Print best chromosome in every iteration of the algorithm")
        println("    --elitism         Use elitism, only valid for generational algorithm")
        println("    --generational    Use generational algorithm, on by default")
        println("    --elimination     Use elimination algorithm")
        System.exit(0)
    }

    private def extractParams(args: Array[String]): Params = {
        type Setter = String => Unit

        var populationSize = 100
        val populationSizeSetter: Setter = a => populationSize = a.toInt

        var maxIterations = 10000
        val maxIterationsSetter: Setter = a => maxIterations = a.toInt

        var fitnessThreshold = 0.0
        val fitnessThresholdSetter: Setter = a => fitnessThreshold = a.toDouble

        var mutationChance = 0.05
        val mutationChanceSetter: Setter = a => mutationChance = a.toDouble

        var mean = 0.0
        val meanSetter: Setter = a => mean = a.toDouble

        var deviation = 1.0
        val deviationSetter: Setter = a => deviation = a.toDouble

        var elitism = false
        var verbose = false
        var selectionMode: SelectionMode = Generational
        var selectionOperator: SelectionOperator = RouletteWheelSelection

        val noSetter: String => Unit = _ => {}
        var setter: String => Unit = noSetter

        for (arg <- args) {
            if (setter eq noSetter) {
                arg match {
                    case "-s" => setter = populationSizeSetter
                    case "-i" => setter = maxIterationsSetter
                    case "-t" => setter = fitnessThresholdSetter
                    case "-c" => setter = mutationChanceSetter
                    case "-m" => setter = meanSetter
                    case "-d" => setter = deviationSetter
                    case "--verbose" => verbose = true
                    case "--elitism" => elitism = true
                    case "--generational" => {
                        selectionMode = Generational
                        selectionOperator = RouletteWheelSelection
                    }
                    case "--elimination" => {
                        selectionMode = Elimination
                        selectionOperator = ThreeTournamentSelection
                    }
                    case _ => {
                        println(s"Unknown argument: $arg")
                        System.exit(1)
                    }
                }
            } else {
                setter(arg)
                setter = noSetter
            }
        }

        (populationSize, maxIterations, fitnessThreshold, mutationChance, mean, deviation, elitism, verbose,
            selectionMode, selectionOperator)
    }

    private def randomArray(): Array[Double] = {
        val array = new Array[Double](5)

        for (i <- array.indices) {
            array(i) = Random.nextDouble()
        }

        array
    }
}
