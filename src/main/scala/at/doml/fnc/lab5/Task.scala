package at.doml.fnc.lab5

import at.doml.fnc.lab2.genalg.GeneticAlgorithm.{Elimination, Generational, SelectionMode}
import at.doml.fnc.lab2.genalg.chromosome.DoubleArray
import at.doml.fnc.lab2.genalg.crossover.ArithmeticDoubleArrayCrossover
import at.doml.fnc.lab2.genalg.mutation.SimpleDoubleArrayMutation
import at.doml.fnc.lab2.genalg.selection.{RouletteWheelSelection, ThreeTournamentSelection}
import at.doml.fnc.lab2.genalg.{CrossoverOperator, GeneticAlgorithm, SelectionOperator}
import at.doml.fnc.lab5.crossover.{PointDoubleArrayCrossover, UniformDoubleArrayCrossover}
import at.doml.fnc.lab5.mutation.CompoundMutation
import scala.io.Source
import scala.util.{Random, Try}

object Task {

    private type PopulationSize = Int
    private type NumIterations = Int
    private type FitnessThreshold = Double
    private type MutationChance = Double
    private type NumMutationOperators = Int
    private type RelativeMutationChances = Array[Double]
    private type Means = Array[Double]
    private type Deviations = Array[Double]
    private type Verbose = Boolean
    private type Elitism = Boolean
    private type NeuralNetworkLayout = String

    private type Params = (PopulationSize, NumIterations, FitnessThreshold, MutationChance, NumMutationOperators,
        RelativeMutationChances, Means, Deviations, Verbose, Elitism, SelectionMode, SelectionOperator,
        CrossoverOperator[DoubleArray], NeuralNetworkLayout, Source)

    def main(args: Array[String]): Unit = {
        val params: Params = if (args.length == 0) {
            printUsage()
        } else {
            getParams(args)
        }

        val dataset = Try(new Dataset(params._15)).getOrElse {
            println("Unable to load provided dataset")
            exit(1)
        }
        val networkLayout = params._14.split('x').drop(1).map(_.toInt)
        val network = new NeuralNetwork(networkLayout(0), networkLayout.drop(1))
        val evaluator = new Evaluator(network, dataset)
        val algorithm = new GeneticAlgorithm[DoubleArray](
            populationSize = params._1,
            maxIterations = params._2,
            fitnessThreshold = params._3,
            elitism = params._10,
            verbose = params._9,
            selectionOperator = params._12,
            crossoverOperator = params._13,
            mutationOperator = createMutationOperator(
                params._5, params._4, params._6, params._7, params._8
            ),
            selectionMode = params._11
        )
        val best = algorithm(
            () => new DoubleArray(randomArray(network.wSize, networkLayout(0))),
            evaluator
        )

        println()
        println(s"Learning finished, solution: ${best.underlying.underlying.mkString("[", ", ", "]")}," +
            s" fitness: ${best.fitness}")
        println()
        println("Testing netowrk against each sample...")
        println()

        var numCorrect = 0

        for (i <- 0 until dataset.size) {
            val x = dataset.x(i)
            val y = dataset.y(i)
            val c1 = dataset.c1(i).toInt
            val c2 = dataset.c2(i).toInt
            val c3 = dataset.c3(i).toInt
            val out = network(x, y, best.underlying.underlying)
            val n1 = getOut(out._1)
            val n2 = getOut(out._2)
            val n3 = getOut(out._3)
            val cStr = if (testClassification(c1, c2, c3, n1, n2, n3)) {
                numCorrect += 1
                "OK"
            } else {
                "INVALID"
            }

            println(s"Sample: x = $x, y = $y, class = [$c1, $c2, $c3]; network out = [$n1, $n2, $n3] -> $cStr")
        }

        val percentage = ((numCorrect / dataset.size.toDouble) * 100.0).formatted("%.2f")

        println()
        println(s"Number of correct classifications: $numCorrect / ${dataset.size} ($percentage %)")
    }

    private def getOut(v: Double): Int = if (v < 0.5) {
        0
    } else {
        1
    }

    private def testClassification(c1: Int, c2: Int, c3: Int, n1: Int, n2: Int, n3: Int): Boolean = c1 == n1 &&
        c2 == n2 && c3 == n3

    private def createMutationOperator(numMutationOperators: Int,
                                       chance: Double,
                                       chances: Array[Double],
                                       means: Array[Double],
                                       deviations: Array[Double]): CompoundMutation[DoubleArray] = {
        val operators = (means, deviations).zipped
            .map((mean, deviation) => new SimpleDoubleArrayMutation(
                chance = chance,
                mean = mean,
                deviation = deviation
            ))
        new CompoundMutation[DoubleArray](operators.toList, chances.toList)
    }

    private def printUsage(): Nothing = {
        println(
            """|Usage: [params...] file
               |Params:"
               |    -s <number>                     Population size, default: 100
               |    -i <number>                     Max number of iterations, default: 10000
               |    -t <decimal>                    Fitness threshold, default: 10e-7
               |    -c <decimal>                    Mutation chance, default: 0.05
               |    --mn <number>                   Number of mutation operators, default: 2
               |    --c<number> <decimal>           Relative chance for nth mutation operator (0-based), default: 1.0
               |    --m<number> <decimal>           Gaussian mean for nth mutation operator (0-based), default: 0.0
               |    --d<number> <decimal>           Gaussian deviation nth for mutation operator (0-based), default: 1.0
               |    --verbose                       Print best chromosome in every iteration of the algorithm
               |    --elitism                       Use elitism, only valid for generational algorithm
               |    --generational                  Use generational algorithm, on by default
               |    --elimination                   Use elimination algorithm
               |    --crop <operator>               Specifies crossover operator, available operators:
               |                                    [arithmetic, point, uniform], default: arithmetic
               |    --nn <neural-network-layout>    Layout of the neural network, default: 2x8x3""".stripMargin
        )
        exit(0)
    }

    private def exit(statusCode: Int): Nothing = {
        System.exit(statusCode)
        throw new IllegalStateException("process should have exited!")
    }

    private def getParams(args: Array[String]): Params = {
        type Setter = String => Unit

        val crossoverOperatorMap = Map(
            "arithmetic" -> ArithmeticDoubleArrayCrossover,
            "point" -> PointDoubleArrayCrossover,
            "uniform" -> UniformDoubleArrayCrossover
        )

        val chance = "--c([0-9]+)".r
        val mean = "--m([0-9]+)".r
        val deviation = "--d([0-9]+)".r

        def require[T](message: String, value: String, converter: String => T, predicate: T => Boolean): T = {
            val result = Try(converter(value)).filter(predicate)

            if (result.isFailure) {
                println(message)
                exit(1)
            }

            result.get
        }

        def requirePositiveInt(name: String, value: String): Int = require(
            s"$name must be a positive integer",
            value, _.toInt, (v: Int) => v > 0
        )

        def requireNonNegativeDouble(name: String, value: String): Double = require(
            s"$name must be a non-negative number",
            value, _.toDouble, (v: Double) => v >= 0.0
        )

        def requireDouble(name: String, value: String): Double = require(
            s"$name must be a number", value, _.toDouble, (_: Double) => true
        )

        def requireInRange(name: String, value: String, from: Double, to: Double) = require(
            s"$name must be a number from range [$from, $to]",
            value, _.toDouble, (v: Double) => v >= from && v <= to
        )

        var populationSize = 100
        val populationSizeSetter: Setter = s => {
            populationSize = requirePositiveInt("Population size", s)
        }

        var numIterations = 10000
        val numIterationsSetter: Setter = s => {
            numIterations = requirePositiveInt("Number of iterations", s)
        }

        var fitnessThreshold = 10e-7
        val fitnessThresholdSetter: Setter = s => {
            fitnessThreshold = requireNonNegativeDouble("Fitness threshold", s)
        }

        var mutationChance = 0.05
        val mutationChanceSetter: Setter = s => {
            mutationChance = requireInRange("Mutation chance", s, 0.0, 1.0)
        }

        class Ref[T](var ref: T)

        var numMutationOperators = 2
        val relativeMutationChances = new Ref(Array(1.0, 1.0))
        val means = new Ref(Array(0.0, 0.0))
        val deviations = new Ref(Array(1.0, 1.0))
        val numMutationOperatorsSetter: Setter = s => {
            numMutationOperators = requirePositiveInt("Number of mutation operators", s)
            relativeMutationChances.ref = Array.fill(numMutationOperators)(1.0)
            means.ref = Array.fill(numMutationOperators)(0.0)
            deviations.ref = Array.fill(numMutationOperators)(1.0)
        }

        def arraySetter(name: String, arrayRef: Ref[Array[Double]],
                        valueSetter: (Array[Double], Int, Double) => Unit,
                        requireFn: (String, String) => Double): Setter = s => {
            val split = s.split(' ')
            val index = Try(split(0).toInt).filter(_ < numMutationOperators)
                .getOrElse {
                    println(s"Mutation operator index is out of range: ${split(0)}")
                    exit(1)
                }
            valueSetter(
                arrayRef.ref,
                index,
                requireFn(
                    s"$name #$index", split(1)
                )
            )
        }

        val relativeMutationChancesSetter: Setter = arraySetter(
            "Relative mutation chance", relativeMutationChances, _ (_) = _, requireNonNegativeDouble
        )
        val meansSetter: Setter = arraySetter(
            "Mean", means, _ (_) = _, requireDouble
        )
        val deviationsSetter: Setter = arraySetter(
            "Deviation", deviations, _ (_) = _, requireNonNegativeDouble
        )

        var verbose = false
        var elitism = false
        var selectionMode: SelectionMode = Generational
        var selectionOperator: SelectionOperator = RouletteWheelSelection

        var crossoverOperator: CrossoverOperator[DoubleArray] = ArithmeticDoubleArrayCrossover
        val crossoverOperatorSetter: Setter = s => {
            crossoverOperator = require(
                "Crossover operator must be one of the following: [arithmetic, point, uniform]",
                s, crossoverOperatorMap(_), (v: CrossoverOperator[DoubleArray]) => v != null
            )
        }

        var neuralNetworkLayout = "2x8x3"
        val neuralNetworkLayoutSetter: Setter = s => {
            neuralNetworkLayout = require(
                "Neural network layout must match the following regular expression: 2x[0-9]+(x[9-0]+)*x3",
                s, identity, (v: String) => v.matches("^2x[0-9]+(x[0-9]+)*x3$")
            )
        }

        val noSetter: Setter = _ => {}
        var setter: Setter = noSetter

        var prefix = ""
        for (arg <- args.dropRight(1)) {

            if (setter eq noSetter) {
                arg match {
                    case "-s" => setter = populationSizeSetter
                    case "-i" => setter = numIterationsSetter
                    case "-t" => setter = fitnessThresholdSetter
                    case "-c" => setter = mutationChanceSetter
                    case "--mn" => setter = numMutationOperatorsSetter
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
                    case "--crop" => setter = crossoverOperatorSetter
                    case "--nn" => setter = neuralNetworkLayoutSetter
                    case chance(index) => {
                        prefix = s"$index "
                        setter = relativeMutationChancesSetter
                    }
                    case mean(index) => {
                        prefix = s"$index "
                        setter = meansSetter
                    }
                    case deviation(index) => {
                        prefix = s"$index "
                        setter = deviationsSetter
                    }
                    case _ => {
                        println(s"Unknown argument: $arg")
                        exit(1)
                    }
                }
            } else {
                setter(s"$prefix$arg")
                setter = noSetter
                prefix = ""
            }
        }

        val source = try {
            Source.fromFile(args.last)
        } catch {
            case _: Exception => {
                println(s"Unable to read file: ${args.last}")
                exit(1)
            }
        }

        (populationSize, numIterations, fitnessThreshold, mutationChance, numMutationOperators,
            relativeMutationChances.ref, means.ref, deviations.ref, verbose, elitism, selectionMode, selectionOperator,
            crossoverOperator, neuralNetworkLayout, source)
    }

    private def randomArray(size: Int, numN1Neurons: Int): Array[Double] = {
        val array = new Array[Double](size)

        for (i <- array.indices) {
            array(i) = Random.nextDouble() - 0.5
        }

        for (i <- 0 until numN1Neurons) {
            array(2 + i * 4) = Random.nextDouble() + 0.5
            array(3 + i * 4) = Random.nextDouble() + 0.5
        }

        array
    }
}
