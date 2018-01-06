package at.doml.fnc.lab3

import at.doml.fnc.lab3.neuralnetwork.{Layer, NeuralNetwork, SigmoidTransferFunction}
import scala.io.Source

object Preprocessor {

    type NumRepresentativePoints = Int
    type MaxIterations = Int
    type TargetError = Double
    type NeuralNetworkLayout = String
    type BatchSize = Int
    type Step = Double
    type Params = (NumRepresentativePoints, MaxIterations, TargetError, NeuralNetworkLayout, BatchSize, Step)
    type Sample = (Array[Double], Array[Double])

    var neuralNetwork: NeuralNetwork = _
    var numRepresentativePoints: Int = 0

    def apply(args: Array[String]): Unit = {
        var file = ""
        if (args.length == 0) {
            printUsage()
        } else if (args.length == 1) {
            file = args(0)
        } else {
            file = args.last
        }

        val params = extractParams(args.dropRight(1))

        this.numRepresentativePoints = params._1

        val dataset = Source.fromFile(file).getLines()
            .toList
            .map(_.split(':'))
            .map(line => (listFromTuples(line(1), params._1), arrayFromNumbers(line(0))))
        val groupedDataset = groupDataset(dataset, params._5)
        val layers = createLayers(params._4, params._1)
        val neuralNetwork = new NeuralNetwork(layers)
        this.neuralNetwork = neuralNetwork

        println("Learning...")
        Backpropagation(neuralNetwork, params._6, groupedDataset, params._2, params._3)
        println("Learning complete")
    }

    private def arrayFromNumbers(string: String): Array[Double] = {
        string.replace("[", "").replace("]", "")
            .split(",")
            .map(_.trim)
            .map(_.toDouble)
    }

    private def listFromTuples(string: String, numRepresentative: Int): Array[Double] = {
        val rawPoints = string.replace("[", "").replace("]", "")
            .split(",\\(")
            .map(_.trim)
            .map(tupleString => {
                val array = tupleString.replace("(", "").replace(")", "")
                    .split(",")
                    .map(_.trim)
                    .map(_.toDouble)
                (array(0), array(1))
            }).toList
        val transformedPoints = PointListTransformer.transform(rawPoints, numRepresentative)

        transformedPoints.flatMap(t => Array(t._1, t._2)).toArray
    }

    private def groupDataset(dataset: List[Sample], batchSize: Int): List[List[Sample]] = {
        dataset.grouped(batchSize).toList
    }

    private def createLayers(layout: String, numRepresentative: Int): List[Layer] = {
        val sizes = try {
            layout.split("x").map(_.toInt)
        } catch {
            case e: NumberFormatException => {
                println("Invalid neural netork layout: $layout")
                exit(1)
            }
        }

        var numInputs = 2 * numRepresentative
        if (sizes.head != numInputs || sizes.last != 5) {
            println(s"Invalid neural network layout: $layout, $numInputs inputs and 5 outputs expected.")
            exit(1)
        }

        (for (size <- sizes) yield {
            val layer = new Layer(SigmoidTransferFunction, numInputs, size)

            numInputs = size
            layer
        }).toList
    }

    private def exit(status: Int): Nothing = {
        System.exit(status)
        throw new IllegalStateException("should have exited")
    }

    private def printUsage(): Unit = {
        println("Usage: [params...] file")
        println("Params:")
        println("    -M <number>                     Number of representative points, default: 5")
        println("    -i <number>                     Maximum number of iterations, default: 10000")
        println("    -e <decimal>                    Target error, default: 10e-7")
        println("    --nn <neural-network-layout>    Layout of the neural network, default: 10x20x5")
        println("    --batch-size <number>           Batch size for backpropagation algorithm, default: 1")
        println("    --step <decimal>                Learning step of backpropagation algorithm, default: 1.0")
        exit(0)
    }

    private def extractParams(args: Array[String]): Params = {
        type Setter = String => Unit

        var numRepresentativePoints = 5
        val numRepresentativePointsSetter: Setter = a => numRepresentativePoints = a.toInt

        var maxIterations = 10000
        val maxIterationsSetter: Setter = a => maxIterations = a.toInt

        var targetError = 10e-7
        val targetErrorSetter: Setter = a => targetError = a.toDouble

        var neuralNetworkLayout = "10x20x5"
        val neuralNetworkLayoutSetter: Setter = a => neuralNetworkLayout = a

        var batchSize = 1
        val batchSizeSetter: Setter = a => batchSize = a.toInt

        var step = 1.00
        val stepSetter: Setter = a => step = a.toDouble

        val noSetter: String => Unit = _ => {}
        var setter: String => Unit = noSetter

        for (arg <- args) {
            if (setter eq noSetter) {
                arg match {
                    case "-M" => setter = numRepresentativePointsSetter
                    case "-i" => setter = maxIterationsSetter
                    case "-e" => setter = targetErrorSetter
                    case "--nn" => setter = neuralNetworkLayoutSetter
                    case "--batch-size" => setter = batchSizeSetter
                    case "--step" => setter = stepSetter
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

        (numRepresentativePoints, maxIterations, targetError, neuralNetworkLayout, batchSize, step)
    }
}
