package at.doml.fnc.lab4

import java.nio.file.{Files, Paths}
import at.doml.fnc.lab4.neuralnetwork.NeuroFuzzySystem
import scala.collection.JavaConverters

object Task {

    type NumOfRules = Int
    type MaxIterations = Int
    type TargetError = Double
    type Step = Double
    type LogErrors = Boolean
    type Params = (NumOfRules, MaxIterations, TargetError, Step, LogErrors)

    def main(args: Array[String]): Unit = {
        var load = false
        val file: String = if (args.length == 0) {
            printUsage()
        } else if (args.length == 1) {
            args(0)
        } else if (args.length == 2 && args(0) == "--load") {
            load = true
            args(1)
        } else {
            args.last
        }

        val data = prepareData()

        if (load) {
            loadAndTestSystem(data, file)
        } else {
            backpropagation(extractParams(args.dropRight(1)), data, file)
        }
    }

    private def loadAndTestSystem(data: List[(Double, Double, Double)], file: String): Unit = {
        println("Loading fuzzy system and testing it against all samples...")

        val lines = JavaConverters.asScalaBuffer(Files.readAllLines(Paths.get(file))).toList
        val numOfRules = lines.head.split(",", 2)(0).toInt
        val system = new NeuroFuzzySystem(numOfRules)

        for (line <- lines.drop(1)) {
            val split = line.split(",")
            val i = split(0).toInt
            val p = split(1).toDouble
            val q = split(2).toDouble
            val r = split(3).toDouble
            val ax = split(4).toDouble
            val bx = split(5).toDouble
            val ay = split(6).toDouble
            val by = split(7).toDouble

            system.p_=(i, p)
            system.q_=(i, q)
            system.r_=(i, r)
            system.ax_=(i, ax)
            system.bx_=(i, bx)
            system.ay_=(i, ay)
            system.by_=(i, by)
        }

        for ((x, y, t) <- data) {
            println(s"$x,$y,${system(x, y) - t}")
        }
    }

    private def backpropagation(params: Params, data: List[(Double, Double, Double)], filePrefix: String): Unit = {
        val system1 = new NeuroFuzzySystem(params._1)

        println("Using stochastic algorithm...")
        Backpropagation(
            system1,
            params._4,
            data,
            params._2,
            params._3,
            stochastic = true,
            logErrors = params._5
        )
        saveSystem(system1, filePrefix + "-sto.txt")

        val system2 = new NeuroFuzzySystem(params._1)

        println("Using gradient descent...")
        Backpropagation(
            system2,
            params._4 / 81.0,
            data,
            params._2,
            params._3,
            stochastic = false,
            logErrors = params._5
        )
        saveSystem(system2, filePrefix + "-grad.txt")
    }

    private def saveSystem(s: NeuroFuzzySystem, file: String): Unit = {
        val writer = Files.newBufferedWriter(Paths.get(file))

        writer.write(s"${s.numRules},p,q,r,ax,bx,ay,by")
        writer.newLine()

        for (i <- 0 until s.numRules) {
            writer.write(s"$i,${s.p(i)},${s.q(i)},${s.r(i)},${s.ax(i)},${s.bx(i)},${s.ay(i)},${s.by(i)}")
            writer.newLine()
        }

        writer.close()
    }

    private def sq(x: Double): Double = x * x

    private def prepareData(): List[(Double, Double, Double)] = (for {
        x <- -4 to 4
        y <- -4 to 4
    } yield {
        val out = (sq(x - 1.0) + sq(y + 2.0) - 5.0 * x * y + 3.0) * sq(Math.cos(x / 5.0))
        (x.toDouble, y.toDouble, out)
    }).toList

    private def printUsage(): Nothing = {
        println("Usage: [params...] outFilePrefix")
        println("       --load inFile")
        println("Params:")
        println("    -r <number>         Number of rules, default: 6")
        println("    -i <number>         Maximum number of iterations, default: 10000")
        println("    -e <decimal>        Target error, default: 10e-7")
        println("    --step <decimal>    Learning step of backpropagation algorithm, default: 0.0008")
        println("    --log-errors        Log error from each iteration into files 'sto-err.txt' and 'grad-err.txt'")
        exit(0)
    }

    private def exit(status: Int): Nothing = {
        System.exit(status)
        throw new IllegalStateException("should have exited")
    }

    private def extractParams(args: Array[String]): Params = {
        type Setter = String => Unit

        var numOfRules = 6
        val numOfRulesSetter: Setter = a => numOfRules = a.toInt

        var maxIterations = 10000
        val maxIterationsSetter: Setter = a => maxIterations = a.toInt

        var targetError = 10e-7
        val targetErrorSetter: Setter = a => targetError = a.toDouble

        var step = 0.0008
        val stepSetter: Setter = a => step = a.toDouble

        var logErrors: Boolean = false

        val noSetter: String => Unit = _ => {}
        var setter: String => Unit = noSetter

        for (arg <- args) {
            if (setter eq noSetter) {
                arg match {
                    case "-r" => setter = numOfRulesSetter
                    case "-i" => setter = maxIterationsSetter
                    case "-e" => setter = targetErrorSetter
                    case "--step" => setter = stepSetter
                    case "--log-errors" => logErrors = true
                    case "--load" => {
                        println("'--load' is not allowed here")
                        exit(1)
                    }
                    case _ => {
                        println(s"Unknown argument: $arg")
                        exit(1)
                    }
                }
            } else {
                setter(arg)
                setter = noSetter
            }
        }

        (numOfRules, maxIterations, targetError, step, logErrors)
    }
}
