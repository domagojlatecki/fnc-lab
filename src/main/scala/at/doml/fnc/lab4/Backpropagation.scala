package at.doml.fnc.lab4

import java.nio.file.{Files, Paths}
import at.doml.fnc.lab4.neuralnetwork.{NetworkSnapshot, NeuroFuzzySystem}

object Backpropagation {

    type InX = Double
    type InY = Double
    type Out = Double
    type Sample = (InX, InY, Out)

    private def calculateError(system: NeuroFuzzySystem, trainingSamples: List[Sample]): Double = {
        (for (sample <- trainingSamples) yield {
            val out = system(sample._1, sample._2)

            sq(out - sample._3)
        }).sum / trainingSamples.size
    }

    private def sq(x: Double): Double = x * x

    private def outErr(ns: NetworkSnapshot, t: Double): Double = -(t - ns.out)

    private def deltaP(ns: NetworkSnapshot, outErr: Double, x: Double, i: Int): Double = outErr * ns.wBar(i) * x

    private def deltaQ(ns: NetworkSnapshot, outErr: Double, y: Double, i: Int): Double = outErr * ns.wBar(i) * y

    private def deltaR(ns: NetworkSnapshot, outErr: Double, i: Int): Double = outErr * ns.wBar(i)

    private def deltaAX(ns: NetworkSnapshot, outErr: Double, bx: Double, i: Int): Double = {
        val second = ns.muX(i) * (1.0 - ns.muX(i)) * ns.muY(i) * bx
        val first = second * ns.f(i)

        outErr * (first * ns.wSum - ns.wTimesFSum * second) / sq(ns.wSum)
    }

    private def deltaAY(ns: NetworkSnapshot, outErr: Double, by: Double, i: Int): Double = {
        val second = ns.muY(i) * (1.0 - ns.muY(i)) * ns.muX(i) * by
        val first = second * ns.f(i)

        outErr * (first * ns.wSum - ns.wTimesFSum * second) / sq(ns.wSum)
    }

    private def deltaBX(ns: NetworkSnapshot, outErr: Double, x: Double, ax: Double, i: Int): Double = {
        val second = ns.muX(i) * (1.0 - ns.muX(i)) * ns.muY(i) * (ax - x)
        val first = second * ns.f(i)

        outErr * (first * ns.wSum - ns.wTimesFSum * second) / sq(ns.wSum)
    }

    private def deltaBY(ns: NetworkSnapshot, outErr: Double, y: Double, ay: Double, i: Int): Double = {
        val second = ns.muY(i) * (1.0 - ns.muY(i)) * ns.muX(i) * (ay - y)
        val first = second * ns.f(i)

        outErr * (first * ns.wSum - ns.wTimesFSum * second) / sq(ns.wSum)
    }

    def apply(system: NeuroFuzzySystem, learningStep: Double, trainingSamples: List[Sample],
              maxIterations: Int, targetError: Double, stochastic: Boolean, logErrors: Boolean): Unit = {
        var iteration: Int = 0
        var currentError = calculateError(system, trainingSamples)
        val numSamples = trainingSamples.size
        val errFileWriter = if (logErrors) {
            Files.newBufferedWriter(Paths.get(if (stochastic) {
                "sto-err.txt"
            } else {
                "grad-err.txt"
            }))
        } else {
            null
        }

        def logError(iter: Int, error: Double): Unit = {
            if (logErrors) {
                errFileWriter.write(s"$iter,$error")
                errFileWriter.newLine()
            }
        }

        println(s"Initial error: $currentError")

        logError(0, currentError)

        val dp = new Array[Double](numSamples)
        val dq = new Array[Double](numSamples)
        val dr = new Array[Double](numSamples)
        val dax = new Array[Double](numSamples)
        val day = new Array[Double](numSamples)
        val dbx = new Array[Double](numSamples)
        val dby = new Array[Double](numSamples)

        def updateWeights(): Unit = {
            for (i <- 0 until system.numRules) {
                system.p_=(i, system.p(i) - learningStep * dp(i))
                system.q_=(i, system.q(i) - learningStep * dq(i))
                system.r_=(i, system.r(i) - learningStep * dr(i))
                system.ax_=(i, system.ax(i) - learningStep * dax(i))
                system.ay_=(i, system.ay(i) - learningStep * day(i))
                system.bx_=(i, system.bx(i) - learningStep * dbx(i))
                system.by_=(i, system.by(i) - learningStep * dby(i))
                dp(i) = 0.0
                dq(i) = 0.0
                dr(i) = 0.0
                dax(i) = 0.0
                day(i) = 0.0
                dbx(i) = 0.0
                dby(i) = 0.0
            }
        }

        while (iteration < maxIterations && currentError > targetError) {
            for (sample <- trainingSamples) {
                val x = sample._1
                val y = sample._2
                val ns = system.snapshot(x, y)
                val outErr = this.outErr(ns, sample._3)

                for (i <- 0 until system.numRules) {
                    val ax = system.ax(i)
                    val bx = system.bx(i)
                    val ay = system.ay(i)
                    val by = system.by(i)

                    dp(i) += deltaP(ns, outErr, x, i)
                    dq(i) += deltaQ(ns, outErr, y, i)
                    dr(i) += deltaR(ns, outErr, i)
                    dax(i) += deltaAX(ns, outErr, bx, i)
                    day(i) += deltaAY(ns, outErr, by, i)
                    dbx(i) += deltaBX(ns, outErr, x, ax, i)
                    dby(i) += deltaBY(ns, outErr, y, ay, i)
                }

                if (stochastic) {
                    updateWeights()
                }
            }

            if (!stochastic) {
                updateWeights()
            }

            iteration += 1
            currentError = calculateError(system, trainingSamples)

            println(s"Iteration $iteration error: $currentError")
            logError(iteration, currentError)
        }

        if (logErrors) {
            errFileWriter.close()
        }
    }
}
