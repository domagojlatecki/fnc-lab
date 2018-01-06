package at.doml.fnc.lab3

import at.doml.fnc.lab3.neuralnetwork.NeuralNetwork

object Backpropagation {

    type Sample = (Array[Double], Array[Double])
    type SampleBatch = List[Sample]

    private def sq(x: Double) = x * x

    private def calculateError(neuralNetwork: NeuralNetwork, samples: List[SampleBatch], numSamples: Int): Double = {
        (for {
            batch <- samples
            (input, expectedOutput) <- batch
        } yield {
            val networkOutputs = neuralNetwork(input)

            (for (i <- networkOutputs.indices) yield {
                sq(networkOutputs(i) - expectedOutput(i))
            }).sum
        }).sum / (2.0 * numSamples)
    }

    def apply(neuralNetwork: NeuralNetwork, learningStep: Double, trainingSamples: List[SampleBatch],
              maxIterations: Int, targetError: Double): Unit = {
        val numSamples = trainingSamples.map(_.size).sum
        var iteration: Int = 0
        var currentError = calculateError(neuralNetwork, trainingSamples, numSamples)

        println(s"Initial error: $currentError")

        while (iteration < maxIterations && currentError > targetError) {
            for (batch <- trainingSamples) {
                var delta: Array[Array[Double]] = null

                for (layerIndex <- neuralNetwork.layers.indices.reverse) {
                    if (delta == null) {
                        delta = updateLastLayer(batch, neuralNetwork, learningStep, layerIndex)
                    } else {
                        delta = updateMiddleLayer(batch, neuralNetwork, learningStep, layerIndex, delta)
                    }
                }
            }

            iteration += 1
            currentError = calculateError(neuralNetwork, trainingSamples, numSamples)

            println(s"Iteration $iteration error: $currentError")
        }
    }

    private def updateLastLayer(batch: SampleBatch, neuralNetwork: NeuralNetwork, learningStep: Double,
                                layerIndex: Int): Array[Array[Double]] = {
        val y: Array[Array[Double]] = (for ((sample, _) <- batch) yield {
            neuralNetwork(sample)
        }).toArray
        val t: Array[Array[Double]] = batch.map(_._2).toArray
        val delta: Array[Array[Double]] = new Array(y.length)

        for (i <- delta.indices) {
            delta(i) = new Array(neuralNetwork.getNumNeurons(layerIndex))
        }

        for {
            j <- 0 until neuralNetwork.getNumNeurons(layerIndex)
            i <- 0 until neuralNetwork.getNumWeights(layerIndex, j)
        } {
            val change = (for (s <- batch.indices) yield {
                delta(s)(j) = y(s)(j) * (1.0 - y(s)(j)) * (t(s)(j) - y(s)(j))
                delta(s)(j) * neuralNetwork.getOutputBeforeNthLayer(batch(s)._1, layerIndex)(i)
            }).sum * learningStep

            neuralNetwork.updateWeight(i, j, layerIndex, change)
        }

        for (j <- 0 until neuralNetwork.getNumNeurons(layerIndex)) {
            val change = (for (s <- batch.indices) yield {
                delta(s)(j)
            }).sum * learningStep

            neuralNetwork.updateWeight(neuralNetwork.getNumWeights(layerIndex, j), j, layerIndex, change)
        }

        delta
    }

    private def updateMiddleLayer(batch: SampleBatch, neuralNetwork: NeuralNetwork, learningStep: Double,
                                  layerIndex: Int, previousDelta: Array[Array[Double]]): Array[Array[Double]] = {
        val y: Array[Array[Double]] = (for ((sample, _) <- batch) yield {
            neuralNetwork.getOutputBeforeNthLayer(sample, layerIndex + 1)
        }).toArray
        val delta: Array[Array[Double]] = new Array(y.length)

        for (i <- delta.indices) {
            delta(i) = new Array(neuralNetwork.getNumNeurons(layerIndex))
        }

        for {
            j <- 0 until neuralNetwork.getNumNeurons(layerIndex)
            i <- 0 until neuralNetwork.getNumWeights(layerIndex, j)
        } {
            val change = (for (s <- batch.indices) yield {
                val nextCorrectionSum = (for (o <- previousDelta(s).indices) yield {
                    previousDelta(s)(o) * neuralNetwork.weightAt(j, o, layerIndex + 1)
                }).sum

                delta(s)(j) = y(s)(j) * (1.0 - y(s)(j)) * nextCorrectionSum
                delta(s)(j) * neuralNetwork.getOutputBeforeNthLayer(batch(s)._1, layerIndex)(i)
            }).sum * learningStep

            neuralNetwork.updateWeight(i, j, layerIndex, change)
        }

        for (j <- 0 until neuralNetwork.getNumNeurons(layerIndex)) {
            val change = (for (s <- batch.indices) yield {
                delta(s)(j)
            }).sum * learningStep

            neuralNetwork.updateWeight(neuralNetwork.getNumWeights(layerIndex, j), j, layerIndex, change)
        }

        delta
    }
}
