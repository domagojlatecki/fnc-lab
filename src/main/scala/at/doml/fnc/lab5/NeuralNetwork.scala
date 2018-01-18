package at.doml.fnc.lab5

import at.doml.fnc.lab3.neuralnetwork.SigmoidTransferFunction

class NeuralNetwork(private val numN1Neurons: Int, private val layerSizes: Array[Int]) {

    require(numN1Neurons > 0)
    require(layerSizes.length > 0)
    require(layerSizes.last == 3)

    val wSize: Int = {
        var previous = numN1Neurons
        var sum = 0

        for (current <- layerSizes) {
            sum += current * (previous + 1)
            previous = current
        }

        4 * numN1Neurons + sum
    }

    def apply(x: Double, y: Double, weights: Array[Double]): (Double, Double, Double) = {
        require(weights.length == wSize)

        val in1 = Seq(x, y)
        var inN = new Array[Double](numN1Neurons)

        for (i <- 0 until numN1Neurons) {
            inN(i) = type1Neuron(in1, weights.view(4 * i, 4 * i + 2), weights.view(4 * i + 2, 4 * i + 4))
        }

        var index = numN1Neurons * 4
        var previousSize = numN1Neurons

        for (layer <- layerSizes) {
            inN = (0 until layer).map(
                i => type2Neuron(
                    inN,
                    weights.view(index + i * (previousSize + 1), index + (i + 1) * previousSize + i),
                    weights(index + (i + 1) * previousSize + i)
                )
            ).toArray
            index += layer * (previousSize + 1)
            previousSize = layer
        }

        require(inN.length == 3)

        (inN(0), inN(1), inN(2))
    }

    private def type1Neuron(inputs: Seq[Double], weights: Seq[Double], scales: Seq[Double]): Double = {
        require(inputs.length == weights.length)

        1.0 / (1.0 + (for ((x, w, s) <- (inputs, weights, scales).zipped) yield {
            (x - w).abs / s.abs
        }).sum)
    }

    private def type2Neuron(inputs: Seq[Double], weights: Seq[Double], w0: Double): Double = {
        require(inputs.length == weights.length)
        SigmoidTransferFunction(
            (for ((x, w) <- (inputs, weights).zipped) yield {
                x * w
            }).sum + w0
        )
    }
}
