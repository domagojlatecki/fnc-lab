package at.doml.fnc.lab3.neuralnetwork

class Layer(transferFunction: TransferFunction, numInputs: Int, val numNeurons: Int) {

    private val neurons: List[Neuron] = (for (_ <- 0 until numNeurons) yield {
        new Neuron(transferFunction, numInputs)
    }).toList

    def apply(inputs: Array[Double]): Array[Double] = {
        (for (neuron <- this.neurons) yield {
            neuron(inputs)
        }).toArray
    }

    def weightAt(index: Int, neuronIndex: Int): Double = this.neurons(neuronIndex).weightAt(index)

    def getNumWeights(neuronIndex: Int): Int = this.neurons(neuronIndex).numInputs

    def updateWeight(index: Int, neuronIndex: Int, change: Double): Unit = this.neurons(neuronIndex)(index) = change
}
