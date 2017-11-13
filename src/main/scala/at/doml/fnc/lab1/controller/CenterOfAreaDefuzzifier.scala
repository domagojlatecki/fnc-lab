package at.doml.fnc.lab1.controller

import at.doml.fnc.lab1.set.FuzzySet

class CenterOfAreaDefuzzifier extends Defuzzifier {

    override def defuzzify(set: FuzzySet): Int = {
        if (set.domain.numberOfComponents != 1) {
            throw new IllegalArgumentException("Set with simple domain is expected")
        }

        var numerator = 0.0
        var denominator = 0.0

        for (element <- set.domain) {
            val value = set.getValueAt(element)

            numerator += value * element.getComponentValue(0)
            denominator += value
        }

        (numerator / denominator).toInt
    }
}
