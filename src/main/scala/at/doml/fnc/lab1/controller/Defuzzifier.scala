package at.doml.fnc.lab1.controller

import at.doml.fnc.lab1.set.FuzzySet

trait Defuzzifier {

    def defuzzify(set: FuzzySet): Int
}
