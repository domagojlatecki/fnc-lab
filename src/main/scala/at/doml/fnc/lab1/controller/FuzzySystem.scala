package at.doml.fnc.lab1.controller

trait FuzzySystem {

    protected val defuzzifier: Defuzzifier

    def decide(left: Int, right: Int, leftAngled: Int, rightAngled: Int, velocity: Int, direction: Int): Int
}
