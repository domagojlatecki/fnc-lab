package at.doml.fnc.lab1

import at.doml.fnc.lab1.controller.{AngleFuzzySystem, AngleRules, CenterOfAreaDefuzzifier, Defuzzifier, FuzzySystem}
import scala.io.StdIn

object AngleRuleTest extends App {

    val tNorm: (Double*) => Double = if (this.args.length > 0 && this.args(0) == "-prod") {
        items => items.product
    } else {
        items => items.min
    }

    println("Rules:")
    println(
        "[0] When left close or critically close and left-angled close or critically close then steer sharply right"
    )
    println("[1] When left close and left-angled close or comfortably close then steer slightly right")
    println("[2] When both comfortably close go straight")
    println("[3] When right close and right-angled close or comfortably close then steer slightly left")
    println(
        "[4] When right close or critically close and right-angled close or critically close then steer sharply left"
    )
    println("[-] Selects all rules")
    println("Input rule index: ")

    val input = StdIn.readLine()
    val rules = if (input == "-") {
        AngleRules.rules(tNorm)
    } else {
        List(AngleRules.rules(tNorm)(input.toInt))
    }
    val defuzzifier: Defuzzifier = new CenterOfAreaDefuzzifier
    val angleFuzzySystem: FuzzySystem = new AngleFuzzySystem(defuzzifier, rules, true)

    println("Input values: left right leftAngled rightAngled velocity direction")

    val (left, right, leftAngled, rightAngled, velocity, direction) = readInput()
    val decision = angleFuzzySystem.decide(left, right, leftAngled, rightAngled, velocity, direction)

    println("Decision: " + decision)

    private def readInput(): (Int, Int, Int, Int, Int, Int) = {
        val in = StdIn.readLine().trim
        val values = in.split(' ').map(_.toInt)

        (values(0), values(1), values(2), values(3), values(4), values(5))
    }
}
