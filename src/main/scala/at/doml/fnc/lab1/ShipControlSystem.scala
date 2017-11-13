package at.doml.fnc.lab1

import at.doml.fnc.lab1.controller.{
    AccelerationFuzzySystem, AccelerationRules, AngleFuzzySystem,
    AngleRules, CenterOfAreaDefuzzifier, Defuzzifier, FuzzySystem
}
import scala.io.StdIn

object ShipControlSystem extends App {

    val tNorm: (Double*) => Double = if (this.args.length > 0 && this.args(0) == "-prod") {
        items => items.product
    } else {
        items => items.min
    }

    val defuzzifier: Defuzzifier = new CenterOfAreaDefuzzifier
    val accelerationFuzzySystem: FuzzySystem = new AccelerationFuzzySystem(
        defuzzifier, AccelerationRules.rules(tNorm)
    )
    val angleFuzzySystem: FuzzySystem = new AngleFuzzySystem(
        defuzzifier, AngleRules.rules(tNorm)
    )

    while (true) {
        val (left, right, leftAngled, rightAngled, velocity, direction) = readInput()
        val acceleration = accelerationFuzzySystem.decide(left, right, leftAngled, rightAngled, velocity, direction)
        val angle = angleFuzzySystem.decide(left, right, leftAngled, rightAngled, velocity, direction)

        Console.println(s"$acceleration $angle")
        Console.flush()
    }

    private def readInput(): (Int, Int, Int, Int, Int, Int) = {
        val in = StdIn.readLine().trim

        if (in == "KRAJ") {
            System.exit(0)
        }

        val values = in.split(' ').map(_.toInt)

        (values(0), values(1), values(2), values(3), values(4), values(5))
    }
}
