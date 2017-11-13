package at.doml.fnc.lab1.controller

import at.doml.fnc.lab1.controller.BaseRules.Steering
import at.doml.fnc.lab1.domain.DomainElement
import at.doml.fnc.lab1.set.{FuzzySet, MutableFuzzySet}

class AngleFuzzySystem(df: Defuzzifier, val rules: List[FuzzySet],
                       val debug: Boolean = false) extends FuzzySystem {

    override protected val defuzzifier: Defuzzifier = df

    override def decide(left: Int, right: Int,
                        leftAngled: Int, rightAngled: Int,
                        velocity: Int, direction: Int): Int = {
        val possibleDecisions = Steering.domain.map(
            e => DomainElement.of(left, right, leftAngled, rightAngled, velocity, direction, e.getComponentValue(0))
        )
        val steeringSet = new MutableFuzzySet(Steering.domain)

        for (decision <- possibleDecisions) {
            val angle = DomainElement.of(decision.getComponentValue(6))
            val max = this.rules.map(_.getValueAt(decision)).max

            steeringSet.set(angle, max)
        }

        if (debug) {
            print("steeringSet = ")
            println(steeringSet)
        }

        defuzzifier.defuzzify(steeringSet)
    }
}
