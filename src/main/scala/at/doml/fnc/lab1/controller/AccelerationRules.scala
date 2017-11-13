package at.doml.fnc.lab1.controller

import at.doml.fnc.lab1.controller.BaseRules.{Acceleration, Direction, Distance, Speed}
import at.doml.fnc.lab1.domain.{CompositeDomain, Domain, DomainElement}
import at.doml.fnc.lab1.set.{FuzzySet, LazyCompositeFuzzySet, Operations}

object AccelerationRules {

    private val closeOrCriticallyClose = Operations.binaryOperation(
        Distance.close,
        Distance.criticallyClose,
        Operations.zadehOr
    )

    private val fastOrModeratelyFast = Operations.binaryOperation(
        Speed.fast,
        Speed.moderatelyFast,
        Operations.zadehOr
    )

    private val domain: Domain = new CompositeDomain(
        Array(
            Distance.domain, // left : 0
            Distance.domain, // right : 1
            Distance.domain, // left angled : 2
            Distance.domain, // right angled : 3
            Speed.domain, // velocity : 4
            Direction.domain, // direction : 5
            Acceleration.domain // acceleration : 6
        )
    )

    def whenCloseOrCriticallyCloseToRiverBankAndSpeedIsFastOrModeratelyFastThenBrake(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val left = DomainElement.of(element.getComponentValue(0))
            val right = DomainElement.of(element.getComponentValue(1))
            val leftAngled = DomainElement.of(element.getComponentValue(2))
            val rightAngled = DomainElement.of(element.getComponentValue(3))
            val speed = DomainElement.of(element.getComponentValue(4))
            val acceleration = DomainElement.of(element.getComponentValue(6))
            val leftCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(left)
            val rightCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(right)
            val leftAngledCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(leftAngled)
            val rightAngledCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(rightAngled)
            val speedFastOrModeratelyFast = fastOrModeratelyFast.getValueAt(speed)
            val fastBrake = Acceleration.brake.getValueAt(acceleration)

            tNorm(
                leftCloseOrCriticallyClose max rightCloseOrCriticallyClose max
                leftAngledCloseOrCriticallyClose max rightAngledCloseOrCriticallyClose,
                speedFastOrModeratelyFast, fastBrake
            )
        })

    def whenCloseOrCriticallyCloseToRiverBankAndSpeedIsSlowThenAccelerate(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val left = DomainElement.of(element.getComponentValue(0))
            val right = DomainElement.of(element.getComponentValue(1))
            val leftAngled = DomainElement.of(element.getComponentValue(2))
            val rightAngled = DomainElement.of(element.getComponentValue(3))
            val speed = DomainElement.of(element.getComponentValue(4))
            val acceleration = DomainElement.of(element.getComponentValue(6))
            val leftCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(left)
            val rightCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(right)
            val leftAngledCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(leftAngled)
            val rightAngledCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(rightAngled)
            val speedSlow = fastOrModeratelyFast.getValueAt(speed)
            val accelerate = Acceleration.accelerate.getValueAt(acceleration)

            tNorm(
                leftCloseOrCriticallyClose max rightCloseOrCriticallyClose max
                leftAngledCloseOrCriticallyClose max rightAngledCloseOrCriticallyClose,
                speedSlow, accelerate
            )
        })

    def whenComfortablyCloseToRiverBankAndSpeedIsSlowThenAccelerate(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val left = DomainElement.of(element.getComponentValue(0))
            val right = DomainElement.of(element.getComponentValue(1))
            val leftAngled = DomainElement.of(element.getComponentValue(2))
            val rightAngled = DomainElement.of(element.getComponentValue(3))
            val speed = DomainElement.of(element.getComponentValue(4))
            val acceleration = DomainElement.of(element.getComponentValue(6))
            val leftComfortablyClose = Distance.comfortablyClose.getValueAt(left)
            val rightComfortablyClose = Distance.comfortablyClose.getValueAt(right)
            val leftAngledComfortablyClose = Distance.comfortablyClose.getValueAt(leftAngled)
            val rightAngledComfortablyClose = Distance.comfortablyClose.getValueAt(rightAngled)
            val speedSlow = Speed.slow.getValueAt(speed)
            val accelerate = Acceleration.accelerate.getValueAt(acceleration)

            tNorm(
                leftComfortablyClose max rightComfortablyClose,
                leftAngledComfortablyClose max rightAngledComfortablyClose,
                speedSlow, accelerate
            )
        })

    def rules(tNorm: (Double*) => Double): List[FuzzySet] = List(
        whenCloseOrCriticallyCloseToRiverBankAndSpeedIsFastOrModeratelyFastThenBrake(tNorm),
        whenCloseOrCriticallyCloseToRiverBankAndSpeedIsSlowThenAccelerate(tNorm),
        whenComfortablyCloseToRiverBankAndSpeedIsSlowThenAccelerate(tNorm)
    )
}
