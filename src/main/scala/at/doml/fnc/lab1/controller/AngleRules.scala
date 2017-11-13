package at.doml.fnc.lab1.controller

import at.doml.fnc.lab1.controller.BaseRules.{Direction, Distance, Speed, Steering}
import at.doml.fnc.lab1.domain.{CompositeDomain, Domain, DomainElement}
import at.doml.fnc.lab1.set.{FuzzySet, LazyCompositeFuzzySet, Operations}

object AngleRules {

    private val closeOrCriticallyClose = Operations.binaryOperation(
        Distance.close,
        Distance.criticallyClose,
        Operations.zadehOr
    )

    private val closeOrComfortablyClose = Operations.binaryOperation(
        Distance.close,
        Distance.comfortablyClose,
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
            Steering.domain // steering : 6
        )
    )

    def whenLeftCloseOrCriticallyCloseAndLeftAngledCloseOrCriticallyCloseThenSteerSharplyRight(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val left = DomainElement.of(element.getComponentValue(0))
            val leftAngled = DomainElement.of(element.getComponentValue(2))
            val angle = DomainElement.of(element.getComponentValue(6))
            val leftCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(left)
            val leftAngledCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(leftAngled)
            val steerSharplyRight = Steering.sharpRight.getValueAt(angle)

            tNorm(leftCloseOrCriticallyClose, leftAngledCloseOrCriticallyClose, steerSharplyRight)
        })

    def whenLeftCloseAndLeftAngledCloseOrComfortablyCloseThenSteerSlightlyRight(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val left = DomainElement.of(element.getComponentValue(0))
            val leftAngled = DomainElement.of(element.getComponentValue(2))
            val angle = DomainElement.of(element.getComponentValue(6))
            val leftClose = Distance.close.getValueAt(left)
            val leftAngledCloseOrComfortablyClose = closeOrComfortablyClose.getValueAt(leftAngled)
            val steerRight = Steering.slightlyRight.getValueAt(angle)

            tNorm(leftClose, leftAngledCloseOrComfortablyClose, steerRight)
        })

    def whenBothComfortablyCloseGoStraight(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val left = DomainElement.of(element.getComponentValue(0))
            val right = DomainElement.of(element.getComponentValue(1))
            val angle = DomainElement.of(element.getComponentValue(6))
            val leftComfortablyClose = Distance.comfortablyClose.getValueAt(left)
            val rightComfortablyClose = Distance.comfortablyClose.getValueAt(right)
            val goStraight = Steering.straight.getValueAt(angle)

            tNorm(leftComfortablyClose, rightComfortablyClose, goStraight)
        })

    def whenRightCloseAndRightAngledCloseOrComfortablyCloseThenSteerSlightlyLeft(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val right = DomainElement.of(element.getComponentValue(1))
            val rightAngled = DomainElement.of(element.getComponentValue(3))
            val angle = DomainElement.of(element.getComponentValue(6))
            val rightClose = Distance.close.getValueAt(right)
            val rightAngledCloseOrComfortablyClose = closeOrComfortablyClose.getValueAt(rightAngled)
            val steerLeft = Steering.slightlyLeft.getValueAt(angle)

            tNorm(rightClose, rightAngledCloseOrComfortablyClose, steerLeft)
        })

    def whenRightCloseOrCriticallyCloseAndRightAngledCloseOrCriticallyCloseThenSteerSharplyLeft(
        tNorm: (Double*) => Double
    ) = new LazyCompositeFuzzySet(this.domain, element => {
            val right = DomainElement.of(element.getComponentValue(1))
            val rightAngled = DomainElement.of(element.getComponentValue(3))
            val angle = DomainElement.of(element.getComponentValue(6))
            val rightCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(right)
            val rightAngledCloseOrCriticallyClose = closeOrCriticallyClose.getValueAt(rightAngled)
            val steerSharplyLeft = Steering.sharpLeft.getValueAt(angle)

            tNorm(rightCloseOrCriticallyClose, rightAngledCloseOrCriticallyClose, steerSharplyLeft)
        })

    def rules(tNorm: (Double*) => Double): List[FuzzySet] = List(
        whenLeftCloseOrCriticallyCloseAndLeftAngledCloseOrCriticallyCloseThenSteerSharplyRight(tNorm),
        whenLeftCloseAndLeftAngledCloseOrComfortablyCloseThenSteerSlightlyRight(tNorm),
        whenBothComfortablyCloseGoStraight(tNorm),
        whenRightCloseAndRightAngledCloseOrComfortablyCloseThenSteerSlightlyLeft(tNorm),
        whenRightCloseOrCriticallyCloseAndRightAngledCloseOrCriticallyCloseThenSteerSharplyLeft(tNorm)
    )
}
