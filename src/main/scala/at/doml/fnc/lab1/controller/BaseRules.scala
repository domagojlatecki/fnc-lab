package at.doml.fnc.lab1.controller

import at.doml.fnc.lab1.domain.{DomainElement, SimpleDomain}
import at.doml.fnc.lab1.set.{CalculatedFuzzySet, FuzzySet, MutableFuzzySet, StandardFuzzySets}

object BaseRules {

    object Distance {

        val domain: SimpleDomain = new SimpleDomain(0, 1301)

        val criticallyClose: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lFunction(10, 30)
        )

        val close: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.piFunction(20, 40, 80, 100)
        )

        val comfortablyClose: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.gammaFunction(80, 100)
        )
    }

    object Speed {

        val domain: SimpleDomain = new SimpleDomain(0, 1301)

        val slow: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lFunction(15, 30)
        )

        val moderatelyFast: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.piFunction(20, 25, 30, 40)
        )

        val fast: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.gammaFunction(35, 50)
        )
    }

    object Direction {

        val domain: SimpleDomain = new SimpleDomain(0, 2)

        val correctDirection: FuzzySet = new MutableFuzzySet(this.domain)
            .set(DomainElement.of(0), 0.0)
            .set(DomainElement.of(1), 1.0)

        val wrongDirection: FuzzySet = new MutableFuzzySet(this.domain)
            .set(DomainElement.of(0), 1.0)
            .set(DomainElement.of(1), 0.0)
    }

    object Steering {

        val domain: SimpleDomain = new SimpleDomain(-90, 91)

        val sharpRight: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lFunction(10, 30)
        )

        val right: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.piFunction(20, 30, 50, 60)
        )

        val slightlyRight: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.piFunction(50, 60, 80, 90)
        )

        val straight: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lambdaFunction(80, 90, 100)
        )

        val slightlyLeft: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.piFunction(90, 100, 120, 130)
        )

        val left: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.piFunction(120, 130, 150, 160)
        )

        val sharpLeft: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.gammaFunction(150, 170)
        )
    }

    object Acceleration {

        val domain: SimpleDomain = new SimpleDomain(-15, 16)

        val fastBrake: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lFunction(0, 5)
        )

        val brake: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lambdaFunction(5, 10, 15)
        )

        val keepSpeed: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lambdaFunction(10, 15, 20)
        )

        val accelerate: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.lambdaFunction(15, 20, 25)
        )

        val fastAccelerate: FuzzySet = new CalculatedFuzzySet(
            this.domain,
            StandardFuzzySets.gammaFunction(25, 30)
        )
    }
}
