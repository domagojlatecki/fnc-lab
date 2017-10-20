package at.doml.fnc.lab1.relations

import at.doml.fnc.lab1.domain.{Domain, DomainElement}
import at.doml.fnc.lab1.set.{FuzzySet, MutableFuzzySet}

object Relations {

    def isUTimesURelation(relation: FuzzySet): Boolean = {
        if (relation.domain.numberOfComponents != 2) {
            return false
        }

        val c1 = relation.domain.getComponent(0)
        val c2 = relation.domain.getComponent(1)

        if (c1.cardinality != c2.cardinality) {
            return false
        }

        if (c1.numberOfComponents != 1 || c2.numberOfComponents != 1) {
            return false
        }

        for (i <- 0 until c1.cardinality) {
            if (c1.elementFor(i) != c2.elementFor(i)) {
                return false
            }
        }

        true
    }

    private def getSetElements(index: Int)(implicit set: FuzzySet): Iterable[Int] = {
        set.domain.getComponent(index)
            .map(_.getComponentValue(0))
    }

    private def evaluate(first: Int, second: Int)(implicit set: FuzzySet): Double = {
        set.getValueAt(DomainElement.of(first, second))
    }

    def isSymmetric(relation: FuzzySet): Boolean = {
        implicit val r: FuzzySet = relation
        this.isUTimesURelation(r) && this.checkSymmetry
    }

    private def checkSymmetry(implicit relation: FuzzySet): Boolean = {
        val u = this.getSetElements(0)

        for {
            x <- u
            y <- u
        } {
            val xy = this.evaluate(x, y)
            val yx = this.evaluate(y, x)

            if (xy != yx) {
                return false
            }
        }

        true
    }

    def isReflexive(relation: FuzzySet): Boolean = {
        implicit val r: FuzzySet = relation
        this.isUTimesURelation(r) && this.checkReflexion
    }

    private def checkReflexion(implicit relation: FuzzySet): Boolean = {
        val u = this.getSetElements(0)

        for {
            x <- u
        } {
            if (this.evaluate(x, x) != 1.0) {
                return false
            }
        }

        true
    }

    def isMaxMinTransitive(relation: FuzzySet): Boolean = {
        implicit val r: FuzzySet = relation
        this.isUTimesURelation(r) && this.checkMinMaxTransitivity
    }

    private def checkMinMaxTransitivity(implicit relation: FuzzySet): Boolean = {
        val u = this.getSetElements(0)

        for {
            x <- u
            z <- u
        } {
            val xz = this.evaluate(x, z)
            val max = (for (y <- u) yield {
                val xy = this.evaluate(x, y)
                val yz = this.evaluate(y, z)

                xy min yz
            }).max

            if (xz < max) {
                return false
            }
        }

        true
    }

    def compositionOfBinaryRelations(r1: FuzzySet, r2: FuzzySet): FuzzySet = {
        this.checkCompositionCompatibility(r1, r2)

        val u = this.getSetElements(0)(r1)
        val v = this.getSetElements(1)(r1)
        val w = this.getSetElements(1)(r2)

        val newDomain = Domain.combine(r1.domain.getComponent(0), r2.domain.getComponent(1))
        val newRelation = new MutableFuzzySet(newDomain)

        for {
            x <- u
            z <- w
        } {
            val xz = (for (y <- v) yield {
                val xy = this.evaluate(x, y)(r1)
                val yz = this.evaluate(y, z)(r2)

                xy min yz
            }).max

            newRelation.set(DomainElement.of(x, z), xz)
        }

        newRelation
    }

    private def checkCompositionCompatibility(r1: FuzzySet, r2: FuzzySet): Unit = {
        if (r1.domain.numberOfComponents != 2 || r2.domain.numberOfComponents != 2) {
            throw new IllegalArgumentException("Provided relations must be binary")
        }

        val u = r1.domain.getComponent(0)
        val v1 = r1.domain.getComponent(1)
        val v2 = r2.domain.getComponent(0)
        val w = r2.domain.getComponent(0)

        if (u.numberOfComponents != 1 || v1.numberOfComponents != 1 ||
            v2.numberOfComponents != 1 || w.numberOfComponents != 1) {
            throw new IllegalArgumentException("Provided relations must be composed of simple domains")
        }

        lazy val incompatibleRelationsMessage = "Provided relations are not compatible"

        if (v1.cardinality != v2.cardinality) {
            throw new IllegalArgumentException(incompatibleRelationsMessage)
        }

        for (i <- 0 until v1.cardinality) {
            if (v1.elementFor(i) != v2.elementFor(i)) {
                throw new IllegalArgumentException(incompatibleRelationsMessage)
            }
        }
    }

    def isFuzzyEquivalence(relation: FuzzySet): Boolean = {
        implicit val r: FuzzySet = relation
        this.isUTimesURelation(r) && this.checkSymmetry && this.checkReflexion && this.checkMinMaxTransitivity
    }
}
