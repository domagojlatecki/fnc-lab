package at.doml.fnc.lab1.set

object StandardFuzzySets {

    private def checkRange(a: Int, b: Int): Unit = {
        if (a >= b) {
            throw new IllegalArgumentException(s"invalid range: [$a, $b]")
        }
    }

    def lFunction(a: Int, b: Int): Int => Double = {
        this.checkRange(a, b)

        x => {
            if (x < a) {
                1.0
            } else if (x >= b) {
                0.0
            } else {
                (b - x).toDouble / (b - a)
            }
        }
    }

    def gammaFunction(a: Int, b: Int): Int => Double = {
        this.checkRange(a, b)

        x => {
            if (x < a) {
                0.0
            } else if (x >= b) {
                1.0
            } else {
                (x - a).toDouble / (b - a)
            }
        }
    }

    def lambdaFunction(a: Int, b: Int, c: Int): Int => Double = {
        this.checkRange(a, b)
        this.checkRange(b, c)

        x => {
            if (x < a) {
                0.0
            } else if (x >= a && x < b) {
                (x - a).toDouble / (b - a)
            } else if (x >= b && x < c) {
                (c - x).toDouble / (c - b)
            } else {
                0.0
            }
        }
    }

    def piFunction(a: Int, b: Int, c: Int, d: Int): Int => Double = {
        this.checkRange(a, b)
        this.checkRange(b, c)
        this.checkRange(c, d)

        x => {
            if (x < a) {
                0.0
            } else if (x >= a && x < b) {
                (x - a).toDouble / (b - a)
            } else if (x >= b && x < c) {
                1.0
            } else if (x >= c && x < d) {
                (d - x).toDouble / (d - c)
            } else {
                0.0
            }
        }
    }
}
