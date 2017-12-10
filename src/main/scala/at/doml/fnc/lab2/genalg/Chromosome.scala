package at.doml.fnc.lab2.genalg

class Chromosome[T](val fitness: Double, val underlying: T) extends Ordered[Chromosome[_]] {

    override def compare(that: Chromosome[_]): Int = this.fitness.compareTo(that.fitness)
}
