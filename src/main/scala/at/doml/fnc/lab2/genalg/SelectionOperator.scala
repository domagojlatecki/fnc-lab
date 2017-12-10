package at.doml.fnc.lab2.genalg

trait SelectionOperator {

    def select[T](population: List[Chromosome[T]]): (List[Chromosome[T]], List[Chromosome[T]])
}
