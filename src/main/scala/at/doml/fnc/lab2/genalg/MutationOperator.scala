package at.doml.fnc.lab2.genalg

trait MutationOperator[T] {

    def mutate(chromosome: T): T
}
