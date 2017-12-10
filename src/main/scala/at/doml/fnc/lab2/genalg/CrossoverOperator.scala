package at.doml.fnc.lab2.genalg

trait CrossoverOperator[T] {

    def doCrossover(parents: (T, T)): T
}
