package esadykov.math

import org.apache.commons.math3.linear._

/**
 * @author Ernest Sadykov
 * @since 13.05.2014
 */
object ConsistencyChecker {
    def main(args: Array[String]) {
        val doubleCoeff: Array[Array[Double]] = Array(Array[Double](0, 1, -1),
            Array[Double](0, 1, -1),
            Array[Double](1, 0, -1))
        val doubleConsts: Array[Double] = Array[Double](-1, 0, 0)
        //        val doubleCoeff: Array[Array[Double]] = Array(Array[Double](1, 0, -1),
        //                                                      Array[Double](1, 0, -1),
        //                                                      Array[Double](1, -1, 0))
        //        val doubleConsts: Array[Double] = Array[Double](2, 2, 1)

        val coeff: RealMatrix  = new Array2DRowRealMatrix(doubleCoeff, false)
        val solver: DecompositionSolver = new LUDecomposition(coeff).getSolver
        val consts: ArrayRealVector = new ArrayRealVector(doubleConsts, false)

        val solution: RealVector = try {
            solver.solve(consts)
        } catch {
            case e: SingularMatrixException => println(e); null
        }

        println(solution)

        val doubleCoeff2: Array[Array[Double]] = Array(Array[Double](0, 1, -1, -1),
            Array[Double](0, 1, -1, 0),
            Array[Double](1, 0, -1, 0))
        val coeff2: RealMatrix  = new Array2DRowRealMatrix(doubleCoeff2, false)


        println("coef rank: " + new SingularValueDecomposition(coeff).getRank +
            " arg rank: " + new SingularValueDecomposition(coeff2).getRank)
    }
//    def check(first: Map[String, List[Int]]): Boolean = {
//        val coeff: Array[Array[Double]] = Array.ofDim(first.keys.size, first.keys.size)
//        for ((key, value) <- first) {
//
//        }
//    }
}
