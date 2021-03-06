package esadykov.math

import org.apache.commons.math3.linear._
import scala.collection.immutable.SortedSet

/**
 * @author Ernest Sadykov
 * @since 13.05.2014
 */
object ConsistencyChecker {
    type AlgebraComponents = List[Map[String, List[String]]]
    type Equations = List[(List[String], Int)]

    // TODO: make functional
    def check(components1: AlgebraComponents,
              components2: AlgebraComponents): Boolean = {
        for {c1 <- components1
             c2 <- components2} {
            var equations: Equations = Nil
            for {f <- c1
                 s <- c2
                 if f._1 == s._1} {
                equations = equations :+ (f._2.filter(_.count(!_.isDigit) > 0) ::: s._2.filter(_.count(!_.isDigit) > 0).map("-" + _),
                    s._2.find(_ forall (_.isDigit)).getOrElse("0").toInt - f._2.find(_ forall (_.isDigit)).getOrElse("0").toInt)
            }
            for {f <- c1
                 if !c2.contains(f._1)} {
                equations = equations :+ (f._2, 0)
            }
            for {s <- c2
                 if !c1.contains(s._1)} {
                equations = equations :+ (s._2, 0)
            }
            if (checkPair(equations)) return true
        }
        false
    }

    private def checkPair(equations0: Equations): Boolean = {
        def makeRow(uniques: IndexedSeq[String], coeff: List[String]): Array[Double] = {
            uniques.map(s => {
                if (coeff.contains(s)) 1.0
                else if (coeff.contains("-"+s)) -1.0
                else 0.0
            }).toArray
        }

        /**
         * @return true if equations contain elements like (List(1), 0),
         *         false otherwise
         */
        def containsWrongRows(equations: Equations): Boolean = {
            for {e <- equations
                 variableName <- e._1
                 if variableName.find(!_.isDigit).isEmpty
                 if e._2 != variableName.foldLeft(0)((acc, ch) => acc + ch.toInt)} {
                return true
            }
            false
        }

        val equations = equations0.filterNot(el => el._1.isEmpty && el._2 == 0)
        if (containsWrongRows(equations)) return false
        val uniqueVars = equations.foldLeft(SortedSet.empty[String]) {
            (set: SortedSet[String], lst: (List[String], Int)) => set ++ lst._1.map(_.stripPrefix("-"))
        }.toList.sorted

        val doubleCoeff: Array[Array[Double]] = Array.ofDim(equations.size, uniqueVars.size)

        var counter = 0
        for (equation <- equations) {
            doubleCoeff(counter) = makeRow(uniqueVars.toIndexedSeq, equation._1)
            counter = counter + 1
        }
        if (equations.isEmpty) return true

        val doubleConsts: Array[Double] = equations.map(_._2.toDouble).toArray[Double]

        val (coeffRank, augmentedRank) = ranks(doubleCoeff, doubleConsts)
//        println("coeffRank: " + coeffRank + ", augmentedRank: " + augmentedRank)
        if (coeffRank < augmentedRank) false
        else if (coeffRank > augmentedRank) true
        else if (coeffRank == uniqueVars.size) solve(doubleCoeff, doubleConsts).find(_ < 0).isEmpty
        else true
    }

    /**
     * @param doubleCoeff coefficient matrix
     * @param doubleConsts solution vector
     * @return pair: _1 - rank of the coefficient matrix,
     *               _2 - rank of the augmented matrix
     */
    private def ranks(doubleCoeff: Array[Array[Double]], doubleConsts: Array[Double]): (Int, Int) = {

        var doubleAugmented: Array[Array[Double]] = Array.empty
        var counter = 0
        for (ar <- doubleCoeff) {
            doubleAugmented = doubleAugmented :+ (ar :+ doubleConsts(counter))
            counter = counter + 1
        }
        val augmented: RealMatrix = new Array2DRowRealMatrix(doubleAugmented, false)

        val coeff: RealMatrix = new Array2DRowRealMatrix(doubleCoeff, false)

        (new SingularValueDecomposition(coeff).getRank, new SingularValueDecomposition(augmented).getRank)
    }

    private def solve(doubleCoeff: Array[Array[Double]], doubleConsts: Array[Double]): Array[Double] = {

        val coeff: RealMatrix  = new Array2DRowRealMatrix(doubleCoeff, false)
        val solver: DecompositionSolver = new QRDecomposition(coeff).getSolver
        val consts: ArrayRealVector = new ArrayRealVector(doubleConsts, false)

        solver.solve(consts).toArray
    }
}
