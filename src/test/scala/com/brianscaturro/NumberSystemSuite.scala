/**
 * Some tests for the number system
 */
package com.brianscaturro

import org.scalatest._
import scala.math._

class NumberSystemSuite extends FunSuite {

    test("the absolute values of a number and its opposite are equal") {
        val x = -25; val y = 25
        assert(abs(x) === abs(y))
    }

    /**
     * Rule:
     * To add two numbers having opposite sign, find the
     * difference of their absolute values and prefix the
     * sign of the number having the larger absolute value
     */
    test("adding two numbers of different sign") {
        val solution = (9) + (-5)

        val ruleApplied = +(abs(9) - abs(-5))

        assert(solution === ruleApplied)
        assert(solution === 4)
    }

    /**
     * Rule:
     * To add two signed numbers have like signs, add
     * their absolute values and prefix their common sign
     */
    test("adding two numbers of same sign") {
        val solution = (-4) + (-13)

        val ruleApplied = -(abs(4) + abs(13))

        assert(solution === ruleApplied)
        assert(solution === -17)
    }

    /**
     * Rule:
     * To subtract a number, add its opposite
     */
    test("(+4) - (-2)") {
        val solution = (4) - (-2)

        val ruleApplied = (4) + (2)

        assert(solution === ruleApplied)
        assert(solution === 6)
    }

    test("(-7) - (-9) - (+4)") {
        val solution = (-7) - (-9) - (+4)

        val ruleApplied = (-7) + (+9) + (-4)

        val grouped = (-11) + (+9)

        val addRuleApplied = -(abs(-11) - abs(9))

        assert(solution === ruleApplied)
        assert(solution === grouped)
        assert(solution === addRuleApplied)
        assert(solution === -2)
    }

    /**
     * Rule:
     * When multiplying two signed numbers, if the signs
     * are the same, the product will be positive
     */
    test("multiplication of two numbers of same sign is positive") {
        val x = -9; val y = -8
        val solution = x * y

        assert(solution > 0)
        assert(solution === 72)
    }

    /**
     * Rule:
     * When multiplying two signed numbers, if the signs of
     * the numbers are different, the product will be negative
     */
    test("multiplication of two numbers of different sign is negative") {
        val x = 3; val y = -7
        val solution = x * y

        assert(solution < 0)
        assert(solution === -21)
    }

    /**
     * Rule:
     * Regardless of the number of factors, the product of
     * more than two numbers is always negative if there are
     * an odd number of negative factors, and positive if
     * there are an even number of negative factors.
     */
    test("multiplication of three factors with two negative factors is positve") {
        val solution = (-3) * (+3) * (-4)

        assert(solution > 0)
        assert(solution === 36)
    }

    test("multiplication of four factors with three negative factors is negative") {
        val solution = (-5) * (-7) * (-1) * (4)

        assert(solution < 0)
        assert(solution === -140)
    }

    test("negative number raised to odd exponent is negative") {
        val solution = pow(-3, 3)

        val factoredThreeTimes = (-3) * (-3) * (-3)

        assert(solution === factoredThreeTimes)
        assert(solution < 0)
        assert(solution === -27)
    }

    test("negative number raised to even exponent is positive") {
        val solution = pow(-4, 2)

        val factoredTwice = (-4) * (-4)

        assert(solution === factoredTwice)
        assert(solution > 0)
        assert(solution === 16)
    }

    test("division of two numbers of same sign is positive") {
        //make these floating point so the fractional result is correct
        val x = -15f; val y = -6f
        val solution = x/y

        assert(solution > 0)
        assert(solution === 2.5)
    }

    test("division of two number of different sign is negative") {
        val x = -24; val y = 8
        val solution = x/y

        assert(solution < 0)
        assert(solution === -3)
    }

    test("(+2)(-3) / (-1)(-4)") {
        val solution = ((+2f) * (-3f)) / ((-1f) * (-4f))

        val numeratorAndDenominatorEvaluated = -6f / 4f

        assert(solution === numeratorAndDenominatorEvaluated)
        assert(solution === -1.5) 
    }

    /**
     * Rule:
     * (a) To evaluate an expression containing signed numbers,
     *     first substitute the values given for the letters, en-
     *     closing them in parentheses
     *
     * (b) Perform the indicated operations in the correct order.
     *     If possible, simplify inside parentheses first; next,
     *     apply any exponents; and finally, follow remaining order
     *     of operations
     */
     test("abc ^ 2 for a = 1, b = 2, c = -3") {
        val a = 1; val b = 2; val c = -3
        val solution = a * b * pow(c, 2)

        val replaced = (1) * (2) * pow(c, 2)

        assert(solution === replaced)
        assert(solution === 18)
     }

     test("2x - 3y / 4z for x = -2, y = -1, z = 3") {
        val x = -2; val y = -1; val z = 3;
        val solution = ((2 * x) - (3 * y)) / 4 * z

        val replaced = ((2 * (-2)) - (3 * (-1))) / 4 * (3)

        assert(solution === replaced)
        assert(solution === -(1/12))
     }
}

