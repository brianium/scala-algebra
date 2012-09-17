/**
 * Some tests for monomials and polynomials
 */
package com.brianscaturro

import math._
import org.scalatest._
import scala.math._

class MonomialsAndPolynomialsSuite extends FunSuite {

    //adding polynomials
    test("Add two polynomials: (4x^2 - 3 + 2x) + (5x - 2x^2)") {
        val x = 3

        //drop parentheses
        val solution = 4 * pow(x, 2) - 3 + 2 * x + 5 * x - 2 * pow(x, 2)

        //group like terms
        val grouped = (4 - 2) * pow(x, 2) + (5 + 2) * x - 3

        //simplify
        val simplified = 2 * pow(x, 2) + 7 * x - 3


        assert(solution === grouped)
        assert(solution === simplified)
    }

    test("Add (k^3 + 8) + (2k - 1) + (3k - 4k^3)") {
        val k = 2

        //drop parentheses
        val solution = pow(k, 3) + 8 + 2 * k - 1 + 3 * k - 4 * pow(k, 3)

        //group like terms
        val grouped = (-4 + 1) * pow(k, 3) + (2 + 3) * k + 7

        //simplify
        val simplified = -3 * pow(k, 3) + 5 * k + 7

        assert(solution === grouped)
        assert(solution === simplified)
    }

    //subtracting polynomials
    test("(6a - b + 2) - (2a + 3b - 4)") {
        val a = 2; val b = 3

        //drop parentheses and use additive inverse of subtrahend
        val solution = 6 * a - b + 2 - 2 * a - 3 * b + 4

        //group like terms
        val grouped = (6 - 2) * a - (3 + 1) * b + 6

        //simplify
        val simplified = 4 * a - 4 * b + 6

        assert(solution === grouped)
        assert(grouped === simplified)
    }

    test("(9z + 6z^2 - 1) - (3 - z^2 - 4z)") {
        val z = 3

        //drop parentheses and use additive inverse of subtrahend
        val solution = 9 * z + 6 * pow(z, 2) - 1 - 3 + pow(z, 2) + 4 * z

        //group like terms
        val grouped = (6 + 1) * pow(z, 2) + (9 + 4) * z - 4

        //simplify
        val simplified = 7 * pow(z, 2) + 13 * z - 4

        assert(solution === grouped)
        assert(solution === simplified)
    }

    //multiply monomials with exponents
    test("a^3 * a^6") {
        val a = 7

        val solution = pow(a, 3) * pow(a, 6)

        //add exponents of common bases
        val simplified = pow(a, 9)

        assert(solution === simplified)
    }

    test("-2b^2c * 4b^2c(b^3)") {
        val b = 2; val c = 4

        val solution = -2 * pow(b, 2) * c * 4 * pow(b, 2) * c * pow(b, 3)

        //multiply numerical factors and add exponents of common bases
        val multiplied = (-2 * 4) *  pow(b, 7) * pow(c, 2)

        val simplified = -8 * pow(b, 7) * pow(c, 2)

        assert(solution === multiplied)
        assert(solution === simplified) 
    }

    //raising exponents to a given power
    test("(x^3)^3") {
        val x = 5

        val solution = pow(pow(x, 3), 3)

        //to find the power of a power of a base, keep the base and multiply the exponents
        val simplified = pow(x, 3 * 3)

        assert(solution === simplified)
    }

    //multiplying polynomials by monomials
    test("(b/3)(9b - 6 + 12a)") {
        val a = 2; val b = 4f

        val solution = (b/3) * (9 * b - 6 + 12 * a)

        //distribute numerator
        val distributed = (9 * pow(b, 2) - 6 * b + 12 * a * b) / 3

        //divide numerical factors by denominator
        val divided = 3 * pow(b, 2) - 2 * b + 4 * a * b

        assert(solution === distributed)
        assert(solution === divided)
    }

    //multiplying polynomials
    test("(-2x^2 + y^2 + xy) * (x - y)") {
        val x = 8; val y = 7

        val solution = (-2 * pow(x, 2) + pow(y, 2) + x * y) * (x - y)

        //multiply each term on the left by x first then multiply each term on the left by -y
        val multiplied = -2 * pow(x, 3) + pow(y, 2) * x + pow(x, 2) * y + 2 * pow(x, 2) * y - pow(y, 3) - x * pow(y, 2)

        //group like terms
        val grouped = -2 * pow(x, 3) - pow(y, 3) + 3 * pow(x, 2) * y

        assert(solution === multiplied)
        assert(solution === grouped)
    }

    //division of monomials with exponents
    test("a^8 / a^5") {
        val a = 3.0

        val solution = pow(a, 8) / pow(a, 5)

        //if exponent in numerator is greater than exponent in denominator then a^x / a^y = a^(x - y)
        val simplified = a ^ (8 - 5)

        assert(solution === simplified)
    }

    test("18xy/-3x^2y^2") {
        val x = 2f; val y = 4f 

        val solution = (18 * x * y) / (-3 * pow(x, 2) * pow(y, 2))

        //if exponent in numerator is less than exponent in denominator then a^x / a^y = 1/a^(x - y)
        //if there are numerical factors, divide those as well
        val divided = (18 / -3) / (pow(x, 2 - 1) * pow(y, 2 - 1))

        val simplified = -6 / (x * y)

        assert(solution === divided)
        assert(solution === simplified)
    }

    //dividing polynomials by monomials
    test("11m^3k^2 + 33mk^3 - 22mk / 11mk") {
        val m = 2; val k = 3

        val solution = (11 * pow(m, 3) * pow(k, 2) + 33 * m * pow(k, 3) - 22 * m * k) / (11 * m * k)

        //divide numerical factors and apply rule for dividing exponents
        val divided = pow(m, 3 - 1) * pow(k, 2 - 1) + 3 * pow(m, 1 - 1) * pow(k, 3 - 1) - 2 * pow(m, 1 - 1) * pow(k, 1 - 1)

        val simplified = pow(m, 2) * pow(k, 1) + 3 * pow(k, 2) - 2

        assert(solution === divided)
        assert(solution === simplified)
    }

}

