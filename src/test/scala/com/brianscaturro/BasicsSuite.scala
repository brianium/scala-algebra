/**
 * Some tests to assert basic algebra principles
 */
import org.scalatest._
import scala.math._

class BasicsSuite extends FunSuite {

    test("division by zero is impossible") {
       val x = 3
       intercept[ArithmeticException] {
           7 / (x - 3)        
       }
    }

    //order of operations
    test("2(3 + 2) - 7 + 9/3") {
        val solution = 2 * (3 + 2) - 7 + 9/3

        //parentheses
        val parensFirst = 2 * 5 - 7 + 9/3

        //multiplication and division
        val multiplyAndDivideLeftToRight = 10 - 7 + 3

        //add and subract
        val addAndSubtractLeftToRight = 6

        assert(parensFirst === solution)
        assert(multiplyAndDivideLeftToRight === solution)
        assert(addAndSubtractLeftToRight === solution)
        assert(solution === 6)
    }

    test("(9 + 3)/4 - 1 - 3 + (6 ÷ 2)") {
        val solution = (9 + 3)/(4 - 1) - 3 + (6/2)

        val parensFirst = 12/3 - 3 + 3

        val divide = 4 - 3 + 3

        val addAndSubtractLeftToRight = 4

        assert(parensFirst === solution)
        assert(divide === solution)
        assert(addAndSubtractLeftToRight === solution)
        assert(solution === 4)
    }

    //evaluating expressions with substitution
    test("(a ^ 2 * c) / 4 + (ac ^ 2) / 6 + c for a = 2, c = 3") {
        val a = 2; val c = 3;
        
        val solution = ((pow(a, 2) * c) / 4) + ((a * pow(c, 2)) / 6) + c

        val substituted = ((pow(2, 2) * 3) / 4) + ((2 * pow(3, 2)) / 6) + 3

        assert(substituted === solution)
        assert(solution === 9)
    }

    //condense factors into powers
    test("mmmy - xx + mx for m = 3, y = 2, x = 5") {
        val m = 3; val y = 2; val x = 5;

        val solution = m * m * m * y - x * x + m * x

        val withExponents = pow(m, 3) * y - pow(x, 2) + m * x

        assert(solution === withExponents)
    }

    //simplification
    test("2a + 3b + 3a - b") {
        //these values arent as important as seeing the grouping of like terms
        val a = 6; val b = 7

        val solution = 2 * a +  3 * b + 3 * a - b

        //group like terms, ie 2a + 3a and 3b - b
        val grouped = 5 * a + 2 * b

        assert(solution === grouped)
    }

    test("ax ^ 2 + by + b ^ 2 + 3ax ^ 2") {
        val a = 4; val x = 7; val b = 3; val y = 5

        val solution = a * pow(x, 2) + b * y + pow(b, 2) + 3 * a * pow(x, 2)

        val grouped = 4 * a * pow(x, 2) + b * y + pow(b, 2)

        assert(solution === grouped)
    }
}