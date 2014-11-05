package uc.lambda

import org.scalatest._

class FizzBuzzSpec extends FlatSpec with Matchers {

  val fizzbuzz: List[String] = FizzBuzz.make

  "FizzBuzz" should "be list of number, fizz, buzz and fizzbuzz" in {
    fizzbuzz(0) should be ("FizzBuzz")
    fizzbuzz(2) should be ("2")
    fizzbuzz(3) should be ("Fizz")
    fizzbuzz(5) should be ("Buzz")
    fizzbuzz(15) should be ("FizzBuzz")
    fizzbuzz(30) should be ("FizzBuzz")
    fizzbuzz(33) should be ("Fizz")
    fizzbuzz(95) should be ("Buzz")
  }

}