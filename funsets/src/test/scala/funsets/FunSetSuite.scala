package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.Predef

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s3a = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only common elements of each set") {
    new TestSets {
      val i1 = intersect(s1, s2)
      val i2 = intersect(s3, s3a)
      assert(!contains(i1, 1), "Intersect 1")
      assert(contains(i2, 3), "Intersect 2")
      assert(!contains(i2, 2), "Intersect 3")
    }
  }

  test("diff contains only elements that are in neither set") {
    new TestSets {
      val d = diff(s1, s2)
      assert(contains(d, 3), "Diff 1")
      assert(contains(d, 42), "Diff 2")
      assert(contains(d, 1234), "Diff 3")
      assert(!contains(d, 2), "Diff 1")
    }
  }

  test("filter contains elements for which the function p returns true") {
    new TestSets {
      def p (n: Int): Boolean = { n == 2 }
      val f = filter(s2, p)
      assert(contains(f, 2))
      assert(!contains(f, 3))
    }
  }

  test("forall returns whether all bounded integers within a set satisfy a given function.") {
    new TestSets {
      def p (n: Int): Boolean = { n == 2 }
      assert(forall(s2, p), "forall 1")
      def b (n: Int): Boolean = { n > 0 }
      val h =
      assert(!forall(Set(-1,0,1,2,3), b), "forall 2")
    }
  }

  test("exists returns whether there exists a bounded integer within a set that satisfies the given function") {
    new TestSets {
      def p (n: Int): Boolean = { n == 2 }
        assert(exists(s2, p), "Exists 1")
        assert(exists(Set(1,2,3), p), "Exists 2")
        assert(!exists(Set(1,3,4), p), "Exists 3")
    }
  }

  test("map returns a set transformed by applying a given function to each element of the given set.") {
    new TestSets {
      def p (n: Int): Int = { n * 2 }
      val m = map(s2, p)
      assert(contains(m, 4), "Map 1")
    }
  }


}
