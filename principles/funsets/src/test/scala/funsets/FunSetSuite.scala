package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains the elements in both of 2 sets") {
    new TestSets:
      val s12 = union(s1, s2)
      val s = intersect(s12, s1)
      assert(contains(s, 1), "1 should be in the intersect of the set (1, 2) and the set (1).")
      assert(!contains(s, 2), "2 should not be in the intersect of the set (1, 2) and the set (1).")
  }

  test("diff contains all elements of the first set that are not in second set") {
    new TestSets:
      val s12 = union(s1, s2)
      val s = diff(s12, s1)
      assert(!contains(s, 1), "1 should not be in the diff of the set (1, 2) and the set (1).")
      assert(contains(s, 2), "2 should be in the diff of the set (1, 2) and the set (1).")
  }

  test("filter the elements in a set") {
    new TestSets:
      val s12 = union(s1, s2)
      val s = filter(s12, x => x == 1)
      assert(contains(s, 1), "1 should be in the set (1, 2) after the filtering by 1.")
      assert(!contains(s, 2), "1 should not be in the set (1, 2) after the filtering by 1.")
  }

  test("forall query") {
    new TestSets:
      val s13 = union(s1, s3)
      assert(forall(s13, x => x > 0), "All the elements in set (1, 3) should be greater than 0.")
      assert(!forall(s13, x => x > 2), "Not all the elements in set (1, 3) are greater than 2.")
      assert(!forall(s13, x => x > 3), "Not all the elements in set (1, 3) are greater than 3.")
  }

  test("exists query") {
    new TestSets:
      val s13 = union(s1, s3)
      assert(exists(s13, x => x > 0), "There should have an element in the set (1, 3) greater than 0.")
      assert(exists(s13, x => x > 2), "There should have an element in the set (1, 3) greater than 2.")
      assert(!forall(s13, x => x > 3), "There should not have an element in the set (1, 3) greater than 3.")
  }

  test("map function") {
    new TestSets:
      val s13 = union(s1, s3)
      val s = map(s13, x => x * 2)
      assert(contains(s, 2) && contains(s, 6), "Should map the set (1, 3) to (2, 6).")
  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
