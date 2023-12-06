//> using test.dep org.scalameta::munit::0.7.29

class SolverSuite extends munit.FunSuite {

    test("test suite runs") {
        assertEquals(true, true)
    }

    test("ranges can touch") {
        import util.*
        assert(Range.Long(1, 3, 1).clips(Range.Long(2, 5, 1)))
    }

}