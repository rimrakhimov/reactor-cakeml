structure Test =
  struct
    
    (***************************************************************************)

    (**
    * the type of function which perform a test case
    *)
    type testFunction = unit -> unit

    (**
     * the type representing a test or aggregation of tests.
     *)
    datatype test =
            (**
              * a test case
              * @params test
              * @param test the function which performs the test
              *)
            TestCase testFunction
            (**
              * a test case with label
              * @params label, test
              * @param label the name of the test
              * @param test the function which performs the test
              *)
          | TestLabel string test
            (**
              * aggregation of tests
              * @params tests
              * @param tests a list of tests
              *)
          | TestList (test list)

    (***************************************************************************)

    (**
    *  labels tests and aggregates them into a test.
    *
    * @params nameAndTester
    * @param nameAndTester list of pair of name and tester of a test
    * @return a test which aggregates the tests
    *)
    fun labelTests labelTestPairList =
        TestList
        (List.map
        (fn (label, function) => (TestLabel label (TestCase function)))
        labelTestPairList)

    (***************************************************************************)

  end ;