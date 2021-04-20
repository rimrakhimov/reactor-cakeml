structure TestTest =
struct

  (***************************************************************************)

  exception TestFail

  (***************************************************************************)

  (**
   * custom assertion for the Test.test type
   *)
  fun assertEqualTest t1 t2 = 
    case (t1, t2) of 
      ((Test.TestLabel label1 test1), (Test.TestLabel label2 test2)) =>
        (Assert.assertEqual2Tuple
          (Assert.assertEqualString, assertEqualTest)
          (label1, test1)
          (label2, test2))
    | ((Test.TestList list1), (Test.TestList list2)) =>
          Assert.assertEqualList assertEqualTest list1 list2
    | ((Test.TestCase _), (Test.TestCase _)) =>
          (*
            We cannot define equality relation on functions.
            Any assertion on functions will success.
          *)
          ()
    | _ => raise TestFail

  (******************************************)

  (**
   * Test case for labelTests: normal case
   *)
  fun testLabelTests0001 () =
      let
        val arg = []
        val expected = Test.TestList []
      in
        assertEqualTest expected (Test.labelTests arg)
      end

  fun testLabelTests0002 () =
      let
        fun test0002 () = ()
        val arg = [ ("test0002", test0002) ]
        val expected =
            Test.TestList
            [ Test.TestLabel "test0002" (Test.TestCase test0002) ]
      in
        assertEqualTest expected (Test.labelTests arg)
      end

  fun testLabelTests0003 () =
      let
        fun test0003_1 () = ()
        fun test0003_2 () = ()
        val arg = [ ("test0003_1", test0003_1), ("test0003_2", test0003_2) ]
        val expected =
            Test.TestList
            [
              Test.TestLabel "test0003_1" (Test.TestCase test0003_1),
              Test.TestLabel "test0003_2" (Test.TestCase test0003_2)
            ]
      in
        assertEqualTest expected (Test.labelTests arg)
      end

  fun testLabelTests0004 () =
      let
        fun test0004 () = ()
        (* Duplicate entry *)
        val arg = [ ("test0004", test0004), ("test0004", test0004) ]
        val expected =
            Test.TestList
            [
              Test.TestLabel "test0004" (Test.TestCase test0004),
              Test.TestLabel "test0004" (Test.TestCase test0004)
            ]
      in
        assertEqualTest expected (Test.labelTests arg)
      end

  (******************************************)


  (**
   * perform tests
   *
   *  We cannot use the SMLUnit.Test to group test cases because we are testing
   * that structure.
   *)
  fun runTest () =
      let
        val tests =
            [
              ("testLabelTests0001", testLabelTests0001),
              ("testLabelTests0002", testLabelTests0002),
              ("testLabelTests0003", testLabelTests0003),
              ("testLabelTests0004", testLabelTests0004)
            ]
        val failCases =
            List.foldl
            (fn failCases => fn (testName, testCase) =>
                ((testCase (); print "."; failCases)
                 handle TestFail => 
                        (print "F"; (testName ^ " by Fail") :: failCases)
                      | exn => 
                        (print "E"; (testName ^ " by " ^ Exception.exn_message exn) :: failCases)))
            []
            tests
      in
        print "\n";
        List.app (fn testName => (print testName; print "\n")) (List.rev failCases);
        print "\n";
        (if 
          List.length failCases = 0
         then 
          print "    SUCCESS\n\n\n"
         else
          print "    FAILURE\n\n\n"
        )
      end

end ;