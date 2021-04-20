structure TestDictionary =
struct
    fun testCreate0001 () =
        (Dictionary.create (); ())

    fun testExists0001 () =
      let 
        val emptyDictionary = Dictionary.create ()
      in 
        Assert.assertFalse (Dictionary.exists emptyDictionary 1); () 
      end

    fun testExists0002 () =
      let
        val key = "K1"
        val dictionary = Dictionary.update (Dictionary.create ()) key 1
      in
        Assert.assertTrue (Dictionary.exists dictionary key); ()
      end

    fun testLookup0001 () =
      let 
        val emptyDictionary = Dictionary.create ()
      in
        (Dictionary.lookup emptyDictionary 1; Assert.fail "must fail")
        handle Dictionary.NotFound => ()
      end

    fun testIsEmpty0001 () =
      let
        val emptyDictionary = Dictionary.create ()
      in
        Assert.assertTrue (Dictionary.isEmpty emptyDictionary); ()
      end

    (******************************************)

    fun suite () =
        Test.labelTests
        [
            ("create0001", testCreate0001),
            ("exists0001", testExists0001),
            ("exists0002", testExists0002),
            ("lookup0001", testLookup0001),
            ("isEmpty0001", testIsEmpty0001)
        ]

end;

TextUITestRunner.runTest
    (TextUITestRunner.Output TextIO.stdOut)
    "TestDictionary"
    (TestDictionary.suite ());