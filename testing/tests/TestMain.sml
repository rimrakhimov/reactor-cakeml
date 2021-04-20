structure TestMain =
struct

  fun test () =
      ( 
        print "\tTest Assert structure: \n";
        TestAssert.runTest ();
        print "\tTest Test structure: \n";
        TestTest.runTest ()
      )

end ;