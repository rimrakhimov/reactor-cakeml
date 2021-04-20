structure TestAssert =
struct

  (***************************************************************************)
  
  exception TestFail
  
  (***************************************************************************)

  local
    fun testSuccess expecteds actual =
        Assert.assertEqualAlternatives Assert.assertEqualInt expecteds actual
    fun testFailure expecteds actual =
        (Assert.assertEqualAlternatives Assert.assertEqualInt expecteds actual;
        raise TestFail)
        handle Assert.Fail _ => ()
  in
    fun testAssertEqualAlternatives0000 () = testFailure [] 1
    fun testAssertEqualAlternatives0010 () = testFailure [2] 1
    fun testAssertEqualAlternatives0011 () = testSuccess [1] 1
    fun testAssertEqualAlternatives0020 () = testFailure [2, 3] 1
    fun testAssertEqualAlternatives0021 () = testSuccess [1, 3] 1
    fun testAssertEqualAlternatives0022 () = testSuccess [2, 1] 1
    fun testAssertEqualAlternatives0030 () = testFailure [2, 3, 4] 1
    fun testAssertEqualAlternatives0031 () = testSuccess [1, 3, 4] 1
    fun testAssertEqualAlternatives0032 () = testSuccess [2, 1, 4] 1
    fun testAssertEqualAlternatives0034 () = testSuccess [2, 3, 1] 1
  end (* local *)

  fun testFail0001 () =
      let
        val message = "message"
      in
        (Assert.fail message;
         raise TestFail)
        handle Assert.Fail (Assert.GeneralFailure failMessage) =>
               if failMessage = message then () else raise TestFail
      end

  (****************************************)

  fun testAssertEqualUnit0001 () =
      let
        val value = ()
      in
        (if Assert.assertEqualUnit value value = value
         then ()
         else raise TestFail)
        handle Assert.Fail message => raise TestFail
      end

  (****************************************)

  fun testAssertEqualInt0001 () =
      let
        val value = 100
      in
        (Assert.assertEqualInt value value)
        handle Assert.Fail message => raise TestFail
      end

  fun testAssertEqualInt0002 () =
      let
        val value1 = 100
        val value2 = 200
      in
        (Assert.assertEqualInt value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
      end

  (****************************************)

  fun testAssertEqualChar0001 () =
      let
        val value = #"a"
      in
        (Assert.assertEqualChar value value)
        handle Assert.Fail message => raise TestFail
      end

  fun testAssertEqualChar0002 () =
      let
        val value1 = #"b"
        val value2 = #"c"
      in
        (Assert.assertEqualChar value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
      end

  (****************************************)

  fun testAssertEqualString0001 () =
      let
        val value = "a"
      in
        (Assert.assertEqualString value value)
        handle Assert.Fail message => raise TestFail
      end

  fun testAssertEqualString0002 () =
      let
        val value = ""
      in
        (Assert.assertEqualString value value)
        handle Assert.Fail message => raise TestFail
      end

  fun testAssertEqualString0003 () =
      let
        val value = "abcdefghijklmnopqrstuvwxyz "
      in
        (Assert.assertEqualString value value)
        handle Assert.Fail message => raise TestFail
      end

  fun testAssertEqualString0004 () =
      let
        val value1 = "b"
        val value2 = "c"
      in
        (Assert.assertEqualString value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
      end

  fun testAssertEqualString0005 () =
      let
        val value1 = ""
        val value2 = "c"
      in
        (Assert.assertEqualString value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
      end

  fun testAssertEqualString0006 () =
      let
        val value1 = "c"
        val value2 = ""
      in
        (Assert.assertEqualString value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
      end

  (****************************************)

  fun testAssertEqualRef0001 () =
      let
        val value = Ref 1
      in
        Assert.assertEqualRef Assert.assertEqualInt value value
      end

  fun testAssertEqualRef0002 () =
      let
        (*
         * Although value1 and value2 point different locations,
         * the assertion succeeds because these locations hold the equal value.
         *)
        val value1 = Ref 1
        val value2 = Ref 1
      in
        Assert.assertEqualRef Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualRef0003 () =
      let
        val value1 = Ref 1
        val value2 = Ref 2
      in
        (
          Assert.assertEqualRef Assert.assertEqualInt value1 value2;
          raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertSameRef0001 () =
      let
        val value = Ref 1
      in
        Assert.assertSameRef value value
      end

  fun testAssertSameRef0002 () =
      let
        (*
         * The assertion fails because value1 and value2 point different
         * locations, although these locations hold the equal value.
         *)
        val value1 = Ref 1
        val value2 = Ref 1
      in
        (Assert.assertSameRef value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertSameRef0003 () =
      let
        val value1 = Ref 1
        val value2 = Ref 2
      in
        (Assert.assertSameRef value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualBool0001 () =
      let
        val value = True
      in
        Assert.assertEqualBool value value
      end

  fun testAssertEqualBool0002 () =
      let
        val value = False
      in
        Assert.assertEqualBool value value
      end

  fun testAssertEqualBool0003 () =
      let
        val value1 = True
        val value2 = False
      in
        (Assert.assertEqualBool value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualBool0004 () =
      let
        val value1 = False
        val value2 = True
      in
        (Assert.assertEqualBool value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertTrue0001 () =
      let
        val value = True
      in
        Assert.assertTrue value
      end

  fun testAssertTrue0002 () =
      let
        val value = False
      in
        (Assert.assertTrue value; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertFalse0001 () =
      let
        val value = False
      in
        Assert.assertFalse value
      end

  fun testAssertFalse0002 () =
      let
        val value = True
      in
        (Assert.assertFalse value; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualOption0001 () =
      let
        val value = Some 1
      in
        Assert.assertEqualOption Assert.assertEqualInt value value
      end

  fun testAssertEqualOption0002 () =
      let
        val value = None
      in
        Assert.assertEqualOption Assert.assertEqualInt value value
      end

  fun testAssertEqualOption0003 () =
      let
        val value1 = Some 1
        val value2 = None
      in
        (
          Assert.assertEqualOption Assert.assertEqualInt value1 value2;
          raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOption0004 () =
      let
          val value1 = None
          val value2 = Some 1
      in
        (
          Assert.assertEqualOption Assert.assertEqualInt value1 value2;
          raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOption0005 () =
      let
          val value1 = Some 1
          val value2 = Some 2
      in
        (
          Assert.assertEqualOption Assert.assertEqualInt value1 value2;
          raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualIntOption0001 () =
    let
      val value = Some 1
    in
      Assert.assertEqualIntOption value value
    end

  fun testAssertEqualIntOption0002 () =
    let
      val value = None
    in
      Assert.assertEqualIntOption value value
    end

  fun testAssertEqualIntOption0003 () =
    let
        val value1 = None
        val value2 = Some 1
    in
        (
            Assert.assertEqualIntOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end
    
  fun testAssertEqualIntOption0004 () =
    let
        val value1 = Some 1
        val value2 = None
    in
        (
            Assert.assertEqualIntOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  fun testAssertEqualIntOption0005 () =
    let
        val value1 = Some 1
        val value2 = Some 2
    in
        (
            Assert.assertEqualIntOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  (****************************************)

  fun testAssertEqualCharOption0001 () =
    let
      val value = Some #"a"
    in
      Assert.assertEqualCharOption value value
    end

  fun testAssertEqualCharOption0002 () =
    let
      val value = None
    in
      Assert.assertEqualCharOption value value
    end

  fun testAssertEqualCharOption0003 () =
    let
        val value1 = None
        val value2 = Some #"a"
    in
        (
            Assert.assertEqualCharOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end
    
  fun testAssertEqualCharOption0004 () =
    let
        val value1 = Some #"a"
        val value2 = None
    in
        (
            Assert.assertEqualCharOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  fun testAssertEqualCharOption0005 () =
    let
        val value1 = Some #"a"
        val value2 = Some #"b"
    in
        (
            Assert.assertEqualCharOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  (****************************************)

  fun testAssertEqualStringOption0001 () =
    let
      val value = Some "abcde"
    in
      Assert.assertEqualStringOption value value
    end

  fun testAssertEqualStringOption0002 () =
    let
      val value = Some ""
    in
      Assert.assertEqualStringOption value value
    end

  fun testAssertEqualStringOption0003 () =
    let
      val value = None
    in
      Assert.assertEqualStringOption value value
    end

  fun testAssertEqualStringOption0004 () =
    let
        val value1 = None
        val value2 = Some "abcde"
    in
        (
            Assert.assertEqualStringOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end
    
  fun testAssertEqualStringOption0005 () =
    let
        val value1 = Some "abcde"
        val value2 = None
    in
        (
            Assert.assertEqualStringOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  fun testAssertEqualStringOption0006 () =
    let
        val value1 = Some "abcde"
        val value2 = Some "fghij"
    in
        (
            Assert.assertEqualStringOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  fun testAssertEqualStringOption0007 () =
    let
        val value1 = Some ""
        val value2 = Some "fghij"
    in
        (
            Assert.assertEqualStringOption value1 value2;
            raise TestFail
        )
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
            | _ => raise TestFail
    end

  (****************************************)

  fun testAssertSome0001 () =
      let
        val value = Some 1
      in
        Assert.assertSome value
      end

  fun testAssertSome0002 () =
      let
        val value = None
      in
        (Assert.assertSome value; raise TestFail)
        handle Assert.Fail _ => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertNone0001 () =
      let
        val value = None
      in
        Assert.assertNone value
      end

  fun testAssertNone0002 () =
      let
        val value = Some 1
      in
        (Assert.assertNone value; raise TestFail)
        handle Assert.Fail _ => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualOrderLL () =
      let
        val value1 = Less
        val value2 = Less
      in
        Assert.assertEqualOrder value1 value2
      end

  fun testAssertEqualOrderLE () =
      let
        val value1 = Less
        val value2 = Equal
      in
        (Assert.assertEqualOrder value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOrderLG () =
      let
        val value1 = Less
        val value2 = Greater
      in
        (Assert.assertEqualOrder value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOrderEL () =
      let
        val value1 = Equal
        val value2 = Less
      in
        (Assert.assertEqualOrder value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOrderEE () =
      let
        val value1 = Equal
        val value2 = Equal
      in
        Assert.assertEqualOrder value1 value2
      end

  fun testAssertEqualOrderEG () =
      let
        val value1 = Equal
        val value2 = Greater
      in
        (Assert.assertEqualOrder value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOrderGL () =
      let
        val value1 = Greater
        val value2 = Less
      in
        (Assert.assertEqualOrder value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOrderGE () =
      let
        val value1 = Greater
        val value2 = Equal
      in
        (Assert.assertEqualOrder value1 value2; raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqualOrderGG () =
      let
        val value1 = Greater
        val value2 = Greater
      in
        Assert.assertEqualOrder value1 value2
      end

  (****************************************)

  fun testAssertEqual2Tuple0001 () =
      let
        val value1 = (1, True)
        val value2 = (1, True)
      in
          Assert.assertEqual2Tuple
          (Assert.assertEqualInt, Assert.assertEqualBool)
          value1
          value2
      end

  fun testAssertEqual2Tuple0002 () =
      let
        val value1 = (1, True)
        val value2 = (2, True)
      in
        (Assert.assertEqual2Tuple
         (Assert.assertEqualInt, Assert.assertEqualBool)
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqual2Tuple0003 () =
      let
        val value1 = (1, True)
        val value2 = (1, False)
      in
        (Assert.assertEqual2Tuple
         (Assert.assertEqualInt, Assert.assertEqualBool)
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqual3Tuple0001 () =
      let
        val value1 = (1, True, "foo")
        val value2 = (1, True, "foo")
      in
          Assert.assertEqual3Tuple
          (
            Assert.assertEqualInt,
            Assert.assertEqualBool,
            Assert.assertEqualString
          )
          value1
          value2
      end

  fun testAssertEqual3Tuple0002 () =
      let
        val value1 = (1, True, "foo")
        val value2 = (2, True, "foo")
      in
        (Assert.assertEqual3Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqual3Tuple0003 () =
      let
        val value1 = (1, True, "foo")
        val value2 = (1, False, "foo")
      in
        (Assert.assertEqual3Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqual3Tuple0004 () =
      let
        val value1 = (1, True, "foo")
        val value2 = (1, True, "bar")
      in
        (Assert.assertEqual3Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqual4Tuple0001 () =
      let
        val value1 = (1, True, "foo", #"a")
        val value2 = (1, True, "foo", #"a")
      in
          Assert.assertEqual4Tuple
          (
            Assert.assertEqualInt,
            Assert.assertEqualBool,
            Assert.assertEqualString,
            Assert.assertEqualChar
          )
          value1
          value2
      end

  fun testAssertEqual4Tuple0002 () =
      let
        val value1 = (1, True, "foo", #"a")
        val value2 = (2, True, "foo", #"a")
      in
        (Assert.assertEqual4Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString,
           Assert.assertEqualChar
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqual4Tuple0003 () =
      let
        val value1 = (1, True, "foo", #"a")
        val value2 = (1, False, "foo", #"a")
      in
        (Assert.assertEqual4Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString,
           Assert.assertEqualChar
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqual4Tuple0004 () =
      let
        val value1 = (1, True, "foo", #"a")
        val value2 = (1, True, "bar", #"a")
      in
        (Assert.assertEqual4Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString,
           Assert.assertEqualChar
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  fun testAssertEqual4Tuple0005 () =
      let
        val value1 = (1, True, "foo", #"a")
        val value2 = (1, True, "foo", #"b")
      in
        (Assert.assertEqual4Tuple
         (
           Assert.assertEqualInt,
           Assert.assertEqualBool,
           Assert.assertEqualString,
           Assert.assertEqualChar
         )
         value1
         value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => ()
             | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualVector0001 () =
      let
        val list1 = []
        val value1 = Vector.fromList list1
        val value2 = Vector.fromList list1
      in
        Assert.assertEqualVector Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualVector0002 () =
      let
        val list1 = [1]
        val value1 = Vector.fromList list1
        val value2 = Vector.fromList list1
      in
        Assert.assertEqualVector Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualVector0003 () =
      let
        val list1 = [1, 2]
        val value1 = Vector.fromList list1
        val value2 = Vector.fromList list1
      in
        Assert.assertEqualVector Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualVector0004 () =
      let
        val list1 = [1, 2, 3]
        val value1 = Vector.fromList list1
        val value2 = Vector.fromList list1
      in
        Assert.assertEqualVector Assert.assertEqualInt value1 value2
      end

  (*
   0 - 1
  *)
  fun testAssertEqualVector1000 () =
      let
        val value1 = Vector.fromList []
        val value2 = Vector.fromList [1]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 0
  *)
  fun testAssertEqualVector1001 () =
      let
        val value1 = Vector.fromList [1]
        val value2 = Vector.fromList []
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 1
  *)
  fun testAssertEqualVector1002 () =
      let
        val value1 = Vector.fromList [2]
        val value2 = Vector.fromList [3]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 2
  *)
  fun testAssertEqualVector1003 () =
      let
        val value1 = Vector.fromList [2]
        val value2 = Vector.fromList [2, 3]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 0
  *)
  fun testAssertEqualVector1004 () =
      let
        val value1 = Vector.fromList [1, 2]
        val value2 = Vector.fromList []
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 1
  *)
  fun testAssertEqualVector1005 () =
      let
        val value1 = Vector.fromList [1, 2]
        val value2 = Vector.fromList [1]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 2
  *)
  fun testAssertEqualVector1006 () =
      let
        val value1 = Vector.fromList [1, 2]
        val value2 = Vector.fromList [1, 3]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 3
  *)
  fun testAssertEqualVector1007 () =
      let
        val value1 = Vector.fromList [1, 2]
        val value2 = Vector.fromList [1, 2, 3]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 0
  *)
  fun testAssertEqualVector1008 () =
      let
        val value1 = Vector.fromList [1, 2, 3]
        val value2 = Vector.fromList []
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 1
  *)
  fun testAssertEqualVector1009 () =
      let
        val value1 = Vector.fromList [1, 2, 3]
        val value2 = Vector.fromList [1]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 2
  *)
  fun testAssertEqualVector1010 () =
      let
        val value1 = Vector.fromList [1, 2, 3]
        val value2 = Vector.fromList [1, 2]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 3
  *)
  fun testAssertEqualVector1011 () =
      let
        val value1 = Vector.fromList [1, 2, 3]
        val value2 = Vector.fromList [1, 2, 4]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 4
  *)
  fun testAssertEqualVector1012 () =
      let
        val value1 = Vector.fromList [1, 2, 3]
        val value2 = Vector.fromList [1, 2, 3, 4]
      in
        (Assert.assertEqualVector Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualArray0001 () =
      let
        val list1 = []
        val value1 = Array.fromList list1
        val value2 = Array.fromList list1
      in
        Assert.assertEqualArray Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualArray0002 () =
      let
        val list1 = [1]
        val value1 = Array.fromList list1
        val value2 = Array.fromList list1
      in
        Assert.assertEqualArray Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualArray0003 () =
      let
        val list1 = [1, 2]
        val value1 = Array.fromList list1
        val value2 = Array.fromList list1
      in
        Assert.assertEqualArray Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualArray0004 () =
      let
        val list1 = [1, 2, 3]
        val value1 = Array.fromList list1
        val value2 = Array.fromList list1
      in
        Assert.assertEqualArray Assert.assertEqualInt value1 value2
      end

  (*
   0 - 1
  *)
  fun testAssertEqualArray1000 () =
      let
        val value1 = Array.fromList []
        val value2 = Array.fromList [1]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 0
  *)
  fun testAssertEqualArray1001 () =
      let
        val value1 = Array.fromList [1]
        val value2 = Array.fromList []
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 1
  *)
  fun testAssertEqualArray1002 () =
      let
        val value1 = Array.fromList [2]
        val value2 = Array.fromList [3]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 2
  *)
  fun testAssertEqualArray1003 () =
      let
        val value1 = Array.fromList [2]
        val value2 = Array.fromList [2, 3]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 0
  *)
  fun testAssertEqualArray1004 () =
      let
        val value1 = Array.fromList [1, 2]
        val value2 = Array.fromList []
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 1
  *)
  fun testAssertEqualArray1005 () =
      let
        val value1 = Array.fromList [1, 2]
        val value2 = Array.fromList [1]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 2
  *)
  fun testAssertEqualArray1006 () =
      let
        val value1 = Array.fromList [1, 2]
        val value2 = Array.fromList [1, 3]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 3
  *)
  fun testAssertEqualArray1007 () =
      let
        val value1 = Array.fromList [1, 2]
        val value2 = Array.fromList [1, 2, 3]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 0
  *)
  fun testAssertEqualArray1008 () =
      let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = Array.fromList []
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 1
  *)
  fun testAssertEqualArray1009 () =
      let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = Array.fromList [1]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 2
  *)
  fun testAssertEqualArray1010 () =
      let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = Array.fromList [1, 2]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 3
  *)
  fun testAssertEqualArray1011 () =
      let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = Array.fromList [1, 2, 4]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 4
  *)
  fun testAssertEqualArray1012 () =
      let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = Array.fromList [1, 2, 3, 4]
      in
        (Assert.assertEqualArray Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (****************************************)

  fun testAssertSameArray0001 () =
    let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = value1
    in
        Assert.assertSameArray value1 value2
    end

  fun testAssertSameArray0002 () =
    let
        val value1 = Array.fromList [1, 2, 3]
        val value2 = Array.fromList [1, 2, 3]
    in
        (Assert.assertSameArray value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
    end

  (****************************************)

  fun testAssertSameWord8Array0001 () =
    let
        val w = Word8.fromInt 1
        val value1 = Word8Array.array 5 w
        val value2 = value1
    in
        Assert.assertSameArray value1 value2
    end

  fun testAssertSameWord8Array0002 () =
    let
        val w = Word8.fromInt 1
        val value1 = Word8Array.array 5 w
        val value2 = Word8Array.array 5 w
    in
        (Assert.assertSameArray value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
    end

  (****************************************)

  fun testAssertEqualList0001 () =
      let
        val list1 = []
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualList Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualList0002 () =
      let
        val list1 = [1]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualList Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualList0003 () =
      let
        val list1 = [1, 2]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualList Assert.assertEqualInt value1 value2
      end

  fun testAssertEqualList0004 () =
      let
        val list1 = [1, 2, 3]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualList Assert.assertEqualInt value1 value2
      end

  (*
   0 - 1
  *)
  fun testAssertEqualList1000 () =
      let
        val value1 = []
        val value2 = [1]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 0
  *)
  fun testAssertEqualList1001 () =
      let
        val value1 = [1]
        val value2 = []
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 1
  *)
  fun testAssertEqualList1002 () =
      let
        val value1 = [2]
        val value2 = [3]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 2
  *)
  fun testAssertEqualList1003 () =
      let
        val value1 = [2]
        val value2 = [2, 3]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 0
  *)
  fun testAssertEqualList1004 () =
      let
        val value1 = [1, 2]
        val value2 = []
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 1
  *)
  fun testAssertEqualList1005 () =
      let
        val value1 = [1, 2]
        val value2 = [1]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 2
  *)
  fun testAssertEqualList1006 () =
      let
        val value1 = [1, 2]
        val value2 = [1, 3]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 3
  *)
  fun testAssertEqualList1007 () =
      let
        val value1 = [1, 2]
        val value2 = [1, 2, 3]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 0
  *)
  fun testAssertEqualList1008 () =
      let
        val value1 = [1, 2, 3]
        val value2 = []
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 1
  *)
  fun testAssertEqualList1009 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 2
  *)
  fun testAssertEqualList1010 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1, 2]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 3
  *)
  fun testAssertEqualList1011 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1, 2, 4]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 4
  *)
  fun testAssertEqualList1012 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1, 2, 3, 4]
      in
        (Assert.assertEqualList Assert.assertEqualInt value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualIntList0001 () =
      let
        val list1 = []
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualIntList value1 value2
      end

  fun testAssertEqualIntList0002 () =
      let
        val list1 = [1]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualIntList value1 value2
      end

  fun testAssertEqualIntList0003 () =
      let
        val list1 = [1, 2]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualIntList value1 value2
      end

  fun testAssertEqualIntList0004 () =
      let
        val list1 = [1, 2, 3]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualIntList value1 value2
      end

  (*
   0 - 1
  *)
  fun testAssertEqualIntList1000 () =
      let
        val value1 = []
        val value2 = [1]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 0
  *)
  fun testAssertEqualIntList1001 () =
      let
        val value1 = [1]
        val value2 = []
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 1
  *)
  fun testAssertEqualIntList1002 () =
      let
        val value1 = [2]
        val value2 = [3]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 2
  *)
  fun testAssertEqualIntList1003 () =
      let
        val value1 = [2]
        val value2 = [2, 3]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 0
  *)
  fun testAssertEqualIntList1004 () =
      let
        val value1 = [1, 2]
        val value2 = []
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 1
  *)
  fun testAssertEqualIntList1005 () =
      let
        val value1 = [1, 2]
        val value2 = [1]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 2
  *)
  fun testAssertEqualIntList1006 () =
      let
        val value1 = [1, 2]
        val value2 = [1, 3]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 3
  *)
  fun testAssertEqualIntList1007 () =
      let
        val value1 = [1, 2]
        val value2 = [1, 2, 3]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 0
  *)
  fun testAssertEqualIntList1008 () =
      let
        val value1 = [1, 2, 3]
        val value2 = []
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 1
  *)
  fun testAssertEqualIntList1009 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 2
  *)
  fun testAssertEqualIntList1010 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1, 2]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 3
  *)
  fun testAssertEqualIntList1011 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1, 2, 4]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 4
  *)
  fun testAssertEqualIntList1012 () =
      let
        val value1 = [1, 2, 3]
        val value2 = [1, 2, 3, 4]
      in
        (Assert.assertEqualIntList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualCharList0001 () =
      let
        val list1 = []
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualCharList value1 value2
      end

  fun testAssertEqualCharList0002 () =
      let
        val list1 = [#"a"]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualCharList value1 value2
      end

  fun testAssertEqualCharList0003 () =
      let
        val list1 = [#"a", #"b"]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualCharList value1 value2
      end

  fun testAssertEqualCharList0004 () =
      let
        val list1 = [#"a", #"b", #"c"]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualCharList value1 value2
      end

  (*
   0 - 1
  *)
  fun testAssertEqualCharList1000 () =
      let
        val value1 = []
        val value2 = [#"a"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 0
  *)
  fun testAssertEqualCharList1001 () =
      let
        val value1 = [#"a"]
        val value2 = []
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 1
  *)
  fun testAssertEqualCharList1002 () =
      let
        val value1 = [#"b"]
        val value2 = [#"c"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 2
  *)
  fun testAssertEqualCharList1003 () =
      let
        val value1 = [#"b"]
        val value2 = [#"b", #"c"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 0
  *)
  fun testAssertEqualCharList1004 () =
      let
        val value1 = [#"b", #"c"]
        val value2 = []
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 1
  *)
  fun testAssertEqualCharList1005 () =
      let
        val value1 = [#"a", #"b"]
        val value2 = [#"a"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 2
  *)
  fun testAssertEqualCharList1006 () =
      let
        val value1 = [#"a", #"b"]
        val value2 = [#"a", #"c"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 3
  *)
  fun testAssertEqualCharList1007 () =
      let
        val value1 = [#"a", #"b"]
        val value2 = [#"a", #"b", #"c"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 0
  *)
  fun testAssertEqualCharList1008 () =
      let
        val value1 = [#"a", #"b", #"c"]
        val value2 = []
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 1
  *)
  fun testAssertEqualCharList1009 () =
      let
        val value1 = [#"a", #"b", #"c"]
        val value2 = [#"a"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 2
  *)
  fun testAssertEqualCharList1010 () =
      let
        val value1 = [#"a", #"b", #"c"]
        val value2 = [#"a", #"b"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 3
  *)
  fun testAssertEqualCharList1011 () =
      let
        val value1 = [#"a", #"b", #"c"]
        val value2 = [#"a", #"b", #"d"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 4
  *)
  fun testAssertEqualCharList1012 () =
      let
        val value1 = [#"a", #"b", #"c"]
        val value2 = [#"a", #"b", #"c", #"d"]
      in
        (Assert.assertEqualCharList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (****************************************)

  fun testAssertEqualStringList0001 () =
      let
        val list1 = []
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualStringList value1 value2
      end

  fun testAssertEqualStringList0002 () =
      let
        val list1 = ["abc"]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualStringList value1 value2
      end

  fun testAssertEqualStringList0003 () =
      let
        val list1 = ["abc", "def"]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualStringList value1 value2
      end

  fun testAssertEqualStringList0004 () =
      let
        val list1 = ["abc", "def", "ghi", "jkl"]
        val value1 = list1
        val value2 = list1
      in
        Assert.assertEqualStringList value1 value2
      end

  (*
   0 - 1
  *)
  fun testAssertEqualStringList1000 () =
      let
        val value1 = []
        val value2 = ["abc"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 0
  *)
  fun testAssertEqualStringList1001 () =
      let
        val value1 = ["a"]
        val value2 = []
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 1
  *)
  fun testAssertEqualStringList1002 () =
      let
        val value1 = ["b"]
        val value2 = ["c"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   1 - 2
  *)
  fun testAssertEqualStringList1003 () =
      let
        val value1 = ["b"]
        val value2 = ["b", "c"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 0
  *)
  fun testAssertEqualStringList1004 () =
      let
        val value1 = ["a", "b"]
        val value2 = []
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 1
  *)
  fun testAssertEqualStringList1005 () =
      let
        val value1 = ["a", "b"]
        val value2 = ["a"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 2
  *)
  fun testAssertEqualStringList1006 () =
      let
        val value1 = ["a", "b"]
        val value2 = ["a", "c"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   2 - 3
  *)
  fun testAssertEqualStringList1007 () =
      let
        val value1 = ["a", "b"]
        val value2 = ["a", "b", "c"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 0
  *)
  fun testAssertEqualStringList1008 () =
      let
        val value1 = ["a", "b", "c"]
        val value2 = []
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 1
  *)
  fun testAssertEqualStringList1009 () =
      let
        val value1 = ["a", "b", "c"]
        val value2 = ["a"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 2
  *)
  fun testAssertEqualStringList1010 () =
      let
        val value1 = ["a", "b", "c"]
        val value2 = ["a", "b"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 3
  *)
  fun testAssertEqualStringList1011 () =
      let
        val value1 = ["a", "b", "c"]
        val value2 = ["a", "b", "d"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (*
   3 - 4
  *)
  fun testAssertEqualStringList1012 () =
      let
        val value1 = ["a", "b", "c"]
        val value2 = ["a", "b", "c", "d"]
      in
        (Assert.assertEqualStringList value1 value2;
         raise TestFail)
        handle Assert.Fail (Assert.NotEqualFailure _ _) => () | _ => raise TestFail
      end

  (***************************************************************************)

  (**
   * performs tests
   *)
  fun runTest () =
      let
        val tests =
            [
               ("testAssertEqualAlternatives0000", testAssertEqualAlternatives0000),
               ("testAssertEqualAlternatives0010", testAssertEqualAlternatives0010),
               ("testAssertEqualAlternatives0011", testAssertEqualAlternatives0011),
               ("testAssertEqualAlternatives0020", testAssertEqualAlternatives0020),
               ("testAssertEqualAlternatives0021", testAssertEqualAlternatives0021),
               ("testAssertEqualAlternatives0022", testAssertEqualAlternatives0022),
               ("testAssertEqualAlternatives0030", testAssertEqualAlternatives0030),
               ("testAssertEqualAlternatives0031", testAssertEqualAlternatives0031),
               ("testAssertEqualAlternatives0032", testAssertEqualAlternatives0032),
               ("testAssertEqualAlternatives0034", testAssertEqualAlternatives0034),

               ("testFail0001", testFail0001),

               ("testAssertEqualUnit0001", testAssertEqualUnit0001),

               ("testAssertEqualInt0001", testAssertEqualInt0001),
               ("testAssertEqualInt0002", testAssertEqualInt0002),

               ("testAssertEqualChar0001", testAssertEqualChar0001),
               ("testAssertEqualChar0002", testAssertEqualChar0002),

               ("testAssertEqualString0001", testAssertEqualString0001),
               ("testAssertEqualString0002", testAssertEqualString0002),
               ("testAssertEqualString0003", testAssertEqualString0003),
               ("testAssertEqualString0004", testAssertEqualString0004),
               ("testAssertEqualString0005", testAssertEqualString0005),
               ("testAssertEqualString0006", testAssertEqualString0006),
               
               ("testAssertEqualRef0001", testAssertEqualRef0001),
               ("testAssertEqualRef0002", testAssertEqualRef0002),
               ("testAssertEqualRef0003", testAssertEqualRef0003),

               ("testAssertSameRef0001", testAssertSameRef0001),
               ("testAssertSameRef0002", testAssertSameRef0002),
               ("testAssertSameRef0003", testAssertSameRef0003),

               ("testAssertEqualBool0001", testAssertEqualBool0001),
               ("testAssertEqualBool0002", testAssertEqualBool0002),
               ("testAssertEqualBool0003", testAssertEqualBool0003),
               ("testAssertEqualBool0004", testAssertEqualBool0004),
               
               ("testAssertTrue0001", testAssertTrue0001),
               ("testAssertTrue0002", testAssertTrue0002),

               ("testAssertFalse0001", testAssertFalse0001),
               ("testAssertFalse0002", testAssertFalse0002),

               ("testAssertEqualOption0001", testAssertEqualOption0001),
               ("testAssertEqualOption0002", testAssertEqualOption0002),
               ("testAssertEqualOption0003", testAssertEqualOption0003),
               ("testAssertEqualOption0004", testAssertEqualOption0004),
               ("testAssertEqualOption0005", testAssertEqualOption0005),

               ("testAssertEqualIntOption0001", testAssertEqualIntOption0001),
               ("testAssertEqualIntOption0002", testAssertEqualIntOption0002),
               ("testAssertEqualIntOption0003", testAssertEqualIntOption0003),
               ("testAssertEqualIntOption0004", testAssertEqualIntOption0004),
               ("testAssertEqualIntOption0005", testAssertEqualIntOption0005),

               ("testAssertEqualCharOption0001", testAssertEqualCharOption0001),
               ("testAssertEqualCharOption0002", testAssertEqualCharOption0002),
               ("testAssertEqualCharOption0003", testAssertEqualCharOption0003),
               ("testAssertEqualCharOption0004", testAssertEqualCharOption0004),
               ("testAssertEqualCharOption0005", testAssertEqualCharOption0005),

               ("testAssertEqualStringOption0001", testAssertEqualStringOption0001),
               ("testAssertEqualStringOption0002", testAssertEqualStringOption0002),
               ("testAssertEqualStringOption0003", testAssertEqualStringOption0003),
               ("testAssertEqualStringOption0004", testAssertEqualStringOption0004),
               ("testAssertEqualStringOption0005", testAssertEqualStringOption0005),
               ("testAssertEqualStringOption0006", testAssertEqualStringOption0006),
               ("testAssertEqualStringOption0007", testAssertEqualStringOption0007),

               ("testAssertSome0001", testAssertSome0001),
               ("testAssertSome0002", testAssertSome0002),
               ("testAssertNone0001", testAssertNone0001),
               ("testAssertNone0002", testAssertNone0002),

               ("testAssertEqualOrderLL", testAssertEqualOrderLL),
               ("testAssertEqualOrderLE", testAssertEqualOrderLE),
               ("testAssertEqualOrderLG", testAssertEqualOrderLG),
               ("testAssertEqualOrderEL", testAssertEqualOrderEL),
               ("testAssertEqualOrderEE", testAssertEqualOrderEE),
               ("testAssertEqualOrderEG", testAssertEqualOrderEG),
               ("testAssertEqualOrderGL", testAssertEqualOrderGL),
               ("testAssertEqualOrderGE", testAssertEqualOrderGE),
               ("testAssertEqualOrderGG", testAssertEqualOrderGG),

               ("testAssertEqual2Tuple0001", testAssertEqual2Tuple0001),
               ("testAssertEqual2Tuple0002", testAssertEqual2Tuple0002),
               ("testAssertEqual2Tuple0003", testAssertEqual2Tuple0003),
               ("testAssertEqual3Tuple0001", testAssertEqual3Tuple0001),
               ("testAssertEqual3Tuple0002", testAssertEqual3Tuple0002),
               ("testAssertEqual3Tuple0003", testAssertEqual3Tuple0003),
               ("testAssertEqual3Tuple0004", testAssertEqual3Tuple0004),
               ("testAssertEqual4Tuple0001", testAssertEqual4Tuple0001),
               ("testAssertEqual4Tuple0002", testAssertEqual4Tuple0002),
               ("testAssertEqual4Tuple0003", testAssertEqual4Tuple0003),
               ("testAssertEqual4Tuple0004", testAssertEqual4Tuple0004),
               ("testAssertEqual4Tuple0005", testAssertEqual4Tuple0005),

               ("testAssertEqualVector0001", testAssertEqualVector0001),
               ("testAssertEqualVector0002", testAssertEqualVector0002),
               ("testAssertEqualVector0003", testAssertEqualVector0003),
               ("testAssertEqualVector0004", testAssertEqualVector0004),
               ("testAssertEqualVector1000", testAssertEqualVector1000),
               ("testAssertEqualVector1001", testAssertEqualVector1001),
               ("testAssertEqualVector1002", testAssertEqualVector1002),
               ("testAssertEqualVector1003", testAssertEqualVector1003),
               ("testAssertEqualVector1004", testAssertEqualVector1004),
               ("testAssertEqualVector1005", testAssertEqualVector1005),
               ("testAssertEqualVector1006", testAssertEqualVector1006),
               ("testAssertEqualVector1007", testAssertEqualVector1007),
               ("testAssertEqualVector1008", testAssertEqualVector1008),
               ("testAssertEqualVector1009", testAssertEqualVector1009),
               ("testAssertEqualVector1010", testAssertEqualVector1010),
               ("testAssertEqualVector1011", testAssertEqualVector1011),
               ("testAssertEqualVector1012", testAssertEqualVector1012),

               ("testAssertEqualArray0001", testAssertEqualArray0001),
               ("testAssertEqualArray0002", testAssertEqualArray0002),
               ("testAssertEqualArray0003", testAssertEqualArray0003),
               ("testAssertEqualArray0004", testAssertEqualArray0004),
               ("testAssertEqualArray1000", testAssertEqualArray1000),
               ("testAssertEqualArray1001", testAssertEqualArray1001),
               ("testAssertEqualArray1002", testAssertEqualArray1002),
               ("testAssertEqualArray1003", testAssertEqualArray1003),
               ("testAssertEqualArray1004", testAssertEqualArray1004),
               ("testAssertEqualArray1005", testAssertEqualArray1005),
               ("testAssertEqualArray1006", testAssertEqualArray1006),
               ("testAssertEqualArray1007", testAssertEqualArray1007),
               ("testAssertEqualArray1008", testAssertEqualArray1008),
               ("testAssertEqualArray1009", testAssertEqualArray1009),
               ("testAssertEqualArray1010", testAssertEqualArray1010),
               ("testAssertEqualArray1011", testAssertEqualArray1011),
               ("testAssertEqualArray1012", testAssertEqualArray1012),

               ("testAssertSameArray0001", testAssertSameArray0001),
               ("testAssertSameArray0002", testAssertSameArray0002),

               ("testAssertSameWord8Array0001", testAssertSameWord8Array0001),
               ("testAssertSameWord8Array0002", testAssertSameWord8Array0002),

               ("testAssertEqualList0001", testAssertEqualList0001),
               ("testAssertEqualList0002", testAssertEqualList0002),
               ("testAssertEqualList0003", testAssertEqualList0003),
               ("testAssertEqualList0004", testAssertEqualList0004),
               ("testAssertEqualList1000", testAssertEqualList1000),
               ("testAssertEqualList1001", testAssertEqualList1001),
               ("testAssertEqualList1002", testAssertEqualList1002),
               ("testAssertEqualList1003", testAssertEqualList1003),
               ("testAssertEqualList1004", testAssertEqualList1004),
               ("testAssertEqualList1005", testAssertEqualList1005),
               ("testAssertEqualList1006", testAssertEqualList1006),
               ("testAssertEqualList1007", testAssertEqualList1007),
               ("testAssertEqualList1008", testAssertEqualList1008),
               ("testAssertEqualList1009", testAssertEqualList1009),
               ("testAssertEqualList1010", testAssertEqualList1010),
               ("testAssertEqualList1011", testAssertEqualList1011),
               ("testAssertEqualList1012", testAssertEqualList1012),

               ("testAssertEqualIntList0001", testAssertEqualIntList0001),
               ("testAssertEqualIntList0002", testAssertEqualIntList0002),
               ("testAssertEqualIntList0003", testAssertEqualIntList0003),
               ("testAssertEqualIntList0004", testAssertEqualIntList0004),
               ("testAssertEqualIntList1000", testAssertEqualIntList1000),
               ("testAssertEqualIntList1001", testAssertEqualIntList1001),
               ("testAssertEqualIntList1002", testAssertEqualIntList1002),
               ("testAssertEqualIntList1003", testAssertEqualIntList1003),
               ("testAssertEqualIntList1004", testAssertEqualIntList1004),
               ("testAssertEqualIntList1005", testAssertEqualIntList1005),
               ("testAssertEqualIntList1006", testAssertEqualIntList1006),
               ("testAssertEqualIntList1007", testAssertEqualIntList1007),
               ("testAssertEqualIntList1008", testAssertEqualIntList1008),
               ("testAssertEqualIntList1009", testAssertEqualIntList1009),
               ("testAssertEqualIntList1010", testAssertEqualIntList1010),
               ("testAssertEqualIntList1011", testAssertEqualIntList1011),
               ("testAssertEqualIntList1012", testAssertEqualIntList1012),

               ("testAssertEqualCharList0001", testAssertEqualCharList0001),
               ("testAssertEqualCharList0002", testAssertEqualCharList0002),
               ("testAssertEqualCharList0003", testAssertEqualCharList0003),
               ("testAssertEqualCharList0004", testAssertEqualCharList0004),
               ("testAssertEqualCharList1000", testAssertEqualCharList1000),
               ("testAssertEqualCharList1001", testAssertEqualCharList1001),
               ("testAssertEqualCharList1002", testAssertEqualCharList1002),
               ("testAssertEqualCharList1003", testAssertEqualCharList1003),
               ("testAssertEqualCharList1004", testAssertEqualCharList1004),
               ("testAssertEqualCharList1005", testAssertEqualCharList1005),
               ("testAssertEqualCharList1006", testAssertEqualCharList1006),
               ("testAssertEqualCharList1007", testAssertEqualCharList1007),
               ("testAssertEqualCharList1008", testAssertEqualCharList1008),
               ("testAssertEqualCharList1009", testAssertEqualCharList1009),
               ("testAssertEqualCharList1010", testAssertEqualCharList1010),
               ("testAssertEqualCharList1011", testAssertEqualCharList1011),
               ("testAssertEqualCharList1012", testAssertEqualCharList1012),

               ("testAssertEqualStringList0001", testAssertEqualStringList0001),
               ("testAssertEqualStringList0002", testAssertEqualStringList0002),
               ("testAssertEqualStringList0003", testAssertEqualStringList0003),
               ("testAssertEqualStringList0004", testAssertEqualStringList0004),
               ("testAssertEqualStringList1000", testAssertEqualStringList1000),
               ("testAssertEqualStringList1001", testAssertEqualStringList1001),
               ("testAssertEqualStringList1002", testAssertEqualStringList1002),
               ("testAssertEqualStringList1003", testAssertEqualStringList1003),
               ("testAssertEqualStringList1004", testAssertEqualStringList1004),
               ("testAssertEqualStringList1005", testAssertEqualStringList1005),
               ("testAssertEqualStringList1006", testAssertEqualStringList1006),
               ("testAssertEqualStringList1007", testAssertEqualStringList1007),
               ("testAssertEqualStringList1008", testAssertEqualStringList1008),
               ("testAssertEqualStringList1009", testAssertEqualStringList1009),
               ("testAssertEqualStringList1010", testAssertEqualStringList1010),
               ("testAssertEqualStringList1011", testAssertEqualStringList1011),
               ("testAssertEqualStringList1012", testAssertEqualStringList1012)
            ]
        val failCases =
            List.foldl
            (fn failCases => fn (testName, testCase) =>
                ((testCase (); print "."; failCases)
                 handle TestFail =>
                        (print "F"; (testName ^ " by Fail") :: failCases)
                      | exn =>
                        (print "E";
                         (testName ^ " by " ^ Exception.exn_message exn) :: failCases)))
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

  (*************************************************************************)


end ;