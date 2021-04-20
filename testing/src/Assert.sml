structure Assert =
  struct
    
    (***************************************************************************)

    type 'a assertEqual = 'a -> 'a -> unit    

    type 'a valueFormatter = 'a -> string

    datatype failure =
            GeneralFailure string
          | NotEqualFailure string string

    (***************************************************************************)

    exception Fail failure

    (***************************************************************************)

    fun fail message = raise (Fail (GeneralFailure message))

    fun failByNotEqual (expected, actual) =
        raise Fail(NotEqualFailure expected actual)

    fun assert message isTrue =
      case isTrue of
        True => ()
      | False => fail message

    fun assertEqual comparator valueFormatter expected actual =
        if comparator expected actual
        then ()
        else
          let
            val expectedText = valueFormatter expected
            val actualText = valueFormatter actual
          in
            failByNotEqual (expectedText, actualText)
          end

    fun assertEqualByCompare comparator =
        assertEqual (fn x => fn y => comparator x y = Equal)

    fun assertEqualAlternatives assert expecteds actual = 
      case expecteds of
        [expected] => assert expected actual
      | (expected :: otherExpecteds) =>
          (assert expected actual
          handle Fail _ => assertEqualAlternatives assert otherExpecteds actual)
      | [] => fail "assertEqualAlternatives expects non-empty list"

    fun convertAssertEqual convert assert expected actual =
        assert (convert expected) (convert actual)

    (****************************************)

    (* assertions specialized for every Top-level types *)


    fun assertEqualUnit () () = ()

    fun assertEqualInt expected actual =
        assertEqualByCompare Int.compare Int.toString expected actual

    local
      fun charCompare x y =
          if (Char.<= x y)
          then if (Char.<= y x) then Equal else Less
          else Greater;
    in
      fun assertEqualChar expected actual = 
        assertEqualByCompare charCompare String.str expected actual
    end

    fun assertEqualString expected actual = 
        assertEqualByCompare
            String.compare (fn s => "\"" ^ s ^ "\"") expected actual

    (****************************************)

    (**
    * asserts that the locations which two references point hold the same value.
    *)
    fun assertEqualRef assertReferred expected actual =
        if expected = actual
        then ()
        else (assertReferred (! expected) (! actual))
              handle Fail(NotEqualFailure expected actual) =>
                    failByNotEqual ("ref(" ^ expected ^ ")", "ref(" ^ actual ^ ")")

    (* *
    * asserts that two references point to the same location
    *)
    fun assertSameRef expected actual =
        (assertEqual
        (fn expected  => fn actual => expected = actual)
        (fn reference => "ref ??")
        expected
        actual)
        handle Fail(NotEqualFailure expected actual) =>
              failByNotEqual ("ref(" ^ expected ^ ")", "ref(" ^ actual ^ ")")

    (****************************************)

    fun assertEqualBool (expected : bool) actual =
        assertEqual
        (fn expected => fn actual => expected = actual)
        (fn v => case v of True => "true" | False => "false")
        expected
        actual
    fun assertTrue actual = assertEqualBool True actual
    fun assertFalse actual = assertEqualBool False actual
    
    (****************************************)

    fun assertEqualOption assertHolded expected actual =
        case (expected, actual) of
            (None, None) => ()
          | (None, Some _) => failByNotEqual ("None", "Some(?)")
          | (Some _, None) => failByNotEqual ("Some(?)", "None")
          | (Some expectedValue, Some actualValue) =>
            (assertHolded expectedValue actualValue)
            handle Fail (GeneralFailure message) =>
                  fail ("both was Some_, but contents differ:" ^ message)
                | Fail (NotEqualFailure expected actual) =>
                  failByNotEqual
                      ("Some(" ^ expected ^ ")", "Some(" ^ actual ^ ")")

    val assertEqualIntOption = assertEqualOption assertEqualInt

    val assertEqualCharOption = assertEqualOption assertEqualChar

    val assertEqualStringOption = assertEqualOption assertEqualString

    fun assertSome actual =
        if Option.isSome actual then () else fail "Some expected."
    fun assertNone actual =
        if Option.isNone actual then () else fail "None expected."

    (****************************************)

    fun assertEqualOrder expected actual =
        assertEqual
        (fn expected => fn actual => expected = actual)
        (fn v => case v of
            Less => "Less"
          | Equal => "Equal"
          | Greater => "Greater")
        expected
        actual

    (****************************************)

    local
      fun assertTupleElement index assertElement expectedValue actualValue =
          assertElement expectedValue actualValue
          handle Fail(NotEqualFailure expected actual) =>
                let
                  val expectedString =
                      "#" ^ Int.toString index ^ " = " ^ expected
                  val actualString =
                      "#" ^ Int.toString index ^ " = " ^ actual
                in failByNotEqual (expectedString, actualString)
                end
    in
      fun assertEqual2Tuple
          (assert1, assert2)
          (expected1, expected2)
          (actual1, actual2) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2
          )

      fun assertEqual3Tuple
          (assert1, assert2, assert3)
          (expected1, expected2, expected3)
          (actual1, actual2, actual3) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3
          )

      fun assertEqual4Tuple
          (assert1, assert2, assert3, assert4)
          (expected1, expected2, expected3, expected4)
          (actual1, actual2, actual3, actual4) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4
          )

      fun assertEqual5Tuple
          (assert1, assert2, assert3, assert4, assert5)
          (expected1, expected2, expected3, expected4, expected5)
          (actual1, actual2, actual3, actual4, actual5) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4;
            assertTupleElement 5 assert5 expected5 actual5
          )

      fun assertEqual6Tuple
          (assert1, assert2, assert3, assert4, assert5, assert6)
          (expected1, expected2, expected3, expected4, expected5, expected6)
          (actual1, actual2, actual3, actual4, actual5, actual6) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4;
            assertTupleElement 5 assert5 expected5 actual5;
            assertTupleElement 6 assert6 expected6 actual6
          )

      fun assertEqual7Tuple
          (assert1, assert2, assert3, assert4, assert5, assert6, assert7)
          (
            expected1,
            expected2,
            expected3,
            expected4,
            expected5,
            expected6,
            expected7
          )
          (actual1, actual2, actual3, actual4, actual5, actual6, actual7) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4;
            assertTupleElement 5 assert5 expected5 actual5;
            assertTupleElement 6 assert6 expected6 actual6;
            assertTupleElement 7 assert7 expected7 actual7
          )

      fun assertEqual8Tuple
          (assert1, assert2, assert3, assert4, assert5, assert6, assert7, assert8)
          (
            expected1,
            expected2,
            expected3,
            expected4,
            expected5,
            expected6,
            expected7,
            expected8
          )
          (actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8)=
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4;
            assertTupleElement 5 assert5 expected5 actual5;
            assertTupleElement 6 assert6 expected6 actual6;
            assertTupleElement 7 assert7 expected7 actual7;
            assertTupleElement 8 assert8 expected8 actual8
          )

      fun assertEqual9Tuple
          (
            assert1,
            assert2,
            assert3,
            assert4,
            assert5,
            assert6,
            assert7,
            assert8,
            assert9
          )
          (
            expected1,
            expected2,
            expected3,
            expected4,
            expected5,
            expected6,
            expected7,
            expected8,
            expected9
          )
          (
            actual1,
            actual2,
            actual3,
            actual4,
            actual5,
            actual6,
            actual7,
            actual8,
            actual9
          ) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4;
            assertTupleElement 5 assert5 expected5 actual5;
            assertTupleElement 6 assert6 expected6 actual6;
            assertTupleElement 7 assert7 expected7 actual7;
            assertTupleElement 8 assert8 expected8 actual8;
            assertTupleElement 9 assert9 expected9 actual9
          )

      fun assertEqual10Tuple
          (
            assert1,
            assert2,
            assert3,
            assert4,
            assert5,
            assert6,
            assert7,
            assert8,
            assert9,
            assert10
          )
          (
            expected1,
            expected2,
            expected3,
            expected4,
            expected5,
            expected6,
            expected7,
            expected8,
            expected9,
            expected10
          )
          (
            actual1,
            actual2,
            actual3,
            actual4,
            actual5,
            actual6,
            actual7,
            actual8,
            actual9,
            actual10
          ) =
          (
            assertTupleElement 1 assert1 expected1 actual1;
            assertTupleElement 2 assert2 expected2 actual2;
            assertTupleElement 3 assert3 expected3 actual3;
            assertTupleElement 4 assert4 expected4 actual4;
            assertTupleElement 5 assert5 expected5 actual5;
            assertTupleElement 6 assert6 expected6 actual6;
            assertTupleElement 7 assert7 expected7 actual7;
            assertTupleElement 8 assert8 expected8 actual8;
            assertTupleElement 9 assert9 expected9 actual9;
            assertTupleElement 10 assert10 expected10 actual10
          )
    end (* local *)

    (****************************************)

    fun assertEqualContainer
        (getLength, getElement, assertElement) expected actual =
        let
          val expectedLength = getLength expected
          val actualLength = getLength actual
          fun scan index =
              if index < actualLength
              then
                (assertElement
                (getElement expected index)
                (getElement actual index)
                handle
                Fail(GeneralFailure message) =>
                fail ("index " ^ Int.toString index ^ " " ^ message)
              | Fail(NotEqualFailure expected actual) =>
                let val indexString = Int.toString index
                in
                    failByNotEqual
                    (
                      "[...{" ^ indexString ^ "}..., " ^ expected ^ ", ...]",
                      "[...{" ^ indexString ^ "}..., " ^ actual ^ ", ...]"
                    )
                end;
                scan (index + 1))
              else
                ()
        in
          if expectedLength <> actualLength
          then
              failByNotEqual
              (
                Int.toString expectedLength ^ " elements",
                Int.toString actualLength ^ " elements"
              )
          else
            scan 0
        end

    fun assertEqualVector assertElement expected actual =
        assertEqualContainer
        (Vector.length, Vector.sub, assertElement)
        expected
        actual

    fun assertEqualArray assertElement expected actual =
        assertEqualContainer
        (Array.length, Array.sub, assertElement)
        expected
        actual

    (**
    * asserts that two array references point to the same array.
    *)
    (* This is a generic function used for mono-array also. *)
    fun assertSameArray expected actual =
        (assertEqual
        (fn expected => fn actual => expected = actual)
        (fn array => "array")
        expected
        actual)
        handle Fail(NotEqualFailure expected actual) =>
              failByNotEqual
                  ("array(" ^ expected ^ ")", "array(" ^ actual ^ ")")

    val assertSameWord8Array : (byte_array assertEqual) = assertSameArray

    fun assertEqualList assertElement expected actual = 
        assertEqualVector
        assertElement
        (Vector.fromList expected)
        (Vector.fromList actual)

    val assertEqualIntList = assertEqualList assertEqualInt 

    val assertEqualCharList = assertEqualList assertEqualChar

    val assertEqualStringList = assertEqualList assertEqualString

  end ;