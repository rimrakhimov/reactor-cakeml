(**
 * A set of assert functions.
 *)
signature ASSERT =
sig

  (***************************************************************************)

  (**
   *  function which asserts the equality of two values.
   * <p>
   *  Functions of this type require two values: expected and actual.
   * They raise Fail if the expected and the actual are not equal to each
   * other.
   * </p>
   *)
  type 'a assertEqual = 'a -> 'a -> unit

  (**
   *  function which translates a value of the type into human readable
   * text representation.
   *)
  type 'a valueFormatter = 'a -> string

  (**
   * detail of failures of assertions.
   *)
  datatype failure =
           (**
            * @params message
            * @param message description of the failure
            *)
           GeneralFailure of string
           (**
            *  indicates that the expected value and acutal value are not equal
            * to each other.
            * @params (expected, actual)
            * @param expected a string repesentation of expected value
            * @param actual a string representation of actual value
            *)
         | NotEqualFailure of string * string

  (***************************************************************************)

  (**
   * the exception that is raised when any assertion fails.
   * @params failure
   * @param failure detail of the failure
   *)
  exception Fail of failure

  (***************************************************************************)

  (**
   * always fail with the specified message
   * @params message
   * @param error message
   *)
  val fail : string -> 'a

  (**
   * fail because exepcted value and acutal value are not equal.
   * @params (expected, actual)
   * @param expected a string representation of expected value
   * @param actual a string representation of actual value
   *)
  val failByNotEqual : (string * string) -> 'a

  (**
   * asserts that a condition is true.
   * @params message v
   * @param error message
   * @param v if v is false, assertion fails with the specified message.
   *)
  val assert : string -> bool -> unit

  (**
   *  general assertion function.
   *
   * @params comparator formatter
   * @param comparator a function which compares two value and returns true if
   *                  they are equal to each other.
   * @param formatter a function which make a string representation of a 
   *                 value of the type
   * @return a function which asserts two values of the type are equal to
   *                 each other.
   *)
  val assertEqual :
      ('a -> 'a -> bool) -> 'a valueFormatter -> 'a assertEqual

  (**
   *  general assertion function.
   * <pre>
   * assertEqualByCompare compare toString
   * </pre>
   * is equivalent to
   * <pre>
   * assertEqual (fn pair => compare pair = EQUAL) toString
   * </pre>
   *
   * @params comparator formatter
   * @param comparator a function which compares two value.
   * @param formatter a function which make a string representation of a 
   *                 value of the type
   * @return a function which asserts two values of the type are equal to
   *                 each other.
   *)
  val assertEqualByCompare :
      ('a -> 'a -> General.order) -> 'a valueFormatter -> 'a assertEqual

  (**
   * @params assert expecteds actual
   * @param assert an assertion function
   * @param expecteds a list of expected values
   * @param actual actual value
   * @return unit if the assert judges that the actual equals to any of
   *      expecteds.
   * @exception Fail raised if none of expecteds equals to the actual.
   *)
  val assertEqualAlternatives : 
      'a assertEqual -> 'a list -> 'a -> unit

  (**
   * converts an assert function on a type to an assert function on another
   * type.
   * <pre>
   *   convertAssertEqual convert assert expected actual
   * </pre>
   * is equivalent to
   * <pre>
   *   assert (convert expected) (convert actual)
   * </pre>
   * 
   * @params converter assert
   * @param converter a function to convert arguments to the assert.
   * @param assert an assert function on the converted values.
   * @return an assert function 
   *)
  val convertAssertEqual : ('a -> 'b) -> 'b assertEqual -> 'a assertEqual

  (****************************************)

  (* assertions specialized for each Top-level types *)

  (**
   * Asserts that two units are equal.
   * This assertion succeeds always.
   *)
  val assertEqualUnit : unit assertEqual

  (**
   * Asserts that two integers are equal.
   *)
  val assertEqualInt : int assertEqual

  (**
   * Asserts that two characters are equal.
   *)
  val assertEqualChar : char assertEqual

  (**
   * Asserts that two strings are equal.
   *)
  val assertEqualString : string assertEqual

(*
  val assertEqualExceptionName : exn assertEqual
*)

(*
  val assertEqualExceptionMessage : exn assertEqual
*)

  (****************************************)

  (**
   * asserts that the locations which two references point hold the same value.
   *)
  val assertEqualRef : 'a assertEqual -> 'a ref assertEqual

  (**
   * asserts that two references point to the same location
   *)
  val assertSameRef : 'a ref assertEqual

  (****************************************)

  (**
   * Asserts that two booleans are equal.
   *)
  val assertEqualBool : bool assertEqual

  (**
   * Asserts that the value is true.
   *)
  val assertTrue : bool -> unit

  (**
   * Asserts that the value is fasle.
   *)
  val assertFalse : bool -> unit

  (****************************************)

  (**
   * Asserts that two option values are equal.
   *)
  val assertEqualOption : 'a assertEqual -> 'a option assertEqual

  (**
   * Asserts that two options of integer are equal.
   *)
  val assertEqualIntOption : int option assertEqual

  (**
   * Asserts that two options of character are equal.
   *)
  val assertEqualCharOption : char option assertEqual

  (**
   * Asserts that two options of string are equal.
   *)
  val assertEqualStringOption : string option assertEqual

  (**
   * Asserts that the option is SOME of any.
   *)
  val assertSome : 'a option -> unit

  (**
   * Asserts that the option is NONE.
   *)
  val assertNone : 'a option -> unit

  (****************************************)

  (**
   * Asserts that two orders are equal.
   *)
  val assertEqualOrder : order assertEqual

  (****************************************)

  (**
   * Asserts that two 2-tuples are equal.
   *)
  val assertEqual2Tuple :
      ('a assertEqual * 'b assertEqual) -> ('a * 'b) assertEqual

  (**
   * Asserts that two 3-tuples are equal.
   *)
  val assertEqual3Tuple :
      ('a assertEqual * 'b assertEqual * 'c assertEqual) ->
      ('a * 'b * 'c) assertEqual

  (**
   * Asserts that two 4-tuples are equal.
   *)
  val assertEqual4Tuple :
      ('a assertEqual * 'b assertEqual * 'c assertEqual * 'd assertEqual) ->
      ('a * 'b * 'c * 'd) assertEqual

  (**
   * Asserts that two 5-tuples are equal.
   *)
  val assertEqual5Tuple :
      (
        'a assertEqual
      * 'b assertEqual
      * 'c assertEqual
      * 'd assertEqual
      * 'e assertEqual
      ) ->
      ('a * 'b * 'c * 'd * 'e) assertEqual

  (**
   * Asserts that two 6-tuples are equal.
   *)
  val assertEqual6Tuple :
      (
        'a assertEqual
      * 'b assertEqual
      * 'c assertEqual
      * 'd assertEqual
      * 'e assertEqual
      * 'f assertEqual
      ) ->
      ('a * 'b * 'c * 'd * 'e * 'f) assertEqual

  (**
   * Asserts that two 7-tuples are equal.
   *)
  val assertEqual7Tuple :
      (
        'a assertEqual
      * 'b assertEqual
      * 'c assertEqual
      * 'd assertEqual
      * 'e assertEqual
      * 'f assertEqual
      * 'g assertEqual
      ) ->
      ('a * 'b * 'c * 'd * 'e * 'f * 'g) assertEqual

  (**
   * Asserts that two 8-tuples are equal.
   *)
  val assertEqual8Tuple :
      (
        'a assertEqual
      * 'b assertEqual
      * 'c assertEqual
      * 'd assertEqual
      * 'e assertEqual
      * 'f assertEqual
      * 'g assertEqual
      * 'h assertEqual
      ) ->
      ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) assertEqual

  (**
   * Asserts that two 9-tuples are equal.
   *)
  val assertEqual9Tuple :
      (
        'a assertEqual
      * 'b assertEqual
      * 'c assertEqual
      * 'd assertEqual
      * 'e assertEqual
      * 'f assertEqual
      * 'g assertEqual
      * 'h assertEqual
      * 'i assertEqual
      ) ->
      ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) assertEqual

  (**
   * Asserts that two 10-tuples are equal.
   *)
  val assertEqual10Tuple :
      (
        'a assertEqual
      * 'b assertEqual
      * 'c assertEqual
      * 'd assertEqual
      * 'e assertEqual
      * 'f assertEqual
      * 'g assertEqual
      * 'h assertEqual
      * 'i assertEqual
      * 'j assertEqual
      ) ->
      ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) assertEqual

  (****************************************)

  (**
   * generates an assert function on a container type.
   * The generated assert function suceeds when both containers are same
   * length, and every pairs of corresponding elements are equal.
   * @params (length, sub, assert)
   * @param length a function which returns the length of a container.
   * @param sub a function which returns the element at the specified position
   *           of a container.
   * @param assert an assert function on the element type.
   *)
  val assertEqualContainer :
      (('a -> int) * (('a * int) -> 'b) * ('b assertEqual)) -> 'a assertEqual

  (**
   * Asserts that two vectors are equal.
   *)
  val assertEqualVector : ('a assertEqual) -> 'a vector assertEqual

  (**
   * Asserts that two arrays are equal.
   *)
  val assertEqualArray : ('a assertEqual) -> 'a array assertEqual

  (**
   * asserts that two array references point to the same array.
   *)
  val assertSameArray : 'a array assertEqual

  (**
   * Asserts that two array references of word8 point to the same array.
   *)
  val assertSameWord8Array : Word8Array.array assertEqual

  (**
   * Asserts that two lists are equal.
   *)
  val assertEqualList : ('a assertEqual) -> 'a list assertEqual

  (**
   * Asserts that two lists of integers are equal.
   *)
  val assertEqualIntList : int list assertEqual

  (**
   * Asserts that two lists of characters are equal.
   *)
  val assertEqualCharList : char list assertEqual

  (**
   * Asserts that two lists of strings are equal.
   *)
  val assertEqualStringList : string list assertEqual

  (***************************************************************************)

end