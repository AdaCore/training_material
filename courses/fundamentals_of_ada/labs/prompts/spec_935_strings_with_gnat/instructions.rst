:title: Word Counter
:subtitle: Strings with GNAT

* Write code to count number of occurences within a text corpus
* The given corpus is "Romeo and Juliet" by Shakespeare
* You are given a starting piece of code

    * It uses :ada:`Bounded_String` for getting the user inputs
    * The :ada:`GNAT.Regpat` to match Regular Expressions
    * And :ada:`Unbounded_String` for the corpus

-----------
Question 1
-----------

* In :file:`input.ads` and :file:`input.adb`

    - Implement the conversion routines between :ada:`Input_Value_T` and :ada:`String`

-----------
Question 2
-----------

* In :file:`input.ads` and :file:`input.adb`

    - Implement :ada:`Get` to read a line from the console

-----------
Question 3
-----------

* In :file:`corpuses.ads` and :file:`corpuses.adb`

    - Implement :ada:`Count` to count over the corpus for a single Input

-----------
Question 4
-----------

* In :file:`input.ads` and :file:`input.adb`

    - Implement :ada:`To_Pattern` to convert from an Input_Value to a Pattern
    - Implement :ada:`Append` to add a Pattern to the :ada:`Value_Regexp_T` set

* In :file:`corpuses.ads` and :file:`corpuses.adb`

    - Implement :ada:`Count` over the corpus for the regex matcher

* Tip: There is no :ada:`Count` function, you will need to :ada:`Match` in a loop

----------
Advanced
----------

* The code can be adapted to support Unicode through the use of :ada:`Wide_Wide_String`
* For that you will need to also change :ada:`data_files.ads` and :ada:`data_files.adb`

------
Tips
------

* Due to the use of privacy, use :ada:`rename`, and shorthands to access the types operations.
* Use the GNAT RM A.4.4 and A.4.5 for the Bounded/Unbounded string types
* Use the spec of GNAT.Regpat at https://files.adacore.com/gnat-book/rts/g-regpat__ads.htm
