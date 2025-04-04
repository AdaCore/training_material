========================
Quantified Expressions
========================

------------------
Range-based Form
------------------

* Based on the usual *for loop* syntax over a range

  .. code:: ada

     for J in T'Range loop
        T (J) := 0;
     end loop;
     pragma Assert (for all J in T'Range => T(J) = 0);

* Universally quantified expression :ada:`(for all J in A .. B => Property)`

  - Express that property holds for **all** values in the range
  - True if the range is empty (:math:`\forall` in logic)
  - At runtime, executed as a loop which stops at first value where the property is not satisfied

* Existentially quantified expression :ada:`(for some J in A .. B => Property)`

  - Express that property holds for **at least one** value in the range
  - False if the range is empty (:math:`\exists` in logic)
  - At runtime, executed as a loop which stops at first value where the property is satisfied

------------------
Array-based Form
------------------

* Based on the *for loop* syntax over an array

  .. code:: ada

     for E of T loop
        E := 0;
     end loop;
     pragma Assert (for all E of T => E = 0);

* Counterparts of range-based forms

  - Universally quantified expression :ada:`(for all E of T => Property)`
  - Existentially quantified expression :ada:`(for some E of T => Property)`

* Note: always in **parentheses**!

----------------------------------
Range-based Vs Array-based Forms
----------------------------------

* Array-based form only possible if :ada:`Property` does **not** refer to the
  **index**

* Example: array :ada:`T` is sorted

  .. code:: ada

     (for all J in T'Range =>
       (if J /= T'First then T(J-1) <= T(J)))

  or (better for proof to avoid the need for induction)

  .. code:: ada

     (for all J in T'Range =>
       (for all K in T'Range =>
         (if J < K then T(J) <= T(K))))

-----------------------------
General Iteration Mechanism
-----------------------------

* **Based** on the :ada:`Iterable` aspect on a type

  - **Not the same** as the standard Ada mechanism!
  - **Simpler** mechanism adopted for the SPARK formal containers

  .. code:: ada

     type Container is private with
       Iterable => (First       => First,
                    Next        => Next,
                    Has_Element => Element
                    Element     => Element);

* :dfn:`Iteration over positions` uses :ada:`for .. in` syntax

 - Uses cursor type with :ada:`First`, :ada:`Next` and :ada:`Has_Element`
 - Function :ada:`Element` is **not** required

* :dfn:`Iteration over components` uses :ada:`for .. of` syntax

  - Based on the previous iteration
  - Function :ada:`Element` retrieves the **component** for a given cursor

----------------------------------
Iteration Over Formal Containers
----------------------------------

* **Generic** units compatible with SPARK

  - The API is slightly different from standard Ada containers
  - Available in the SPARK Library

* Available for **all** formal containers:

  - vectors
  - doubly linked lists
  - sets (hashed and ordered)
  - maps (hashed and ordered)

* Iteration over positions

  - Access to **component** through function :ada:`Element`
  - For maps, access to **key** through function :ada:`Key`

* Iteration over components

  - For maps, really an iteration over **keys**

    - Use another function :ada:`Element` to get **component**

-------------------------------
Iteration Over Formal Vectors
-------------------------------

* Only formal container to have 3 iteration mechanisms
* Range-based iteration (using :command:`-gnatX` for dot-notation)

  .. code:: ada

     for J in V.First_Index .. V.Last_Index loop
        V.Replace_Element (J, 0);
     end loop;
     pragma Assert
       (for all J in V.First_Index .. V.Last_Index => V.Component (J) = 0);

* Iteration over positions

  .. code:: ada

     for J in V loop
        V.Replace_Element (J, 0);
     end loop;
     pragma Assert (for all J in V => V.Element (J) = 0);

* Iteration over components (**no update**!)

  .. code:: ada

     for E of V loop
        pragma Assert (E = 0);
     end loop;
     pragma Assert (for all E of V => E = 0);

