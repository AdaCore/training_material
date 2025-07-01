=============================================
Implicitly and Explicitly Raised Exceptions
=============================================

------------------------------
Implicitly-Raised Exceptions
------------------------------

* Correspond to language-defined checks
* Can happen by statement execution

   .. code:: Ada

      K := -10;  -- where K must be greater than zero

* Can happen by declaration elaboration

   .. code:: Ada

      Doomed : array (Positive) of Big_Type;

----------------------------------
Some Language-Defined Exceptions
----------------------------------

* :ada:`Constraint_Error`

    - Violations of constraints on range, index, etc.

* :ada:`Program_Error`

    - Runtime control structure violated (function with no return ...)

* :ada:`Storage_Error`

    - Insufficient storage is available

* For a complete list see RM Q-4

------------------------------
Explicitly-Raised Exceptions
------------------------------

.. container:: columns

  .. container:: column

    * Raised by application via :ada:`raise` statements

       - Named exception becomes active

    * Syntax

      .. container:: latex_environment footnotesize

       .. code:: Ada

          raise_statement ::= raise; |
             raise exception_name
             [with string_expression];

       *Note "with string_expression" only available in Ada 2005 and later*

    * A :ada:`raise` by itself is only allowed in handlers

  .. container:: column

    .. container:: latex_environment footnotesize

      .. code:: Ada

       if Unknown (User_ID) then
         raise Invalid_User;
       end if;

       if Unknown (User_ID) then
         raise Invalid_User
            with "Attempt by " &
                 Image (User_ID);
       end if;
