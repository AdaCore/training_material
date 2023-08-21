------------------
Access Types Lab
------------------

* Overview

  - Create a (really simple) Password Manager

    * The Password Manager should store the password and a counter for each of some number of logins
    * As it's a Password Manager, you want to modify the data directly (not pass the information around)

* Requirements

   - Create a Password Manager package

      * Create a record to store the password string and the counter
      * Create an array of these records indexed by the login identifier
      * The user should be able to retrieve a pointer to the record, either for modification or for viewing

   - Main program should:

      + Set passwords and initial counter values for many logins
      + Print password and counter value for each login

* Hint

   - Password is a string of varying length

      - Easiest way to do this is a pointer to a string that gets initialized to the correct length

----------------------------------------------
Access Types Lab Solution - Password Manager
----------------------------------------------

.. container:: source_include labs/answers/140_access_types.txt :start-after:--Password :end-before:--Password :code:Ada

----------------------------------
Access Types Lab Solution - Main
----------------------------------

.. container:: source_include labs/answers/140_access_types.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
