========
Lab
========

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
      * Create an array of these records indexed by the login identification
      * Uuser should be able to retrieve a pointer to the record to modify it directly
      * User should also be able to retrieve the record data without being able to change
        it in the database

   - Main program should:

      + Set passwords and initial counter values for many logins
      + Print password and counter value for each login

* Hint

   - Password is a string of varying length

      - Easiest way to do this is a pointer to a string that gets initialized to the correct length

----------------------------------------------
Access Types Lab Solution - Password Manager
----------------------------------------------

.. container:: source_include 140_access_types/lab/access_types/answer/password_manager.ads :code:Ada

.. container:: source_include 140_access_types/lab/access_types/answer/password_manager.adb :code:Ada

----------------------------------
Access Types Lab Solution - Main
----------------------------------

.. container:: source_include 140_access_types/lab/access_types/answer/main.adb :code:Ada
