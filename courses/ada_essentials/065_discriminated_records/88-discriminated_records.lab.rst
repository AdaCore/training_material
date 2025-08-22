========
Lab
========

---------------------------
Discriminated Records Lab
---------------------------
   
* Requirements for a simplistic employee database
   
   - Create a package to handle varying length strings using variant records

   - Create a package to create employee data in a variant record

      * Store first name, last name, and hourly pay rate for all employees
      * Supervisors must also include the project they are supervising
      * Managers must also include the number of employees they are managing and the department name

   - Main program should read employee information from the console

      * Any number of any type of employees can be entered in any order
      * When data entry is done, print out all appropriate information for each employee

* Hints

   - Create concatenation functions for your varying length string type
   - Is it easier to create an input function for each employee category, or a common one?
 
----------------------------------------------
Discriminated Records Lab Solution - Vstring
----------------------------------------------

.. container:: source_include 065_discriminated_records/lab/discriminated_records/answer/vstring.ads :code:Ada :number-lines:1

.. container:: source_include 065_discriminated_records/lab/discriminated_records/answer/vstring.adb :code:Ada :number-lines:1

------------------------------------------------------
Discriminated Records Lab Solution - Employee (Spec)
------------------------------------------------------

.. container:: source_include 065_discriminated_records/lab/discriminated_records/answer/employee.ads :code:Ada :number-lines:1

-----------------------------------------------------
Discriminated Records Lab Solution - Employee (Body)
-----------------------------------------------------

.. container:: source_include 065_discriminated_records/lab/discriminated_records/answer/employee.adb :code:Ada :number-lines:1

-------------------------------------------
Discriminated Records Lab Solution - Main
-------------------------------------------

.. container:: source_include 065_discriminated_records/lab/discriminated_records/answer/main.adb :code:Ada :number-lines:1
