--------------------------------
Discriminated Record Types Lab
--------------------------------
   
* Requirements for a simplistic employee database
   
   - Create a package to handle varying length strings using variant records

      * The string type **must** be `private`!
      * The variant can appear on the partial definition or the full

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
 
---------------------------------------------------
Discriminated Record Types Lab Solution - Vstring
---------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Strings :end-before:--Strings :code:Ada

----------------------------------------------------------
Discriminated Record Types Lab Solution - Employee (Spec)
----------------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Employee_Spec :end-before:--Employee_Spec :code:Ada

----------------------------------------------------------
Discriminated Record Types Lab Solution - Employee (Body)
----------------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Employee_Body :end-before:--Employee_Body :code:Ada

-------------------------------------------------
Discriminated Record Types Lab Solution - Main
-------------------------------------------------

.. container:: source_include labs/answers/adv_060_discriminated_record_types.txt :start-after:--Main :end-before:--Main :code:Ada
