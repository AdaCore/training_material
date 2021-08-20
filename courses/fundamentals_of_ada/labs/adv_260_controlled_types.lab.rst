
----------------------
Controlled Types Lab
----------------------
   
* (Simplified) Computer User Account System

  * Overview

    * User should be able to create multiple user accounts for a computer network
    * Each account can be a different "level", with different "quota" values for each level

  * Goals

    * All user account information should be stored in their own record

    * When a user account "level" is changed or the account is deleted, the total quota should reflect the change

----------------------
Project Requirements
----------------------

* User Account Content

  * At a minimum this should include: user's name, email address, quota value, "level"

* Controlled Types

  * The user should not have to worry about tracking quota information

    * Setting the "level" or deleting the account should set the quota for the user in addition to modifying the global value

* Duplicate user names should be allowed

  * Email addresses should be unique

* Application should be able to create, modify, and print a user record, in addition to query allocated quota
   
-----------------------------------------------------
Controlled Types Lab Solution - User Account (Spec)
-----------------------------------------------------

.. container:: source_include labs/answers/adv_260_controlled_types.txt :start-after:--User_Accounts_Spec :end-before:--User_Accounts_Spec :code:Ada

--------------------------------------
Controlled Types Lab Solution - Main
--------------------------------------

.. container:: source_include labs/answers/adv_260_controlled_types.txt :start-after:--Main :end-before:--Main :code:Ada

-----------------------------------------------------
Controlled Types Lab Solution - User Account (Body)
-----------------------------------------------------

.. container:: source_include labs/answers/adv_260_controlled_types.txt :start-after:--User_Accounts_Body :end-before:--User_Accounts_Body :code:Ada

------------------------------------------------------------
Controlled Types Lab Solution - User Account (Body - Cont)
------------------------------------------------------------

.. container:: source_include labs/answers/adv_260_controlled_types.txt :start-after:--User_Accounts_Support :end-before:--User_Accounts_Support :code:Ada
