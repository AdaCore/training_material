----------------------
Advanced Privacy Lab
----------------------
   
(Simplified) Bank Account Managment

  * Overview

    * We want a (reasonably) secure way of storing withdrawals and deposits for bank accounts
    * Each account should have a unique identifier and be "password-protected"
  
  * Goals
   
    * Create application that allows user to

      * Create bank accounts for customers
      * Allow user to deposit and withdraw funds and query the balance
      * Ensure "reasonable" security on the accounts

----------------------
Project Requirements
----------------------

* Private / Limited Types

  * Account information must be :ada:`limited`

    * Should not be able to copy account information

  * User and account information should be "encrypted" and consistent

    * Only the specified customer should have access to an account

* Driver

  * You must maintain a collection of accounts
  * You should be able to perform transactions on any account at any time

* Extra Credit

  * To increase security, no one should be able to write a child package to manipulate account information

-------------------------------------------------
Advanced Privacy Lab Solution - Accounts (Spec)
-------------------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Accounts_Spec :end-before:--Accounts_Spec :code:Ada

-------------------------------------------------
Advanced Privacy Lab Solution - Accounts (Body)
-------------------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Accounts_Body :end-before:--Accounts_Body :code:Ada

-----------------------------------------------------------
Advanced Privacy Lab Solution - Accounts (Key Generation)
-----------------------------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Accounts_Keys :end-before:--Accounts_Keys :code:Ada

--------------------------------------
Advanced Privacy Lab Solution - Main
--------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Main :end-before:--Main :code:Ada

