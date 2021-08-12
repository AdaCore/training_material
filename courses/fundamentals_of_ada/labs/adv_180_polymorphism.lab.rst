---------------------------
Advanced Polymorphism Lab
---------------------------

(Simplified) Employee Database

  * Overview

    * Create a database of people, some of which are employees, and some of those are in manageent
    * Employees are people, and management are employess (which means they are people too!)

  * Goal

    * You should be able to create people, "hire" them, and "promote" then
    * You need to maintain a list of all people and their current "status"

----------------------
Project Requirements
----------------------

* Data structure

  * A person must have at least two identifying characterstics (name, birth date, etc)

  * An employee adds a unique ID to a person, plus some other characteristics (e.g. pay rate, hire date)

  * A manager adds a new characteristic to an employee (e.g. department)

* User Interface

  * Create a person
  * "Hire" someone (e.g. change their category from "person" to "employee"
  * "Promote" someone (e.g. change their category from "employee" to "manager"
  * Print information

    * Single entry
    * All entries
    * All entries of a particular category

---------------------------------------------
Advanced Polymorphism Lab Solution - Person
---------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Person_Pkg :end-before:--Person_Pkg :code:Ada

-----------------------------------------------
Advanced Polymorphism Lab Solution - Employee
-----------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Employee_Pkg :end-before:--Employee_Pkg :code:Ada

-------------------------------------------------
Advanced Polymorphism Lab Solution - Management
-------------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Management_Pkg :end-before:--Management_Pkg :code:Ada

---------------------------------------------------
Advanced Polymorphism Lab Solution - Main (Utils)
---------------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Main_Utils :end-before:--Main_Utils :code:Ada

---------------------------------------------------
Advanced Polymorphism Lab Solution - Main (Body)
---------------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Main_Body :end-before:--Main_Body :code:Ada

---------------------------------------------------
Advanced Polymorphism Lab Solution - Main (Print)
---------------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Main_Print :end-before:--Main_Print :code:Ada

---------------------------------------------------
Advanced Polymorphism Lab Solution - Dates (Spec)
---------------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Dates_Spec :end-before:--Dates_Spec :code:Ada

---------------------------------------------------
Advanced Polymorphism Lab Solution - Dates (Body)
---------------------------------------------------

.. container:: source_include labs/answers/adv_180_polymorphism.txt :start-after:--Dates_Body :end-before:--Dates_Body :code:Ada

