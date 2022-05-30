-------------------------
GNAT Style Checking Lab
-------------------------

* We will use the same project as the *GNAT Warnings* lab

  * Now that the code is "good", we can make sure it's "neat"

* Use :toolname:`GNAT Studio` to open project :filename:`lab.gpr`

* Enable *all* style checks and rebuild the project

  * Don't forget to clean the project first
  * First clean the project using the "Broom" icon or :menu:`Build` |rightarrow| :menu:`Clean`

.. container:: animate

  * :menu:`Edit` |rightarrow| :menu:`Project Properties` |rightarrow| :menu:`Switches` |rightarrow| :menu:`Ada`
  * :menu:`Style checks` dialog item
  * Click on all the boxes

-----------------------
Style Checking Result
-----------------------

.. container:: latex_environment tiny

  ::

    Compile
       [Ada]          main.adb
       [Ada]          backpack.adb
    backpack.adb:3:04: (style) subprogram body has no previous spec
    backpack.adb:20:04: (style) subprogram body has no previous spec
    backpack.adb:20:13: (style) subprogram body "Find_Mandatory" not in alphabetical order
    backpack.adb:33:04: (style) subprogram body has no previous spec
    backpack.adb:50:14: (style) subprogram body "Back_To_School" not in alphabetical order
    backpack.adb:60:41: (style) bad capitalization, mixed case required
       [Ada]          conductor.adb
    conductor.adb:11:37: (style) bad capitalization, mixed case required
    conductor.adb:33:42: (style) bad capitalization, mixed case required
    conductor.adb:39:48: (style) bad capitalization, mixed case required
    conductor.adb:43:64: (style) bad capitalization, mixed case required
    conductor.adb:73:41: (style) bad capitalization, mixed case required
    conductor.adb:84:72: (style) bad capitalization, mixed case required
       [Ada]          cruise.adb
    cruise.adb:4:04: (style) subprogram body has no previous spec
    cruise.adb:12:04: (style) subprogram body has no previous spec
    cruise.adb:12:13: (style) subprogram body "Control_Braking" not in alphabetical order
    cruise.adb:25:04: (style) subprogram body has no previous spec
    cruise.adb:40:04: (style) subprogram body has no previous spec
    cruise.adb:40:13: (style) subprogram body "Control_Position" not in alphabetical order
       [Ada]          score.ads
    cannot generate code for file score.ads (package spec)
    gprbuild: *** compilation phase failed

-------------------------
Clean Up Style Warnings
-------------------------

* Fix style warnings you "agree" with

  * Like capitalization, adding subprogram specs

* Turn off style warnings you don't "agree" with

  * Like alphabetical order

* Repeat until the code looks good!
