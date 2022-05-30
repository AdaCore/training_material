-------------------
GNAT Warnings Lab
-------------------

* Code in this lab is not designed to do anything except show warnings behaviors

  * It's taken from :toolname:`CodePeer` examples

* Use :toolname:`GNAT Studio` to open project :filename:`lab.gpr`

* Build the main program

  * Some warnings are enabled by default

-----------------
Enable Warnings
-----------------

* Enable most common warnings and rebuild the project

  * Changing project properties does not force a rebuild
  * First clean the project using the "Broom" icon or :menu:`Build` |rightarrow| :menu:`Clean`

.. container:: animate

  * :menu:`Edit` |rightarrow| :menu:`Project Properties` |rightarrow| :menu:`Switches` |rightarrow| :menu:`Ada`
  * :menu:`Warnings` dialog item
  * :menu:`Activate most optional warnings`

------------------------
Enable Warnings Result
------------------------

.. container:: latex_environment tiny

  ::

    Compile
       [Ada]          main.adb
    main.adb:1:06: warning: unit "Backpack" is not referenced [-gnatwu]
    main.adb:2:06: warning: unit "Conductor" is not referenced [-gnatwu]
    main.adb:3:06: warning: unit "Cruise" is not referenced [-gnatwu]
       [Ada]          backpack.adb
      backpack.adb:43:24: warning: "Res" may be referenced before it has a value [enabled by default]
    backpack.adb:62:20: warning: "Best" may be referenced before it has a value [enabled by default]
    backpack.adb:69:27: warning: "Total_Weight" may be referenced before it has a value [enabled by default]
       [Ada]          conductor.adb
    conductor.adb:58:19: warning: "Inter" is not modified, could be declared constant [-gnatwk]
    conductor.adb:59:19: warning: "Instr" is not modified, could be declared constant [-gnatwk]
    conductor.adb:73:23: warning: condition can only be False if invalid values present [-gnatwc]
       [Ada]          cruise.adb
    cruise.adb:43:07: warning: "Front_Speed" is not modified, could be declared constant [-gnatwk]
    cruise.adb:44:07: warning: "Front_Distance" is not modified, could be declared constant [-gnatwk]
    cruise.adb:45:07: warning: "Self_Speed" is not modified, could be declared constant [-gnatwk]
    cruise.adb:46:07: warning: "Rear_Speed" is not modified, could be declared constant [-gnatwk]
    cruise.adb:47:07: warning: "Rear_Distance" is not modified, could be declared constant [-gnatwk]
    cruise.adb:87:07: warning: variable "Front_Speed" is not referenced [-gnatwu]
    cruise.adb:88:07: warning: "Front_Distance" is not modified, could be declared constant [-gnatwk]
    cruise.adb:89:07: warning: "Front_Braking" is not modified, could be declared constant [-gnatwk]
    cruise.adb:90:07: warning: "Self_Speed" is not modified, could be declared constant [-gnatwk]
    cruise.adb:91:07: warning: "Rear_Speed" is not modified, could be declared constant [-gnatwk]
    cruise.adb:92:07: warning: "Rear_Distance" is not modified, could be declared constant [-gnatwk]
    cruise.adb:93:07: warning: "Sec_Dist_Front" is not modified, could be declared constant [-gnatwk]
    cruise.adb:94:07: warning: "Sec_Dist_Rear" is not modified, could be declared constant [-gnatwk]
       [Ada]          score.ads
    cannot generate code for file score.ads (package spec)

-----------------
Remove Warnings
-----------------

* Fix code to remove warnings and rebuild

* We do not want to remove the dependencies in :ada:`Main`, so we need to use pragmas to ignore the warnings

.. container:: animate

  .. code:: Ada

    with Backpack;
    pragma Unreferenced (Backpack);
    with Conductor;
    pragma Unreferenced (Conductor);
    with Cruise;
    pragma Unreferenced (Cruise);
    procedure Main is
