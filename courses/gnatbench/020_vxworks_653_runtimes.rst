***********************************
GNAT Pro Runtimes For VxWorks 653
***********************************

==========
Runtimes
==========

----------
Runtimes
----------

+ GNAT Pro offers a variety of runtimes restricting the Ada language to a lesser or greater extent
+ Simplifies safety assessments
+ Reduces the footprint
+ Restricted runtimes can be extended with user code

--------------------
653 Runtimes - ZFP
--------------------

+ No footprint (or executable code)
+ Does not rely on the operating system
+ Main restrictions

  + No footprint (or executable code)
  + No tasking
  + No exception propagation (handle in same scope, or else last chance handler)
  + No controlled types
  + No secondary stack (functions returning unconstrained arrays)
  + No allocators
  + Very few Ada.* packages

+ To be used in certified environment or environment with very limited memory

  + Not encouraged for 653 applications

---------------------
653 Runtimes - CERT
---------------------

+ Little footprint
+ Has been certified
+ ZFP restrictions +

  + Allow secondary stack (functions returning unconstrained arrays)
  + Allow exception propagation
  + Default last chance handler
  + Adds many Ada runtime packages

+ As of June 2018, CERT runtime has been certified on at least two projects.

+ Only used with APEX processes.

-------------------------------
653 Runtimes - Ravenscar CERT
-------------------------------

+ Little footprint
+ Can be certified (more expensive)
+ CERT restrictions +

  + Tasking (with the Ravenscar restrictions)

+ Mostly exists for cert applications that want to use Ada tasking rather than APEX processes.

---------------------
653 - Kernel (Full)
---------------------

+ Complete Ada language
+ Not certifiable

==========
Bindings
==========

---------------------------
The ARINC653 APEX Binding
---------------------------

+ The Ada binding to the ARINC653 APEX (APplication EXecutive)

  + Comes with the compiler
  + Available for all runtime libraries
  + Precompiled for each valid combination of runtime library and binding variant

---------------------------
Precompiled APEX Bindings
---------------------------

+ Use APEX (plain) cert bindings with APEX processes

  + No Ada tasking

+ Use APEX ZFP bindings with Ada tasking

  + No APEX processes

.. list-table::
   :header-rows: 1

   * - Binding

     - Runtime

     - Language Version

   * - apex

     - cert

     - Ada 83

   * - apex_zfp

     - kernel, ravenscar -cert, zfp

     - Ada 83

   * - apex_95

     - cert

     - Ada 95/2005/2012

   * - apex_zfp_95

     - kernel, ravenscar -cert, zfp

     - Ada 95/2005/2012

---------------------------
Precompiled APEX Bindings
---------------------------

.. container:: latex_environment tiny

   .. list-table::
      :header-rows: 1

      * - Binding

        - Runtime

        - VxWorks 653 Version

      * - apex

        - cert

        - <= 2.4

      * - apex_zfp

        - kernel, ravenscar -cert, zfp

        - <= 2.4

      * - apex_95

        - cert

        - <= 2.4

      * - apex_zfp_95

        - kernel, ravenscar -cert, zfp

        - <= 2.4

      * - apex_1.3

        - cert

        - 2.5.0.2

      * - apex_1.3_zfp

        - kernel, ravenscar -cert, zfp

        - 2.5.0.2

      * - apex_1.3_95

        - cert

        - 2.5.0.2

      * - apex_1.3_zfp_95

        - kernel, ravenscar -cert, zfp

        - 2.5.0.2

      * - apex_VXW653-11162

        - cert

        - 2.5-2.5.0.1, 3.0, 3.1

      * - apex_VXW653-11162_zfp

        - kernel, ravenscar_cert , zfp

        - 2.5-2.5.0.1, 3.0, 3.1

      * - apex_VXW653-11162_95

        - cert

        - 2.5-2.5.0.1, 3.0, 3.1

      * - apex_VXW653-11162_zfp_95

        - kernel, ravenscar -cert, zfp

        - 2.5-2.5.0.1, 3.0, 3.1

-------------------------
ARINC-653 APEX Bindings
-------------------------

.. container:: latex_environment scriptsize

  .. columns::

    .. column::

      + Accessed by including the predefined project :filename:`apex.gpr`
      + Three scenario variables must be set

        + **PLATFORM**

          + target triplet for your platform

        + **RUNTIME**

          + the runtime library to use
          + ``zfp``, ``cert``, ``ravenscar-cert`` or ``kernel`` (default)

        + **BINDING**

          + the APEX binding to use
          + ``apex`` (default), ``apex_95``, ``apex_zfp``, ``apex_zfp_95``, or their variants for other VxWorks 653 versions

    .. column::

      .. code:: Ada

         with "apex";
         project My_Project is
         ...
            for Source_Dirs use
              (<list of project source dirs>);

            -- To discriminate on variable Runtime:
            case APEX.Runtime is
               when "cert" =>
                  ...
            end case;

         end My_Project;
