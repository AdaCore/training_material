************************
GNAT-defined libraries
************************

.. include:: support_files/symbols.rst

--------------
Introduction
--------------

* GNAT defines a large number of packages

    - For a large variety of **needs**

* Portable API between several targets / OSes
* Use for performance optimization

    - Interface to **machine-specific** operations
    - Compile time **hints**

* Use for bebugging

    - **Build** informations
    - Raw **memory** access

* Use for code reuse

    - **Generic** implementation of complex algorithms
    - **Well-known** libraries and interfaces (regex, CGI...)

* Warning: GNAT extension are, by definition, GNAT-specific

----------------
Support Status
----------------

* Each set of packages is developped and maintained apart

    - All of them are **supported**
    - All of them are **tested**
    - **Upwards compatibility** is guaranteed

* Some packages are **very** specific

    - eg :ada:`GNAT.Exceptions` for Ada 83
    - The use of some of them is **discouraged**
    - **Not** shown here
    - Complete list in the GNAT Reference Manual chapter 12

https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/the_gnat_library.html#the-gnat-library 

* Look at the **source code** of the packages for further information

-----------------------------------
:ada:`package GNAT` - OS
-----------------------------------

* Those packages are **not always portable**
* :ada:`GNAT.Directory_Operations` - Operations on directories
* :ada:`GNAT.Lock_Files` - Access to file-locking **primitives**
* :ada:`GNAT.OS_Lib` - Bindings to **OS-specific** operations
* :ada:`GNAT.Registry` - Windows registry
* :ada:`GNAT.Signals` - Signals blocking
* :ada:`GNAT.Sockets` - **Portable**, high-level sockets interface

--------------------------
:ada:`package GNAT` - IO
--------------------------

* Various text and binary IO are available
* :ada:`GNAT.CGI` - CGI interface to a **webserver**
* :ada:`GNAT.Command_Line` - **Switches**, options
* :ada:`GNAT.Expect` - **External process** IO, similar to Tcl's Expect
* :ada:`GNAT.IO`, :ada:`GNAT.IO_Aux` - stdin, stdout, files IO
* :ada:`GNAT.Serial_Communications` - Serial ports - **GNU/Linux and Windows**

---------------------------------------
:ada:`package GNAT` - Data Structures
---------------------------------------

* :ada:`GNAT.Array_Split` - **No-copy** accesses to array slices
* :ada:`GNAT.Rewrite_Data` - **Performant** on-the-fly rewrite of data-streams
* :ada:`GNAT.Bounded_Buffers` - **Generic** bounded buffer
* :ada:`GNAT.Bubble_Sort`
* :ada:`GNAT.Heap_Sort`
* :ada:`GNAT.HTable`, :ada:`GNAT.Dynamic_HTables` - **Hash** tables
* :ada:`GNAT.Table`, :ada:`GNAT.Dynamic_Tables` - Array with **dynamic length**

------------------------------------------
:ada:`package GNAT` - Numeric Algorithms
------------------------------------------

* :ada:`GNAT.AWK` - AWK-like capabilities on files
* :ada:`GNAT.Calendar` - Extension of :ada:`Ada.Calendar`
* :ada:`GNAT.CRC32`
* :ada:`GNAT.SHA1`, :ada:`GNAT.SHA224`, :ada:`GNAT.SHA256`, :ada:`GNAT.SHA384`, :ada:`GNAT.SHA512`
* :ada:`GNAT.MD5`
* :ada:`GNAT.Perfect_Hash_Generators` - **No collision** hashes
* :ada:`GNAT.Random_Numbers` - **Improved API** on standard's RNG
* :ada:`GNAT.String_Hash` - Generic hash function on arrays of scalars

------------------------------------
:ada:`package GNAT` - Multitasking
------------------------------------

* :ada:`GNAT.Bounded_Mailboxes` - **Thread-safe** mailboxes
* :ada:`GNAT.Semaphores` - Semaphores using **protected** types
* :ada:`GNAT.Task_Lock` - **Global** task lock
* :ada:`GNAT.Threads` - **Foreign-language** threads handling

-----------------------------
:ada:`package GNAT` - Debug
-----------------------------

* Memory

    - :ada:`GNAT.Debug_Pools` - Memory pool for debugging **dynamic** issues
    - :ada:`GNAT.Memory_Dump` - Dumps memory to standard output or error

* Exceptions

    - :ada:`GNAT.Exception_Actions` - **Callback** on exception
    - :ada:`GNAT.Exception_Traces` - **Automatic output** on exceptions
    - :ada:`GNAT.Traceback` - Exception Traceback information

---------------------------------------
:ada:`package GNAT` - Metaprogramming
---------------------------------------

* Build context

    - :ada:`GNAT.Source_Info` - Source files and lines informations
    - :ada:`GNAT.Compiler_Version` - Binder version informations
    - :ada:`GNAT.Bind_Environment` - Binder environment (binder's :command:`-V`)

* Execution context

    - :ada:`GNAT.Branch_Prediction` - Branch-predictor **hints**
    - :ada:`GNAT.Float_Control` - FPU control
    - :ada:`GNAT.Secondary_Stack_Info` - **High** water mark

-------------------------------------------
:ada:`package GNAT` - Encodings / Unicode
-------------------------------------------

* :ada:`GNAT.Byte_Order_Mark` - UTF-8 BOM
* :ada:`GNAT.Decode_String` - Generic decoders **from** :ada:`Wide_*_String`
* :ada:`GNAT.Decode_UTF8_String` - Instanciation for UTF-8
* :ada:`GNAT.Encode_String` - Generic encoders **to** :ada:`Wide_*_String`
* :ada:`GNAT.Encode_UTF8_String` - Instanciation for UTF-8
- :ada:`GNAT.UTF_32` - :ada:`Wide_*_Character` **categorization** routines

-------------------------------
:ada:`package GNAT` - Strings
-------------------------------

* Search

    - :ada:`GNAT.Regexp`, :ada:`GNAT.Regpat` - Regex matching
    - :ada:`GNAT.Spelling_Checker` - Search *near-spellings* in :ada:`String` arrays

        + Also :ada:`GNAT.Wide_*_Spelling_Checker`

* Modification

    - :ada:`GNAT.Case_Util` - Simple upper/lower case handling
    - :ada:`GNAT.Formatted_String` - C/C++'s :C:`printf(...)`
    - :ada:`GNAT.String_Split` - :ada:`GNAT.Array_Split` for :ada:`String`

        + Also :ada:`GNAT.Wide_*_String_Split`


    - :ada:`GNAT.Strings` - Common :ada:`access String` types and subprograms

        * Also :ada:`array of access String`

