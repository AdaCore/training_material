.. role:: ada(code)
    :language: Ada

******************************
Course Materials Style Guide
******************************

==============
Introduction
==============

---------
Purpose
---------

This document serves as a collection of the policies and procedures
for writing ReStructured Text (RST) content for the courses offered
by the Field Engineering team.

As this document was created long after the original RST files
were created, we understand that most of the existing content does
not match this guide. But any new additions and modifications should
adhere to them.

----------------------
Producing This Guide
----------------------

This file is designed to be read as a document (as opposed to the
slides we generate for training purposes). As such, the command
to generate the document would be:

``pandoc_fe.py --source ./style_guide.rst --output style_guide.docx --extension docx``

(update paths as appropriate)

==================
Course Structure
==================

---------
Courses
---------

A course is stored in a directory in the **courses** folder of the
repository. The course directory contains all of the module information
and a **standard_course.txt** file that contains a list of the modules
most commonly taught in that course. If there is a need for a different
course listing that is used consistently, it will also have the **.txt**
extension.

The directory name for the course should be the course title, using
underscores where blanks would be expected. We are using "<language>_essentials"
for the three languages we support (Ada, SPARK, Rust) and then either the tool
name (e.g. "gnatsas") or a brief description (e.g. "gnat_project_facility")
for any other course.

---------
Modules
---------

A module is a self-contained unit within the course, focusing on a
specific concept or set of related concepts. Each module is a complete
topic in itself and typically contains several chapters.

For each module in the course, the course folder should contain:

  * Base module RST file that lists the chapters typically taught
    in the module
  * Folder with the same name as the RST file that contains
    the chapter RST files

The module naming scheme uses a 3-digit prefix followed by the module
name. This prefix should indicate the preferred order for teaching the
module. A simple example would be

   ``005_introduction.rst`` Introduction module file, containing a list of chapters

   ``005_introduction`` Introduction module folder, containing an RST file for each chapter

   ``010_overview.rst`` Overview module file, containing a list of chapters

   ``010_overview`` Overview module folder, containing an RST file for each chapter

If there is need for multiple versions of the module (introduction, in-depth),
the chapters will be stored in the module folder, but there will be an
additional module file using the prefix and module name, and then a
hyphen to describe the change from the base module.

   ``170_tagged_derivation-intro.rst``
   
   ``195_exceptions-in_depth.rst``

----------
Chapters
----------

A chapter is a focused section within a module that delves into a
specific sub-topic or a particular aspect of the module's broader
concept. Chapters are designed to present information in a logical,
step-by-step manner, often building upon preceding chapters within the
same module to contribute to a complete understanding of the overall
module topic. Think of chapters as individual lessons or specific
themes that collectively form a comprehensive module.

Each chapter will be in its own RST file in the module folder.

The chapter name should start with a two digit number indicating its
typical order of presentation. Sometimes, there are two versions of a
chapter; they should have the same number and base name, with something
added to describe the differences. Not all chapters will be used by
every module, but there should never be two versions of the same chapter
in a module.

To simplify renaming if we decide to reorder chapters within a module,
the summary chapter will always be **99-summary[-version].rst** and
the lab will always be **88-<module_name>[-version].lab.rst**. Examples:

  * 88-expressions-with_quantified.lab.rst
  * 88-expressions.lab.rst
  * 99-summary-with_quantified.rst
  * 99-summary.rst

=============
Style Rules
=============

---------------
Bullet Points
---------------

Bullets should always be used when a slide has multiple points to be made.

If a slide has a single point, no bullet should be added.

If a slide has sub-points, the sub-points should be nested bullets (regardless
of how many top-level points there are). If there is one top-level point and
one sub-point - rework the slide!

-----------------------
Module / Slide Titles
-----------------------

Slide titles are surrounded (via line above and below) by a line of "-" characters. 
The "-" characters should extend 2 characters past the end of the title string.
Example:

    ``-----------------``

    ``This is a Title``

    ``-----------------``

---------------
Code Examples
---------------

Code examples should use reasonble identifiers (as opposed to a single letter).
Where reasonable, the names should be descriptive of the situation being explained.

*Bad snippet*

   .. code:: Ada

      type OneD_T is array (Index_T) of Boolean;
      B : OneD_T;

*Better snippet*

   .. code:: Ada

      type One_Dimension_Array_T is array (Index_T) of Boolean;
      One_Dimension : One_Dimension_Array_T;

Obviously, longer names may make the text harder to read on a slide. Try to
balance expressiveness with brevity, and think about reformatting the code
example as well.

-----------------
Confusing Terms
-----------------

* runtime / run-time / run time

   * **runtime** is basically the operating system
   * **run-time** means during execution
   * **run time** is how long the executable took

----------------
Spacing Issues
----------------

A good way to create a blank line is to write some text in white.

:color-white:`This line will not appear in a PDF file`

*If you see a blank space above, look at the original source!*
