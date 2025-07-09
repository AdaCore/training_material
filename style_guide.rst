*******
Purpose
*******

This document serves as a collection of the policies and procedures
for writing ReStructured Text (RST) content for the courses offered
by the Field Engineering team.

As this document was created long after the original RST files
were created, we understand that most of the existing content does
not match this guide. But any new additions and modifications should
adhere to them.

***********
File Layout
***********

=======
Courses
=======

A course is stored in a directory in the **courses** folder of the
repository. The course directory contains all of the module information
and a **.txt** file that lists the modules in that course. 

There may be more than one **.txt** file to indicate multiple versions
of the course.

=======
Modules
=======

For each module in the course, the course folder should contain:

  * Module RST file that lists the chapters in the module
  * Folder with the same name as the RST file that contains
    the chapter RST files

For multiple versions of the module (e.g. "intro" and "in_depth"), the
filename would be the module name followed by "-" and then a description
of the version. Examples:

  * 240_tasking-in_depth.rst
  * 240_tasking-light.rst
  * 240_tasking-light_in_depth.rst
  * 240_tasking.rst

========
Chapters
========

Each chapter will be in its own RST file in the module folder.

The chapter name should start with a two digit number indicating its
typical order of presentation. Sometimes, there are two versions of a
chapter; they should have the same number and base name, with something
added to describe the differences. Not all chapters will be used by
every module, but there should never be two versions of the same chapter
in a module.

***********
Style Rules
***********

=============
Bullet Points
=============

Bullets should always be used when a slide has multiple points to be made.

If a slide has a single point, no bullet should be added.

If a slide has sub-points, the sub-points should be nested bullets (regardless
of how many top-level points there are). If there is one top-level point and
one sub-point - rework the slide!

=====================
Module / Slide Titles
=====================

There should be no in-line formatting in a slide title (e.g. emphasis, role, etc).
This is to make it easier when we extract titles for syllabi or the like.

Ada keywords in a slide title should be quoted - e.g. *Simple "case" Statements*
