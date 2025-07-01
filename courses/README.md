# Overview

This folder is a collection of courses for teaching SPARK/Ada. Each subfolder
is a collection of modules (written in RST). Although all modules are related
to the folder, it's possible a typical course may not include all the modules.
Each folder should have one file called "standard_course.txt" that contains a
list of the modules in a typical course presentation.
In addition, you can create other text files that combine modules from
multiple courses. This is useful for a combined class, or a review class,
or similar circumstances. Those files should be at this level.

Current courses are listed below. Each folder should have a "lab" folder 
containing any lab modules for the course (lab modules should have the
same name as their course module with ".lab" appended to the name).

## Maintenance

When adding a new course to this folder, there is some maintenance that needs
to be done to this repository in addition to creating the course directory.
(Assume *(course name)* is replaced with the name of the folder you are creating.)

### File **.github/workflows/main.yaml**

Add *(course name)* to the ``source`` object

### File **contrib/rst_files_with_prelude.txt**

Add ``courses/(course name)/*.rst`` to the end of the file

### File **courses/(course name)/course.toml**

This file must be created in the *(course name)* directory.
It provides the "pretty" name of the course. The content should just be
``name = "Course Name"``

## Course Overviews

### Ada Essentials

This is the basic introduction class to Ada (or SPARK). This assumes no
prior knowledge of the language. All modules should be written towards
the Ada language itself (although references to SPARK are useful
where appropriate).  If there is a need for a SPARK-specific module,
it can be added as needed, and a special TXT file can be created.

Duration: 5 days

### SPARK Essentials

This is a course designed to teach SPARK to students already familiar with
Ada. Some modules are overviews of newer Ada functions (Ada 2012 and beyond),
while most deal with actual SPARK programming and analysis

Duration: 5 days

### CodePeer

Introduction to CodePeer

Duration: 1 day

### GNAT Project Facility

Introduction to GPR file content and GPRbuild tool

Duration: 1 day

### GNAT Studio

Introduction to GNAT Studio. Directory also contains some "QuickStart" modules
that can be inserted into other courses to give users a crash course in GNAT Studio

Duration: 1/2 day

### GNATcheck

Introduction to the GNATcheck tool

Duration: 1/2 day

### Static Analysis via Compiler

Course on using compiler capabilities to improve code behavior and maintainability

  * Compiler Warnings
  * Style Checks
  * Compilation restrictions
  * Producing representation information

Duration: 1/2 day

### GNATSAS

Course on the GNAT Static Analysis Suite - GNAT Metric, GNATcheck, GNAT SAS
