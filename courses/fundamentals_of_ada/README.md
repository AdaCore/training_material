# Overview

This folder is a collection of modules for teaching Ada. Each module
is an RST file that may include files from the *labs* folder or *examples*
folder.

The file **standard_course.txt** contains a list of all the modules that
we expect to find in a typical "Fundamentals of Ada" course. If you need
to modify the list of included modules, this is the file you would change
(but don't check it into MASTER!)

## Supplementary Labs

Supplementary Ada labs are also available through Alire, with the crates

- [`labs_radar`](https://alire.ada.dev/crates/labs_radar)
- [`labs_solar_system`](https://alire.ada.dev/crates/labs_solar_system)
- [`labs_standalone`](https://alire.ada.dev/crates/labs_standalone)

## Naming Scheme

The naming scheme uses a 3-digit prefix, followed by an underscore and
then the description of the module (all lower case, words
separated by "\_").

When the filename starts with the 3-digit prefix, this prefix specifies
the expected order of teaching the modules.

Files that are prefaced by "intro_" are introductory courses. When the prefix
matches a fundamental course prefix, then the introductory topic is a lightweight
version the fundamental course (or a portion thereof).
When the prefix does not match a fundamental course prefix, then the
course is just an introduction to an Ada topic that was not covered in
the fundamental course.

Files that are prefaced by "adv_" are advanced courses. When the prefix
matches a fundamental course prefix, then the advanced topic is an
in-depth version the fundamental course (or a portion thereof).
When the prefix does not match a fundamental course prefix, then the
advanced topic is just an advanced Ada topic that was not covered in
the fundamental course.

Files that are prefaced by "spec_" are courses that were created at
the request of a customer, and may or may not be useful to other customers.

### Prefix Grouping

The numeric value of the file prefix should be grouped as follows:

#### Prefixes 900 - 999

These prefixes should indicate reference materials. Typically, these modules would be a
way to give students a central location for finding answers to simple Ada questions.

#### Prefixes 890 and below

These prefixes should be used by the "spec_" courses. New modules should use the
highest available number (leaving everything below for normal courses)

