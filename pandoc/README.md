# Overview

This repository is a collection of files for using Pandoc to generate
presentations from the course modules (see www.pandoc.org for a full
description of Pandoc).
All course modules should be syntactically correct Restructured Text.
With that, you can use Pandoc to convert the RST files into any format
you want. Pandoc allows the creation of filters to customize how you
want certain constructs created. Those filters should be stored in
this directory.
Files in this directory should be described below

## beamer_filter.py

This is a Pandoc filter used to customize output for generating 
output using the 'beamer' package of LaTex. It can be used for
other outputs as well ('LaTex' output will only be used if the
specified format is 'beamer'), as it does a decent job of finding
the images needed for the courses.
Behavior of the filter should be documented in the file itself!

## pandoc_fe.py

This is a simple python front end to Pandoc that makes it a little easier
to use Pandoc with the structure set up in this repository. Because
it is only dealing with a subset of the Pandoc options, the help is
easier to decipher. When it runs Pandoc, it echoes the command
being run, so you can use that as a starting point when generating
your own Pandoc command.

## Lab docs

These files are used to generate Labs PDF from RST files with the proper
adacore styling and settings. Can probably be adapted for more usage.
Doc template taken from the Eisvogel template: https://github.com/Wandmalfarbe/pandoc-latex-template
