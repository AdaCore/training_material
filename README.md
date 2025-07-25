![Training Material Build](https://github.com/adacore/training_material/workflows/CI/badge.svg)

# Overview

This repository is a collection of courses for teaching Ada (or SPARK) plus some
support files to convert the courses into various presentation formats. The
courses are written in ReStructured Text (RST) format, making them easy to
convert into word processing documents, slides, or any other formats as needed.
The most common conversion tool is Pandoc, and there is a folder that contains
some artifacts that can be used with the tool. (Artifacts for other tools
may be added in the future) Each of the folders at this level are described
below.

# Download

You can download the material PDFs by going to [the actions page](http://github.com/adacore/training_material/actions).

**NB: You need to be logged in to GitHub.**

Click on the build you want. You probably want to take one from the **master** branch.

![Click build](images/github/artifacts_1.png)

Scroll to the bottom to locate the **Artifacts**

![Bottom scroll](images/github/artifacts_2.png)

Select the **Artifact** associated with the course you want ("standard" courses are "Ada Essentials - Standard Course" and "SPARK - Course," for example).

![Artifact examples](images/github/artifacts_3.png)

# Content

## Courses

Each folder contains a collection of RST files that make up modules within
the course. The file names for the modules should use a numbering scheme so
users can quickly understand the typical order of presentation. 

**Note: RST files under this folder should follow the guidelines set out
in the** *style_guide.rst* **file at the top-level of the repository**

### Module Labs

If the course is designed with laboratory exercises / examples, these RST
files should be in a subfolder, where the name of the file is the same as the
course name with ".lab" at the end. The benefit of keeping labs separate from
their enclosing courses is to be able to generate "Lab Manuals" as separate
course materials. You can also create subfolders here to store lab answers
and supporting files.

## Images

All images used in any of the courses should be stored here. This allows
sharing of images across courses without duplication. The filename should be
as descriptive as possible.

## Pandoc

This folder contains artifacts useful in running Pandoc to generate any output
format.

## Support Files

This folder contains files that may be useful for multiple presentation formats.

## Contrib

This folder contains scripts that are used for generating the docs or packages.

# Testing

This folder contains some automated tests for the various tools that we use. You can run them by typing the following into the terminal/command prompt:

```
pytest
```

# CI-Specific

## Building a given course only

You can specify a single course (or course directory) to build by adding the following
file to your directory:

`DO_NOT_MERGE.SINGLE_COURSE`

This should contain the path to the course directory or txt file to build.

This should **not** be merged back to master.

## Mirrored (Protected) branches

Branches that have names that follow the globs

- `mr/*`
- `training/*`
- `experimental/*`
- `slides/*`
- `build/*`

Will be **protected** and **mirrored**, so they will be built by the CI if you manually
[create a Pull Request](https://github.com/AdaCore/training_material/compare) for the GitHub branch.

NB: MR created through GitLab "Create Merge Request" button are automatically named `mr/nnn-slug` so
they are mirrored automatically.

# Building Locally

PDFs of the course material can be generated locally (on your machine). To do this, you'll need to configure your environment appropriately, then execute the build command.

## Windows Setup

**Note:** Use **Command Prompt** (as opposed to PowerShell), as paths will be configured correctly by default with the following installers.

1. Install latest version of Pandoc (https://pandoc.org/)

2. Install latest version of Miktext (https://miktex.org/)
    * Set “Preferred Paper” size as “Letter”

3. Install latest version of Python (available thru Microsoft Store)
    * Verified to work with: 3.12

4. Install pandocfilters for python:
    * `pip install pandocfilters`

5. Install [rsvg-convert](https://community.chocolatey.org/packages/rsvg-convert) 
    * Ensure this is added to your path

**Note:** _Images generated locally may not match those generated by the CI exactly. Always verify graphic changes within the target slides generated by the CI._

## Building Slide Content

The following command will generate a PDF for a specific slide:

```
python [LOCAL_PATH]\training_material\pandoc\pandoc_fe.py --source [FILENAME] --output [OUTPUT_FILE] --extension pdf --directories [LOCAL_PATH]\images,[LOCAL_PATH]\training_material\support_files  --theme adacore --color adacore  --filter [LOCAL_PATH]\training_material\pandoc\beamer_filter.py
```

In the above command:

* **[LOCAL_PATH]** should be replaced with the full path containing the “training_material” source folder

* **[FILENAME]** should be replaced with the path to either of the following:

   * Name of the .rst file containing the source to generate the PDF file
   * Name of a .txt file containing a list of RST files (one per line) for which a single PDF file will be generated

   The filename needs to be a path (for Pandoc) so, if it's local, prepend it with "./" or ".\\" (depending on your OS).

   For example, to build the standard course (in the repository as "standard_course.txt"), use **--source ./standard_course.txt**.
   If you want a PDF of just the polymorphism module, use **--source ./180_polymorphism.rst**. You can also create your own
   .txt file to build multiple modules at once (just don't check it in!).

* **[OUTPUT_FILE]** should be replaced with the desired name of the generated PDF (minus the file extension)

### First Time Run

Upon running the script for the first time, a **Package Installation** window may appear, prompting for installation of various packages for specific files. Don't be alarmed...just click through to **Install** each one, as they are required to generate the slide content.
