#!/usr/bin/env python3
"""
Python filter for generating 'beamer' output from Pandoc

Special handling done by this filter:
   + Role translations
      - toolname => small caps
      - menu => colored box with white text
      - command => black box with monospaced white text
      - filename => bold monospaced on light yellow background
   + Search TEXINPUTS environment variable for paths to find images
   + Slides are vertically aligned to the top, with the 'shrink' attribute
     so everything fits one the page
   + Bullet lists are forced to appear all at once (Pandoc default is
     one bullet at a time)
   + Container 'speakernote' will be treated as a beamer 'note'
   + Admonition 'language variant' will add a subtitle to the slide
     with a boxed text of the variant (useful for adding things like
     "Ada 2012" to a slide that has Ada 2012-specific code)
"""

import os
import sys

import pandocfilters
from pandocfilters import toJSONFilter, Strong, Str, SmallCaps, Emph, Para

#############################################################################
## CONFIGURATION INFORMATION HERE
##

# Control wether sub-bullets appear one at a time in a 'beamer' presentation
# (False indicates everything appears at once)
bullet_point_animation = False

# Decorators to apply to slide frames in beamer (except title slides)
# Typical decorators are 't' for top-alignment, and 'shrink' for shrink-to-fit
slide_decorators = ["t", "shrink"]


# This dictionary defines the function (dictionary value) that should
# be called to provide the formatting for an RST 'role' (dictionary key).
# If the function name is found in 'pandocfilters', the caller must supply
# the parameter as an AST string node.
# Otherwise (for local functions), the parameter will be a literal text string
role_format_functions = {
    "toolname": "SmallCaps",
    "url": "format_url",
    "menu": "format_menu",
    "command": "format_command",
    "dfn": "format_dfn",
    "answer": "format_answer",
    "answermono": "format_answermono",
    "animate": "format_animate",
    "filename": "format_filename",
    "default": "Strong",
}
##
## END CONFIGURATION INFORMATION
#############################################################################


class FilterException(Exception):
    pass


def fail(s):
    raise FilterException(s)


"""
Convert an AST paragraph node to a literal text string
"""


def para_to_text(content_list):
    ret_val = ""
    for c in content_list:
        if c["t"] == "Str":
            ret_val = ret_val + c["c"]
        elif c["t"] == "Space":
            ret_val = ret_val + " "
        else:
            fail(" *** Don't understand: " + str(c))
    return ret_val


############################
## LATEX HELPER FUNCTIONS ##
############################
def latex_block(s):
    return pandocfilters.RawBlock("latex", s)


def latex_inline(s):
    return pandocfilters.RawInline("latex", s)


def latex_box(text, color="adacore2"):
    return "\\colorbox{" + color + "}{" + text + "}"


def latex_color(text, color="white"):
    return "\\textcolor{" + color + "}{" + text + "}"


def latex_bold(text):
    return "\\textbf{" + text + "}"


def latex_italic(text):
    return "\\textit{" + text + "}"


def latex_bold_italic(text):
    return "\\textbf{\\textit{" + text + "}}"


def latex_monospace(text):
    return "\\texttt{" + text + "}"


def latex_escape(text):
    # For "--": https://tex.stackexchange.com/questions/9813/how-can-i-stop-latex-from-converting-two-hyphens-to-a-single-hyphen-when-loading
    return (
        text.replace("_", "\\_")
        .replace("&", "\\&")
        .replace("#", "\\#")
        .replace("--", "-{}-")
    )


def latex_answer(text):
    return "\\textit<2>{\\textbf<2>{\\textcolor<2>{green!65!black}{" + text + "}}}"


def latex_monoconvert(text):
    # Taken from Pandoc's stringToLatex
    replacements = [
        {
            "{": "\\{",
            "}": "\\}",
        },
        {
            "'": "\\textquotesingle{}",
            "`": "\\textasciigrave{}",
            "&": "\\&",
            "<": "\\textless{}",
            ">": "\\textgreater{}",
            " ": "\\ ",
        },
    ]

    for rs in replacements:
        r = ""
        for c in text:
            r += rs.get(c, c)
        text = r

    return r


def latex_answermono(text):
    return latex_monospace(latex_answer(latex_monoconvert(text)))


def latex_animate(text):
    return "\\onslide<2->{" + text + "}"


#############################
## PANDOC HELPER FUNCTIONS ##
#############################
def Space():
    ret_val = {}
    ret_val["t"] = "Space"
    return ret_val


# convert a text string to an AST list
def literal_to_AST_node(text):
    ret_val = []
    pieces = text.split(" ")
    for piece in pieces:
        ret_val.append(Str(piece))
        ret_val.append(Space())
    return ret_val[:-1]


"""
A header is a triplet consisting of
   Header level
   Header attributes
   Content
This subprogram will add the specified LaTeX attributes (decorators)
to the slide title. Typically, these are strings like 't' to force
vertical alignment to the top, and 'shrink' to make sure the whole
slide fits on one page
NOTE: For a single input file, slide title is level 2 - for multiple input
files, slide title is level 3. For now, I'll put the decorators on
both levels!
To do that, we need to modify the header attributes.
Header attributes is a triplet consisting of
   Hyperlink name
   Special decorators
   Something else
We need to put our specified decorators into the 'special decorators' location
"""


def modify_header(value):
    global slide_decorators
    # If all the fields are there and the decorators are ready
    if len(value) == 3 and len(value[1]) > 2:
        for decorator in slide_decorators:
            value[1][1].append(decorator)
    return None


"""
BlockQuote forces bullet lists to appear one bullet at a time.
Returning 'value' effectively strips BlockQuote from the AST
"""


def bullet_point_fix(value):
    global bullet_point_animation

    if not bullet_point_animation:
        return value
    else:
        return None


"""
PANDOC does not like using 'TEXINPUTS' to find image files,
so we will do it here.
For an inserted image, 'value' is a triplet whose 3rd element is
a doublet, the first element of which is the path to the file.
We will first look at the filename and see if it resolves itself.
If not, we will look in each of the directories specified by TEXINPUTS
to find the file (EVEN IF WE ARE NOT GENERATING TEX/PDF!)
"""


def find_file(filename):
    if os.path.isfile(filename):
        return filename
    else:
        paths = os.environ["TEXINPUTS"]
        # For linux, try separating paths by ':' first
        path_list = []
        if not sys.platform.startswith("win"):
            path_list = paths.split(":")
        path_list = paths.split(";")
        # try combining full specified filename with path
        for path in path_list:
            attempt = os.path.join(path, filename)
            if os.path.isfile(attempt):
                return attempt
        # try combining just filename with path
        just_filename = os.path.basename(filename)
        for path in path_list:
            attempt = os.path.join(path, just_filename)
            if os.path.isfile(attempt):
                return attempt
    return filename


##########################
## CONVERSION FUNCTIONS ##
##########################


def speaker_note(contents):
    return [latex_block("\\note{")] + contents + [latex_block("}")]


def language_variant_admonition(contents):
    text = para_to_text(contents[1]["c"])
    return [
        latex_block(
            "\\framesubtitle{\\rightline{" + latex_box(text) + "\\hspace{1cm}}}"
        )
    ]


###################################
## INCLUDE SOURCE CODE FROM FILE ##
###################################

"""
   RST "include" directive allows the inclusion of a snippet of an
   external file, and can format it as a block of code.
   (https://docutils.sourceforge.io/docs/ref/rst/directives.html#including-an-external-document-fragment)

   HOWEVER, Pandoc does not support it!

   So these subprograms allow us to simulate it using a "container" directive.
   The format of the directive is:

      .. container:: source_include <path-to-file> [option [option ...]]

   Options follow the format specified in the above link. As of now, the only options 
   supported are
      :start-after:<string>
         insert code starting at the line after the first occurrence of <string>
      :end-before:<string>
         stop inserting code at the line before the first occurrence of <string>
         (if "start-after" is specified, only look for <string> after starting)
      :code:<language>
         Language to format code insertion
      :number-lines:<number>
         Add line numbers starting at <number>
"""


def source_file_contents(filename, keywords):
    retval = ""

    start_after = ""
    end_before = ""
    echo_on = False

    # if we're looking for a string before starting, save the string
    if "start-after" in keywords.keys():
        start_after = keywords["start-after"]
    # otherwise, we start by echoing the file
    else:
        echo_on = True

    if "end-before" in keywords.keys():
        end_before = keywords["end-before"]

    if os.path.isfile(filename):
        with open(filename, "r") as the_file:
            for line in the_file:
                # if we're not echoing, then look for the starting text
                if not echo_on:
                    if len(start_after) > 0 and start_after in line:
                        echo_on = True
                # if we are echoing and we find the ending text, we're done
                elif len(end_before) > 0 and end_before in line:
                    break
                # otherwise add this to the return value
                else:
                    retval = retval + line
        return retval
    else:
        return filename


SUPPORTED_CLASSES = [
    "container",
    "source_include",
    "admonition",
    "animate",
    "speakernote",
    "columns",
    "column",
    "latex_environment",
    "footnotesize",
]


def source_include(classes, contents):
    # useful for debugging
    filename = str(classes)
    keywords = {}
    keywords["code"] = "Ada"

    for item in classes:
        if os.path.isfile(item):
            filename = item
        else:
            # keywords are in format ":<keyword>:value"
            pieces = item.split(":")
            if len(pieces) == 3:
                keywords[pieces[1]] = pieces[2]

    code = [keywords["code"]]
    startfrom = []
    if "number-lines" in keywords.keys():
        code.append("numberLines")
        startfrom = [["startFrom", keywords["number-lines"]]]

    value0 = {}
    value0["t"] = "CodeBlock"
    value0["c"] = [
        ["", code, startfrom],
        source_file_contents(filename, keywords),
    ]
    value = [value0]

    return value


def is_source_include(classes):
    return ("container" in classes) and ("source_include" in classes)


###############
## ANIMATION ##
###############

"""
   We are going to use a container to "animate" blocks of code.
   The format of the directive is:

      .. container:: animate [<slide #>[-]]
      
   Slide number is the overlay(s) to display the contents.
   A number will appear on the specific overlay.
   A number followed by a "-" will appear on the specific overlay an all subsequent.
   So, in pseudocode:
      AAA
      animate 2
         BBB
      animate 3-
         CCC
      animate 4-
         DDD
   will cause the following 4 overlays: AAA, AAA BBB, AAA CCC, AAA CCC DDD
   If <slide #> is not specified, it will default to 2-.
   NOTE: We use "visibleenv" to make text appear, so space is reserved for hidden
   text. If not, then the slide may resize, causing the animation to not really
   look like an animation
"""


def is_animate(classes):
    return ("container" in classes) and ("animate" in classes)


def animate(classes, contents):
    slide_number = 2
    dash = "-"
    if len(classes) > 2:
        requested = classes[2]
        if len(requested) > 0:
            if requested[-1] == "-":
                requested = requested[:-1]
            else:
                dash = ""
            slide_number = int(requested)
    slide_number = str(slide_number) + dash

    first = {
        "t": "RawBlock",
        "c": ["latex", "\\begin{visibleenv}<" + slide_number + ">"],
    }
    last = {"t": "RawBlock", "c": ["latex", "\\end{visibleenv}"]}

    value = []
    value.append(first)
    for c in contents:
        value.append(c)
    value.append(last)

    return value


########################
## LATEX ENVIRONMENTS ##
########################

"""
   This is a highly flexible way of adding LaTeX capabilities
   into an RST document. I found it useful for changing text
   sizes when I knew I needed it.

   The format of the directive is:

      .. container:: latex_environment <environment-name> [options]
      
   It will add "\begin{environment-name}options" at the beginning of 
   the container block, and "\end{environment-name}" at the end.
   No guarantees as to safety - if Pandoc has a same-named begin and/or end
   inside the container, I have no idea what will happen.
"""


def is_latex_environment(classes):
    return ("container" in classes) and ("latex_environment" in classes)


def latex_environment(classes, contents):
    if len(classes) > 2:
        environment = classes[2]
        begin = "\\begin{" + environment + "}"
        if len(classes) > 3:
            for option in classes[3:]:
                begin = begin + " " + option

        first = {"t": "RawBlock", "c": ["latex", begin]}
        last = {"t": "RawBlock", "c": ["latex", "\\end{" + environment + "}"]}

        value = []
        value.append(first)
        for c in contents:
            value.append(c)
        value.append(last)
        return value

    else:
        return contents


#####################
## QUERY FUNCTIONS ##
#####################


# Return the type of the admonition
def admonition_type(classes, contents):
    if "admonition" in classes:
        if len(contents) == 2:
            if contents[0]["t"] == "Para" and contents[1]["t"] == "Para":
                type = para_to_text(contents[0]["c"])
                return type.lower()
    return ""


# Look at information in AST to see if this is a speaker note
def is_speakernote(classes):
    return ("container" in classes) and ("speakernote" in classes)


#####################
## TEXT FORMATTING ##
#####################
"""
In RST, interpreted text is text that is enclosed in single back-ticks (`).
In Pandoc's internal representation, if the interpreted text has a role specifed,
then the AST node has a key of "Code", the role is part of the value as specified
by the indicator "interpreted-text", and the text is a single literal.
Otherwise, (for a default role), the AST node has
a key of "Span", the indicator is "title-ref', and the text is an AST text node.
"""


def format_text(key, value, format):
    [[ident, classes, kvs], text] = value

    if key == "Span" and "title-ref" in classes:
        return pandoc_format("default", text)
    elif key == "Code" and "interpreted-text" in classes and kvs[0][0] == "role":
        res = perform_role(kvs[0][1], text, format)
        if res == None:
            # Fallback returns default
            res = pandoc_format("default", literal_to_AST_node(text))
        return res


"""
pandoc_format takes the name of a pandoc emphasis function and
an AST string node and calls the function with the node.
If the function doesn't exist, we will default to Strong
"""


def pandoc_format(function_name, ast_string_node):
    function_name = role_format_functions[function_name]
    return globals()[function_name](ast_string_node)


"""
If the role is a function defined in the pandocfilters module, we will
convert the literal text to an AST string node and call the function.
If not, we will assume the function is defined locally and pass in the
literal text.
"""


def perform_role(role, literal_text, format):
    function_name = role_format_functions.get(role, None)
    if function_name == None:
        return function_name
    elif function_name in dir(pandocfilters):
        return globals()[function_name](literal_to_AST_node(literal_text))
    else:
        return globals()[function_name](literal_text)


"""
"menu" role
(items that would appear in a GUI menu)
"""


def format_menu(literal_text):
    # white text on box of color
    return latex_inline(latex_box(latex_color(latex_escape(literal_text))))


"""
"url" role
Pretty-print URL
"""


def format_url(literal_text):
    # white text on box of color
    url = latex_escape(literal_text)

    # shrink based on length of actual (not escaped) text
    url_text = ""
    if len(literal_text) <= 60:
        url_text = "\\normalsize{" + url + "}"
    elif len(literal_text) <= 67:
        url_text = "\\small{" + url + "}"
    elif len(literal_text) <= 71:
        url_text = "\\footnotesize{" + url + "}"
    elif len(literal_text) <= 80:
        url_text = "\\scriptsize{" + url + "}"
    else:
        url_text = "\\tiny{" + url + "}"
    return latex_inline("{" + latex_box(latex_color(url_text, "adacore1")) + "}")


"""
"command" role
(items that indicate user-typed commands)
"""


def format_command(literal_text):
    # white text on box of black
    return latex_inline(
        latex_box(latex_color(latex_monospace(latex_escape(literal_text))), "black")
    )


"""
"dfn" role
Items that indicate a term definition
"""


def format_dfn(literal_text):
    return latex_inline(
        latex_box(latex_color(latex_italic(latex_escape(literal_text))), "cyan")
    )


"""
"filename" role
(items that indicate a particular filename/folder)
"""


def format_filename(literal_text):
    # bold monospaced on light yellow background
    return latex_inline(
        latex_box(
            latex_bold(latex_monospace(latex_escape(literal_text))), "lightyellow"
        )
    )


"""
"answer" role
Items will appear normal at first then highlighted on "page down".
Useful for quiz answers to appear after a quiz slide is presented.
"""


def format_answer(literal_text):
    return latex_inline(latex_answer(latex_escape(literal_text)))


def format_answermono(literal_text):
    return latex_inline(latex_answermono(latex_escape(literal_text)))


"""
"animate" role
Items will only appear on a slide after "page down".
Useful for explaining why a quiz answer is incorrect after a
quiz slide is presented.
"""


def format_animate(literal_text):
    return latex_inline(latex_animate(latex_escape(literal_text)))


#####################
## MAIN SUBPROGRAM ##
#####################


def perform_filter(key, value, format, meta):
    # For an inserted image, 'value' is a triplet whose 3rd element is
    # a doublet, the first element of which is the path to the file.
    if key == "Image":
        value[2][0] = find_file(value[2][0])

    # Common manipulations
    elif key == "Code" or key == "Span":
        return format_text(key, value, format)

    ## Beamer-specific manipulations
    elif format == "beamer":
        if key == "BlockQuote":
            return bullet_point_fix(value)

        elif key == "Header":
            modify_header(value)

        # Div is used when Pandoc finds a container
        # If it is a container, handle the containers that we care about
        # looking like [<some string>, ['container', '<container name>'], [<some tuple]]
        elif key == "Div":
            [[ident, classes, kvs], contents] = value

            assert all(
                c in SUPPORTED_CLASSES for c in classes[:2]
            ), f"unsupported: {', '.join(c for c in classes[:2] if c not in SUPPORTED_CLASSES)}"

            if is_speakernote(classes):
                return speaker_note(contents)

            if is_source_include(classes):
                return source_include(classes, contents)

            if is_animate(classes):
                return animate(classes, contents)

            if is_latex_environment(classes):
                return latex_environment(classes, contents)

            # language variant admonition
            elif admonition_type(classes, contents) == "language variant":
                return language_variant_admonition(contents)


if __name__ == "__main__":
    toJSONFilter(perform_filter)
