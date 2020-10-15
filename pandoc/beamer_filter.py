'''
Python filter for generating 'beamer' output from Pandoc

Special handling done by this filter:
   + Role translations
      - toolname => small caps
      - menu => colored box with white text
      - command => black box with monospaced white text
      - filename => bold italic
   + Search TEXINPUTS environment variable for paths to find images
   + Slides are vertically aligned to the top, with the 'shrink' attribute
     so everything fits one the page
   + Bullet lists are forced to appear all at once (Pandoc default is
     one bullet at a time)
   + Container 'speakernote' will be treated as a beamer 'note'
   + Admonition 'language variant' will add a subtitle to the slide
     with a boxed text of the variant (useful for adding things like
     "Ada 2012" to a slide that has Ada 2012-specific code)
'''

import os
import sys

import pandocfilters
from pandocfilters import toJSONFilter, Strong, Str, SmallCaps, Emph, Para

#############################################################################
## CONFIGURATION INFORMATION HERE
##

# If debug_file is not an empty string, exceptions will be writte to the file
if sys.platform.startswith ('win'):
    debug_file = "c:\\temp\\pandoc\\output.txt"
else:
    import tempfile
    debug_file = tempfile.mkstemp (prefix="beamerfilter-")[1]

# Control wether sub-bullets appear one at a time in a 'beamer' presentation
# (False indicates everything appears at once)
bullet_point_animation = False

# Decorators to apply to slide frames in beamer (except title slides)
# Typical decorators are 't' for top-alignment, and 'shrink' for shrink-to-fit
slide_decorators = [ 't', 'shrink' ]


# This dictionary defines the function (dictionary value) that should
# be called to provide the formatting for an RST 'role' (dictionary key).
# If the function name is found in 'pandocfilters', the caller must supply
# the parameter as an AST string node.
# Otherwise (for local functions), the parameter will be a literal text string
role_format_functions = { 'toolname' : 'SmallCaps',
                          'menu'     : 'format_menu',
                          'command'  : 'format_command',
                          'filename' : 'format_filename',
                          'default'  : 'Strong' }
##
## END CONFIGURATION INFORMATION
#############################################################################

'''
If the debug file is specified, append 'text' to the end of the file
'''
def debug ( text ):
   global debug_file
   if len(debug_file) > 0:
      with open(debug_file, "a") as myfile:
         myfile.write ( text + "\n" )

'''
Convert an AST paragraph node to a literal text string
'''
def para_to_text ( content_list ):
    ret_val = ""
    for c in content_list:
        if c['t'] == 'Str':
            ret_val = ret_val + c['c']
        elif c['t'] == 'Space':
            ret_val = ret_val + ' '
        else:
            debug ( " *** Don't understand: " + str(c) )
    return ret_val

############################
## LATEX HELPER FUNCTIONS ##
############################
def latex_block(s):
   return pandocfilters.RawBlock('latex', s)

def latex_inline(s):
   return pandocfilters.RawInline('latex', s)

def latex_box ( text, color='adacore2' ):
    return "\\colorbox{" + color + "}{" + text + "}"

def latex_color ( text, color='white' ):
    return "\\textcolor{" + color + "}{" + text + "}"

def latex_bold_italic ( text ):
    return "\\textbf{\\textit{" + text + "}}"

def latex_monospace ( text ):
    return "\\texttt{" + text + "}"

def latex_escape ( text ):
    return text.replace('_', '\\_' ).replace('&', '\\&')

#############################
## PANDOC HELPER FUNCTIONS ##
#############################
def Space():
    ret_val = {}
    ret_val['t'] = 'Space'
    return ret_val

# convert a text string to an AST list
def literal_to_AST_node ( text ):
    ret_val = []
    pieces = text.split ( ' ' )
    for piece in pieces:
        ret_val.append ( Str(piece) )
        ret_val.append ( Space() )
    return ret_val[:-1]

'''
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
'''
def modify_header ( value ):
   global slide_decorators
   # If all the fields are there and the decorators are ready
   if len(value) == 3 and len(value[1]) > 2:
      for decorator in slide_decorators:
         value[1][1].append ( decorator )
   return None

'''
BlockQuote forces bullet lists to appear one bullet at a time.
Returning 'value' effectively strips BlockQuote from the AST
'''
def bullet_point_fix ( value ):
    global bullet_point_animation

    if not bullet_point_animation:
       return value
    else:
       return None

'''
PANDOC does not like using 'TEXINPUTS' to find image files,
so we will do it here.
For an inserted image, 'value' is a triplet whose 3rd element is
a doublet, the first element of which is the path to the file.
We will first look at the filename and see if it resolves itself.
If not, we will look in each of the directories specified by TEXINPUTS
to find the file (EVEN IF WE ARE NOT GENERATING TEX/PDF!)
'''
def find_file ( filename ):
   try:
      if os.path.isfile ( filename ):
         return filename
      else:
         paths = os.environ['TEXINPUTS']
         # For linux, try separating paths by ':' first
         path_list = []
         if not sys.platform.startswith ( 'win' ):
            path_list = paths.split(':')
         path_list = ( paths.split(';') )
         # try combining full specified filename with path
         for path in path_list:
            attempt = os.path.join ( path, filename )
            if os.path.isfile ( attempt ):
               return attempt
         # try combining just filename with path
         just_filename = os.path.basename ( filename )
         for path in path_list:
            attempt = os.path.join ( path, just_filename )
            if os.path.isfile ( attempt ):
               return attempt
      return filename
   except Exception as e:
      debug ( "find_file EXCEPTION: " + str(e) )
      return filename

##########################
## CONVERSION FUNCTIONS ##
##########################

def speaker_note ( contents ):
   return ( [latex_block('\\note{')] + contents + [latex_block('}')] )

def language_variant_admonition ( contents ):
   text = para_to_text ( contents[1]['c'] )
   return ( [latex_block('\\framesubtitle{\\rightline{' +
                   latex_box(text) +
                   '\\hspace{1cm}}}')] )

#####################
## QUERY FUNCTIONS ##
#####################

# Return the type of the admonition
def admonition_type ( classes, contents ):
   try:
      if "admonition" in classes:
         if len(contents) == 2:
            if contents[0]['t'] == 'Para' and contents[1]['t'] == 'Para':
               type = para_to_text ( contents[0]['c'] )
               return type.lower()
      return ""
   except Exception as e:
      debug ( "admonition_type EXCEPTION: " + str(e) )
      return ""

# Look at information in AST to see if this is a speaker note
def is_speakernote ( classes ):
   return ( "container" in classes ) and ( "speakernote" in classes)

#####################
## TEXT FORMATTING ##
#####################
'''
In RST, interpreted text is text that is enclosed in single back-ticks (`).
In Pandoc's internal representation, if the interpreted text has a role specifed,
then the AST node has a key of "Code", the role is part of the value as specified
by the indicator "interpreted-text", and the text is a single literal.
Otherwise, (for a default role), the AST node has
a key of "Span", the indicator is "title-ref', and the text is an AST text node.
'''
def format_text ( key, value, format ):
   [[ident, classes, kvs], text] = value

   if key == "Span" and 'title-ref' in classes:
      return pandoc_format ( 'default', text )
   elif ( key == "Code" and
          'interpreted-text' in classes and
          kvs[0][0] == 'role'):
      try:
         return perform_role ( kvs[0][1], text, format )
      except Exception as e:
         return pandoc_format ( 'default', literal_to_AST_node ( text ) )

'''
pandoc_format takes the name of a pandoc emphasis function and
an AST string node and calls the function with the node.
If the function doesn't exist, we will default to Strong
'''
def pandoc_format ( function_name, ast_string_node ):
   global role_format_functions
   function_name = role_format_functions[function_name]
   try:
      return globals()[function_name]( ast_string_node )
   except:
      return Strong ( ast_string_node )

'''
If the role is a function defined in the pandocfilters module, we will
convert the literal text to an AST string node and call the function.
If not, we will assume the function is defined locally and pass in the
literal text.
'''
def perform_role ( role, literal_text, format ):
   global role_format_functions
   function_name = role_format_functions[role]
   try:
      if function_name in dir(pandocfilters):
         return globals()[function_name] ( literal_to_AST_node ( literal_text ) )
      elif format == 'beamer':
         return globals()[function_name] ( literal_text )
      else:
         return globals()[function_name] ( literal_to_AST_node ( literal_text ) )
   except Exception as e:
      debug ( "perform_role EXCEPTION: " + str(e) )

'''
"menu" role
(items that would appear in a GUI menu)
'''
def format_menu ( literal_text ):
   # white text on box of color
   return latex_inline ( latex_box ( latex_color ( latex_escape ( literal_text ) ) ) )

'''
"command" role
(items that indicate user-typed commands)
'''
def format_command ( literal_text ):
   # white text on box of black
   return latex_inline ( latex_box ( latex_color ( latex_monospace ( latex_escape ( literal_text ) ) ), "black" ) )

'''
"filename" role
(items that indicate a particular filename/folder)
'''
def format_filename ( literal_text ):
   # bold and italic
   return latex_inline ( latex_bold_italic ( latex_escape ( text ) ) )

#####################
## MAIN SUBPROGRAM ##
#####################

def perform_filter(key, value, format, meta):

   try:

      # For an inserted image, 'value' is a triplet whose 3rd element is
      # a doublet, the first element of which is the path to the file.
      if key == "Image":
         value[2][0] = find_file ( value[2][0] )

      # Common manipulations
      elif key == "Code" or key == "Span":
         return format_text ( key, value, format )

      ## Beamer-specific manipulations
      elif format == "beamer":
         if key == "BlockQuote":
             return bullet_point_fix ( value )

         elif key == "Header":
            modify_header ( value )

         # Div is used when Pandoc finds a container
         # If it is a container, handle the containers that we care about
         # looking like [<some string>, ['container', '<container name>'], [<some tuple]]
         elif key == "Div":

            [[ident, classes, kvs], contents] = value

            if is_speakernote ( classes ):
                return speaker_note ( contents )

            # language variant admonition
            elif admonition_type ( classes, contents ) == "language variant":
               return language_variant_admonition ( contents )

   except Exception as e:
      debug ( "perform_filter EXCEPTION: " + str(e) )
      pass

if __name__ == "__main__":
  toJSONFilter(perform_filter)

