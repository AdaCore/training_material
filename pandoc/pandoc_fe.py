import argparse
import os
import sys

def windows ():
    return sys.platform.startswith ( 'win' )

'''
When building a list of directories to search, we want them to be unique
(so we don't end up searching directories twice). For Windows, case
doesn't matter, so we are going to force it to lower case
'''
def fix_file_case ( filename ):
    if windows():
       return filename.lower()
    return filename

'''
Set the TEXINPUTS environment variable based on the directories specified
on the command line. These directories will contain things like images
and Beamer theme files
'''
def set_texinputs ( new_directories ):

    # initialize list of directories
    unique = list()

    # add user-specified directories to front of list
    paths = new_directories.split ( ',' )
    for path in paths:
       abspath = fix_file_case ( os.path.abspath ( path ) )
       if not abspath in unique:
          unique.append ( abspath )

    # add any previously existing directories
    current = ""
    if 'TEXINPUTS' in os.environ:
        current = os.environ['TEXINPUTS']

    # default separator (works for Windows and linux 'sh')
    separator = ';'
    # For linux, check if we're using ':' or ';' as a separator
    if not windows():
       # if we don't find a ";" separator, we will use ':'
       if not ';' in current:
          separator = ':'

    paths = current.split ( separator )
    for path in paths:
       abspath = fix_file_case ( os.path.abspath ( path ) )
       if not abspath in unique:
          unique.append ( abspath )

    os.environ['TEXINPUTS'] = separator.join ( unique )

'''
For PDF and TEX, we are producing slides, so use the 'beamer' format
For any other extension, assume the extension and the format are the same
'''
def output_format ( extension ):
    if extension == "pdf" or extension == "tex":
        return "beamer"
    else:
        return extension

'''
If the source file is an RST file, then send it to Pandoc
For any other type of file, assume it contains a list of
files to parse
NOTE: Pandoc treats multiple source files on the command
line DIFFERENTLY than all the source files combined as one!
(When multiple source files, each source file is its own section)
'''
def expand_source ( source_file ):
    if ".rst" in source_file.lower():
       return source_file
    else:
       ret_val = ""
       dirname = os.path.dirname ( source_file )
       # Read lines from source file
       with open ( source_file ) as sources:
          for source in sources:
             # Generate full path
             path = os.path.abspath ( os.path.join ( dirname, source.strip() ) )
             # If file does not exist, say so
             if not os.path.isfile ( path ):
                print ( path + " not found - skipping" )
             else:
                # If there are spaces in the path, enclose path in quotes
                if ' ' in path:
                   path = '"' + path + '"'
                ret_val = ret_val + path + ' '
       return ret_val

if __name__== "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument('--source',
                        help='Source RST file OR text file with list of RST files',
                        required=True)

    parser.add_argument('--extension',
                        help='Output file extension. "PDF" => Beamer, "TEX" => LaTeX, "DOCX" => Word, "PPTX" => PowerPoint',
                        default='pdf',
                        required=False)

    parser.add_argument('--directories',
                        help='Comma-separated list of folders to search for things like images and Beamer themes',
                        default='.',
                        required=False)

    parser.add_argument('--title',
                        help='Document title and name of output file. If not specified, output file will be source filename with specified extension.',
                        required=False)

    parser.add_argument('--theme',
                        help='Beamer theme',
                        required=False)

    parser.add_argument('--color',
                        help='Beamer color theme',
                        required=False)

    parser.add_argument('--filter',
                        help='Pandoc filter to do special processing',
                        default='beamer_filter.py',
                        required=False)

    args = parser.parse_args()

    if not os.path.isfile ( args.source ):
        print ( args.source + " does not exist" )

    else:

        theme = args.theme
        if len(theme) > 0:
            theme = " -V theme=" + theme

        color = args.color
        if len(color) > 0:
            color = " -V colortheme=" + color

        title = args.title
        if len(title) > 0:
            title = " -V title=" + title

        output_file = os.path.basename ( args.source )
        if len(args.title) > 0:
            output_file = args.title
        output_file = output_file + '.' + args.extension
        output_file = os.path.abspath ( output_file )

        filter = args.filter
        if not os.path.isfile ( filter ):
            filter = os.path.join ( os.path.dirname(__file__),
                                    filter )
            if not os.path.isfile ( filter ):
                filter = ""
            else:
                filter = " --filter " + filter

        # build list of search directories
        set_texinputs ( args.directories )

        # make paths relative to original source file
        os.chdir ( os.path.dirname ( args.source ) )

        source = expand_source ( args.source )
        if len(source) == 0:
            print ( "No source files found" )

        else:
            command = ( 'pandoc --standalone' +
                        filter +
                        title +
                        theme +
                        color +
                        ' -f rst ' +
                        ' -t ' + output_format ( args.extension.lower() ) +
                        ' -o ' + output_file +
                        ' ' + expand_source ( args.source ) )

            print ( command )

            os.system ( command )

