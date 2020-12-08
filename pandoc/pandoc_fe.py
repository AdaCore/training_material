import argparse
import os
import sys
import subprocess
from pathlib import Path

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

def to_texinputs_path( path ):
   abspath = fix_file_case ( os.path.abspath ( path ) )
   if path.endswith('//'):
      # (texlive specific?) recursive path
      abspath += '//'
   return abspath

def extend_texinputs_path_list(unique, paths):
    for path in paths:
        abspath = to_texinputs_path(path)
        if abspath not in unique:
            unique.append(abspath)

'''
Set the TEXINPUTS environment variable based on the directories specified
on the command line. These directories will contain things like images
and Beamer theme files
'''
def set_texinputs ( new_directories ):

    # initialize list of directories
    unique = list()

    # add user-specified directories to front of list
    extend_texinputs_path_list(unique, new_directories.split ( ',' ))

    # add any previously existing directories
    current = os.environ.get('TEXINPUTS', "")

    # default separator (works for Windows and linux 'sh')
    separator = ';'
    # For linux, check if we're using ':' or ';' as a separator
    if not windows():
       # if we don't find a ";" separator, we will use ':'
       if not ';' in current:
          separator = ':'

    # add current TEXINPUTS paths
    extend_texinputs_path_list(unique, current.split (separator))

    # when TEXINPUTS ends w/ a separator it means to append to standard TeX paths
    texinputs_append = (len(current) and current[-1] == separator)
    texinputs_formated = separator.join ( unique ) \
                              + (separator if texinputs_append else '')
    os.environ['TEXINPUTS'] = texinputs_formated
    return texinputs_formated

'''
Chose and setup an output file full path based on various arguments received.
'''
def output_file_name(input_file, n, extension,
                     title = None,
                     output_file = None,
                     strip_extension = False,
                     output_dir = None):
    if output_file:
        name = output_file
    elif title:
        name = title
    else:
        name = os.path.basename(input_file)
    
    # In case of multiple inputs, add number
    if n:
        name += f" {n}"

    if strip_extension:
        name = str(Path(name).with_suffix(f'.{extension}'))
    else:
        name += f'.{extension}'
        
    if output_dir:
        output_dir_path = Path(args.output_dir)
        output_dir_path.mkdir(parents=True, exist_ok=True)
        name = str(output_dir_path / name)

    return os.path.abspath(name)

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
    if source_file.lower().endswith(".rst"):
       return [os.path.abspath(source_file)]
    else:
       dirname = os.path.dirname ( source_file )
       # Read lines from source file
       with open ( source_file ) as sources:
          files = list()
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
                files.append(path)
       return files

if __name__== "__main__":
    PANDOC = Path(sys.argv[0]).resolve().parent
    ROOT = PANDOC.parent

    parser = argparse.ArgumentParser()

    parser.add_argument('--source',
                        help='Source RST file OR text file with list of RST files',
                        nargs="+",
                        required=True)

    parser.add_argument('--output',
                        help='Output file name (without extension)')

    parser.add_argument('--extension',
                        help='Output file extension. "PDF" => Beamer, "TEX" => LaTeX, "DOCX" => Word, "PPTX" => PowerPoint',
                        default='pdf',
                        required=False)

    parser.add_argument('--directories',
                        help='Comma-separated list of folders to search for things like images and Beamer themes',
                        default=str(ROOT) + "//,:",
                        required=False)

    parser.add_argument('--title',
                        help='Document title and name of output file. '
                             'If not specified, output file will be source filename '
                             'with specified extension.\n'
                             'If several sources are specified, each one will have a '
                             'number appended after its title.',
                        default='',
                        required=False)

    parser.add_argument('--theme',
                        help='Beamer theme',
                        default='adacore',
                        required=False)

    parser.add_argument('--color',
                        help='Beamer color theme',
                        default='adacore',
                        required=False)

    parser.add_argument('--filter',
                        help='Pandoc filter to do special processing',
                        default=str(PANDOC / 'beamer_filter.py'),
                        required=False)

    parser.add_argument('--output-dir',
                        help="Output directory",
                        required=False)

    parser.add_argument('--do-not-strip-extension',
                        help="Do not strip the original extension from the title of "
                             "the output file.\nLegacy behaviour\n"
                             "Eg. 'foo.rst.pdf' will become 'foo.pdf'.",
                        action="store_true")

    parser.add_argument('--hush',
                        help="Hide ran commands.",
                        action="store_true")

    args = parser.parse_args()

    for n, source_or_source_list in enumerate(args.source):
        if not os.path.isfile ( source_or_source_list ):
            print ( source_or_source_list + " does not exist" )

        else:

            theme = args.theme
            if len(theme) > 0:
                theme = " -V theme=" + theme

            color = args.color
            if len(color) > 0:
                color = " -V colortheme=" + color
                
            pandoc_title_arg = args.title
            if args.title:
                pandoc_title_arg = ' -V title="' + args.title.replace('_', ' ') + '"'

            input_file = args.title or source_or_source_list

            # Output default value is input file name
            output_file = output_file_name(input_file, n if len(args.source) > 1 else None,
                                           args.extension,
                                           title = args.title,
                                           output_dir = args.output_dir,
                                           output_file = args.output,
                                           strip_extension = not args.do_not_strip_extension)
            filter = args.filter
            if not os.path.isfile ( filter ):
                filter = os.path.join ( os.path.dirname(__file__),
                                        filter )
                if not os.path.isfile ( filter ):
                    filter = ""
            if os.path.isfile ( filter ):
               filter = " --filter " + filter

            # build list of search directories
            texinputs = set_texinputs ( args.directories )
            if not args.hush:
                print(f"TEXINPUTS={os.environ['TEXINPUTS']}")

            source_list = expand_source ( source_or_source_list )

            pcwd = os.getcwd()

            os.chdir( os.path.dirname (source_or_source_list))

            if len(source_list) == 0:
                print ( "No source files found" )

            else:
                if len(args.source) > 1:
                    print (f"{input_file} -> "
                           f"{output_file}")

                command = ( 'pandoc --standalone',
                            '--resource-path', texinputs,
                            filter,
                            pandoc_title_arg,
                            theme,
                            color,
                            '-f rst',
                            '-t ' + output_format ( args.extension.lower() ),
                            '-o ' + output_file, *source_list)

                if not args.hush:
                    print ( " ".join(command) )

                subprocess.check_call ( " ".join(command), shell=True )
            os.chdir(pcwd)
