import argparse
import os
import shutil
import tempfile

def tempname():
    temp = tempfile.NamedTemporaryFile()
    temp.close()
    return temp.name

def process_one_file ( in_filename ):

    lines = []
    with open (in_filename) as f:
        lines = f.read().splitlines()

    while True:
        lines, found = replace_admonition ( lines )
        if not found:
            break

    with open(in_filename, 'w') as fp:
        for item in lines:
            fp.write(item + '\n')

if __name__== "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument('--filename',
                        required=True)

    parser.add_argument('--version',
                        help='most recent language version supported ' +
                             '(e.g. "95" if training does not support 2005/2012/2022)',
                        required=True)

    args = parser.parse_args()
    compare_to = int(args.version)

    fp = open(args.filename, 'r')
    tempfile_name = tempname()
    output = open(tempfile_name, 'w')
    while True:
        line = fp.readline()
        if not line:
            break
        if 'language_version' in line:
            version = int(line.strip().split()[1])
            if version > compare_to:
                output.write ('*This functionality unsupported before Ada ' + str(version) + '*\n')
            else:
                output.write ( line )
        else:
            output.write ( line )
    fp.close()
    output.close()
    shutil.copyfile ( tempfile_name, args.filename )
    os.remove (tempfile_name)


