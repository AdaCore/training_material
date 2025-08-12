import os


def source_file_contents(filename, start_after, end_before):
    retval = ""

    echo_on = False

    # if start_after is not set, we'll start at the beginning
    if start_after == "":
        echo_on = True

    # begin_ug
    if os.path.isfile(filename):
        with open(filename, "r") as the_file:
            for line in the_file:
                # if we're not echoing, then look for the starting text
                if not echo_on:
                    if len(start_after) > 0 and start_after == line.strip():
                        echo_on = True
                # if we are echoing and we find the ending text, we're done
                elif len(end_before) > 0 and end_before == line.strip():
                    break
                # Do not echo lines with tags we're not looking for
                elif line.strip().startswith ("--|"):
                    pass
                # otherwise add this to the return value
                else:
                    retval = retval + line
        return retval
    # end_ug
    else:
        return filename
