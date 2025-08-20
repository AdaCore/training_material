import os

def line_matches (line, search_str):
    if len(search_str) > 0:
        stripped = line.strip()
        if stripped == search_str:
            return True
        elif stripped == "--" + search_str:
            return True
        elif stripped == "--|" + search_str:
            return True
    return False

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
                    if line_matches (line, start_after):
                        echo_on = True
                # if we are echoing and we find the ending text, we're done
                elif line_matches (line, end_before):
                    break
                # Do not echo lines with tags we're not looking for
                elif line.strip().startswith("--|"):
                    pass
                # otherwise add this to the return value
                else:
                    retval = retval + line
        return retval
    # end_ug
    else:
        return filename
