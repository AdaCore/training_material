# This script sorts all the keywords entered and displays them in a tabular format,
# similar to what is done for the "Reserved words" slide, or the RM.
# It will also mark language version, if not Ada 83

ada_kw_ref = {
    "95": ["abstract", "aliased", "protected", "requeue", "tagged", "until"],
    "2005": ["interface", "overriding", "synchronized"],
    "2012": ["some"],
}


def add_ref(kw):
    for refname, kws in ada_kw_ref.items():
        if kw in kws:
            return f"{kw} ({refname})"
    return kw


def tabulate(words, number_of_columns):
    cols = []
    col_maxlen = len(words) // number_of_columns
    if len(words) % number_of_columns:
        col_maxlen += 1

    for n in range(number_of_columns):
        cols.append(words[n * col_maxlen : (n + 1) * col_maxlen])

    lines = []
    for i in range(col_maxlen):
        line = []
        for c in cols:
            if i < len(c):
                width = max([15] + [2 + len(s) for s in c])
                line.append(("{:" + f"{width}" + "}").format(c[i]))
        lines.append("".join(line))
    return "\n".join(lines)


kw = set()
try:
    while True:
        kw.add(input())
except EOFError:
    pass

kws = [add_ref(k) for k in sorted(kw)]
print(tabulate(kws, 4))
