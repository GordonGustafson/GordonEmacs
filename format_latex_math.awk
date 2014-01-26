#!/bin/gawk -f
{
    RS = "\n"
    if ( $0 ~ /^[ \t]*$/ ) {
        print "\\\\";
    } else if ( $0 !~ /\\begin|\\end/ && $0 !~ /\\\\[ \t]*$/ ) {
        print $0, " \\\\";
    } else {
        print $0, "matches one of the previous"
    }
}


# BEGIN { lastwasmary = 0; }

# (tolower($1) ~ /mary/ && !lastwasmary) { print "Mary appeared."; lastwasmary = 1; }

# (tolower($1) ~ /mary/ && lastwasmary) { print "Mary appeared again"; lastwasmary = 1; }

# (tolower($1) !~ /mary/ && lastwasmary) { print "No Mary."; lastwasmary = 0; }
