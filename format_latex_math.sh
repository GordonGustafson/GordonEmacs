#!/bin/evn bash

read -r -d '' awk_add_newlines <<"EOF"
{
    RS = "\n"
    if ( $0 ~ /^[ \t]*$/ ) {
        print "\\\\";
    } else if ( $0 ~ /\\begin{/ || $0 ~ /\\\\[ \t]*$/ || $0 ~ /\\end{align/ || $0 ~ /\\end{.*}/ ) {
        print $0
    } else {
        print $0, " \\\\";
    }
}
EOF

sed -e "s;@@@;\\\\begin\\{align*\\};" -e "s;@@;\\\\end\\{align*\\};" -e "s;\\([^&=]\\)=\\|^=;\\1\\&=;" \
    | gawk "$awk_add_newlines"
