#!/bin/bash

RDRVIEW_OUTPUT=~/work/dropbox/rdrview
DROPBOX_DIR="dropbox:Apps/Dropbox PocketBook/articles/2021/"

add_link_to_dropbox() {
    # Create tmp file
    TEMP_FILE=$(mktemp)

    # Make link readable
    rdrview -T title,body -H $1 > $TEMP_FILE

    # Extract title
    TITLE=$(xmllint --html --xpath "//div/h1/text()" 2>/dev/null ${TEMP_FILE})
    if [ -z "${TITLE}" ];
    then
        # If title is empty then extract path out of URL and use it as filename
        TITLE=$(basename $1 .html)
    fi

    # Sanitize title
    echo "[-] Converting $TITLE"

    # Convert to PDF
    OUTPUT_FILE="${RDRVIEW_OUTPUT}/$(date +"%Y-%m-%d")-"$(echo ${TITLE} | sed -e 's/[^A-Za-z0-9._-]/_/g')".epub"
    pandoc -s --pdf-engine=xelatex --metadata title="${TITLE}" -f html -t epub -o ${OUTPUT_FILE} ${TEMP_FILE}

    # Copy to dropbox
    rclone copy ${OUTPUT_FILE} "${DROPBOX_DIR}"

    # Log
    echo "[-] Successfully added ${OUTPUT_FILE} to dropbox."

    # Clean up
    rm $TEMP_FILE
    rm $OUTPUT_FILE
}

add_link_to_dropbox $1
# EOF
