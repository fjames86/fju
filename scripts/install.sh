#!/bin/sh

echo ""
echo "Self extracting installer"
echo ""

export TMPDIR=$(mktemp -d /tmp/selfextract.XXXXXX)
ARCHIVE=$(awk '/^__ARCHIVE_BELOW__/ {print NR + 1; exit 0; }' $0)
tail -n+$ARCHIVE $0 | tar xzv -C $TMPDIR

CDIR=$(pwd)
cd $TMPDIR

# run installer script to set things up 
#./installer.sh

cd $CDIR
#rm -rf $TMPDIR 
exit 0

__ARCHIVE_BELOW__
