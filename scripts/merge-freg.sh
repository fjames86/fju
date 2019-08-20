#!/bin/sh

regfile=$1
#!/bin/sh

while IFS= read -r LINE; do
    bin/freg put $LINE
done < $regfile

