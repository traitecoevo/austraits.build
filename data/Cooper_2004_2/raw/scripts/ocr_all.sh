#!/bin/bash
SP=/private/tmp/claude-502/-Users-z3524079-GitHub-traits-build-inst-support/fb9a03f2-b5ee-4e53-a6f9-9631bd56b1ab/scratchpad/cooper
D=/Users/z3524079/Library/CloudStorage/OneDrive-UNSW/Documents/taxon_descriptions/resources-books
for t in pages001-100 pages101-200 pages201-400 pages401-560 pages_rescanned pages_rescanned2; do
  f=$D/Cooper_2004_$t.pdf; n=$(pdfinfo "$f" | awk '/^Pages/{print $2}')
  for p in $(seq 1 $n); do echo "$f $t $p"; done
done | xargs -P 4 -n 3 $SP/scripts/ocr_page.sh
echo "OCR done: $(ls $SP/ocr/ | grep -c 'tsv$') pages"
