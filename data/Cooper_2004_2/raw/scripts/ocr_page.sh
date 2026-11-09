#!/bin/bash
# ocr_page.sh <pdf> <tag> <page>: render one page at the scan's native 240 dpi, Vision-OCR it, delete the image
SP=/private/tmp/claude-502/-Users-z3524079-GitHub-traits-build-inst-support/fb9a03f2-b5ee-4e53-a6f9-9631bd56b1ab/scratchpad/cooper
out=$SP/ocr/$2_$(printf %03d $3)
[ -s $out.tsv ] && exit 0
pdftoppm -f $3 -l $3 -r 240 -jpeg -singlefile "$1" $out
$SP/scripts/vocr $out.jpg > $out.tsv.tmp && mv $out.tsv.tmp $out.tsv
rm -f $out.jpg
