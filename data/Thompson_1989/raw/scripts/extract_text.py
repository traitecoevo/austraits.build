"""Step 1 of the Thompson_1989 extraction: PDF -> text.

Writes into raw/text/:
  t.txt       pdftotext -layout output (pages separated by form feeds; pdf page 1 = Telopea p. 301)
  blocks.json one entry per taxon (79 species + 8 subspecies), from its heading to the next taxon's heading
  desc.txt    the habit and leaf part of each description (heading -> "Flowers"), read for the hand transcription in spec.txt

Step 2 is make_trait_data.py. Run both from anywhere. Needs poppler's pdftotext.
"""
import json, re, subprocess
from pathlib import Path

here = Path(__file__).parent.parent   # raw (this script is in raw/scripts)
pdf = Path.home() / ("Library/CloudStorage/OneDrive-UNSW/Documents/taxon_descriptions/resources-journal_articles/"
                     "Myrtaceae Leptospermum revision Thompson_1989 Telopea telopea19894902.pdf")
txt_dir = here / "text"
txt_dir.mkdir(exist_ok=True)

subprocess.run(["pdftotext", "-layout", str(pdf), str(txt_dir / "t.txt")], check=True)

L = (txt_dir / "t.txt").read_text(encoding="utf-8").split("\n")
# drop running headers
L = [l for l in L if not re.match(r"^\s*\d+\s+Telopea Vol", l) and not re.match(r"^\s*Thompson, Leptospermum\s+\d+\s*$", l)]

# taxon headings ("7. L. brachyandrum ...", "b. subsp. montanum ..."), after the key to groups
start = next(i for i, l in enumerate(L) if l.strip().startswith("1. L. macgillivrayi J. Thompson"))
heads = [i for i, l in enumerate(L) if i >= start and (
    re.match(r"^\s*\d+\.\s+L[\.,]\s*[a-z]", l) and not re.search(r"\d+\.\s+L\.?\s*\w+\.?\s+\d+\.\s+L", l)
    or re.match(r"^\s*[a-f]\.\s+subsp\.", l))]
end = next((i for i, l in enumerate(L) if i > start and l.strip().startswith("Acknowledgements")), len(L))

blocks = {}
for k, i in enumerate(heads):
    j = heads[k + 1] if k + 1 < len(heads) else end
    b = " ".join(x.strip() for x in L[i:j])
    b = re.sub(r"(\w)[-­]\s+(\w)", r"\1\2", b).replace("­", "")
    blocks[L[i].strip()[:45]] = b
json.dump(blocks, open(txt_dir / "blocks.json", "w"), indent=0)

out = []
for h, b in blocks.items():
    # the description starts after the last type / synonymy citation, at the first sentence naming a shrub or tree
    cut = 0
    for m in re.finditer(r"(H ?o ?l ?o ?t ?y ?p ?e|I ?s ?o ?t ?y ?p ?e ?s?|L ?e ?c ?t ?o ?t ?y ?p ?e|N ?e ?o ?t ?y ?p ?e|T ?y ?p ?e ?:|"
                         r"S ?y ?n ?o ?n ?y ?m ?y|B ?a ?s ?i ?o ?n ?y ?m)", b[:4000]):
        cut = m.end()
    seg = b[cut:]
    m = re.search(r"\.\s+([A-Z][^\.]*?\b(shrub|tree|Shrub|Tree)s?\b)", seg)
    s = m.start(1) if m else 0
    fl = seg.find(" Flowers", s)
    out.append(f"### {h}\n{seg[s:fl if fl > 0 else s + 1200]}\n")
(txt_dir / "desc.txt").write_text("\n".join(out))
print(len(blocks), "taxa ->", txt_dir)
