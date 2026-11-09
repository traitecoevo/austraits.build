# SAH_2026 metadata questions (first pass done without asking, user 2026-11-05): the judgement calls, written into metadata.yml's
# questions block (replacing "questions: .na"; idempotent). Answered 2026-11-06 and removed: "and/or" phenology -> both flowering and
# fruiting; "Questionably naturalised" -> introduced; genus Habitat not used; Acacia study notes -> species traits unless they state
# regional variation explicitly.
import yaml
p = "data/SAH_2026/metadata.yml"
Q = {
    "question1": "Illegible values are left out: Acacia menzelii pods \"2.514.5 cm long\", A. symonii \"345 cm long\", Lissanthe strigosa "
                 "peduncle \"2-1.5 mm\", Geranium molle leaves \"2-0.5 cm long\". Five clear OCR slips were corrected in the parser's "
                 "fix_text (Brassica nigra, Goodenia benthamiana, Scaevola collina, Hakea leucoptera, Grammosolen dixonii), and cm / m "
                 "for mm in Ecballium elaterium and Medicago truncatula.",
}
s = open(p).read()
blk = "questions:\n" + "".join(f"  {k}: {yaml.safe_dump(v, width=130, default_style=None).strip().removesuffix('...').strip()}\n".replace("\n  ", "\n    ") for k, v in Q.items())
import re
s = re.sub(r"^questions:(?: \.na\n|\n(?:  .*\n)+)", blk, s, flags=re.M)
yaml.safe_load(s); open(p, "w").write(s); print(blk)
