import re, subprocess
REPO = "/Users/z3524079/GitHub/austraits.build"
HEAD = re.compile(r"^ {3,4}\^?([A-Za-z0-9_]+):\s*$")  # optional ^ = approved for develop

def blocks(lines):
    """Return {trait: (start, end)} line spans of trait entries (indented 3-4 spaces)."""
    out, cur, start = {}, None, None
    for i, l in enumerate(lines):
        m = HEAD.match(l)
        ends = m or (l.strip() and not l.startswith("    "))
        if cur and ends:
            out[cur] = (start, i); cur = None
        if m:
            cur, start = m.group(1), i
    if cur: out[cur] = (start, len(lines))
    return out

def develop_text():
    return subprocess.run(["git", "-C", REPO, "show", "origin/develop:config/traits.yml"],
                          capture_output=True, text=True, check=True).stdout

def develop_traits():
    return set(blocks(develop_text().splitlines(keepends=True)))
