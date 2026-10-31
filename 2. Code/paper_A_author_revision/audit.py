"""Read-only closure checks for the current accepted and reset-review Paper A."""
from pathlib import Path
import ast, csv, hashlib, json, math, os, re, statistics, subprocess
from pypdf import PdfReader

R = Path(__file__).resolve().parents[2]
O = R / '3. Output/paper_A_author_revision'
D = R / '4. Latex/paper_A'
T = R / 'tmp/paper_A_author_revision'
checks = []

def ok(name, condition):
    assert condition, name
    checks.append(name)

def rows(path):
    with (O / path).open(encoding='utf-8-sig') as f:
        return list(csv.DictReader(f))

for path in (R / '2. Code/paper_A_author_revision').glob('*.py'):
    ast.parse(path.read_text(encoding='utf-8-sig'), filename=str(path))
checks.append('Python source syntax')
main = rows('Main/summary.csv')[0]
common = rows('Common/summary.csv')[0]
ok('Main and common samples', (int(main['teachers']), int(main['decisions']), int(common['teachers']), int(common['decisions'])) == (2520, 20160, 2164, 17312))
ok('Eight responses in both analysis cohorts', all(int(x['fe_decisions']) == 8 * int(x['fe_teachers']) for x in [main, common]))
shares = rows('Main/decomposition.csv')
draws = rows('Main/decomposition_bootstrap_draws.csv')
ok('Decomposition shares sum to one', math.isclose(sum(float(x['share']) for x in shares), 1, abs_tol=1e-12))
ok('199 accepted decomposition draws', len(draws) == 199 and all(x['accepted'] == 'TRUE' for x in draws))
ok('Gradient, Hessian and optimizer checks', all(float(x['gradient']) < .002 and float(x['min_hessian']) > 0 and float(x['optimizer']) == 0 for x in draws))
ok('13 boundary draws retained and disclosed', sum(int(x['singular']) for x in draws) == 13)
ok('All decomposition SEs reproduce', all(math.isclose(float(x['se']), statistics.stdev(float(b[x['component']]) for b in draws), abs_tol=1e-12) for x in shares))
parts = rows('tables/partition_audit.csv')
ok('81 prediction partition refits verified', len(parts) == 81 and all(int(x['rank_drop']) == 0 and int(x['other_warnings']) == 0 and float(x['max_stat_error']) < 1e-11 for x in parts))
for label in ['B_random', 'B_ordered', 'C_random', 'NoScreen_random', 'Matched_random']:
    fits = rows('tables/prediction_fit_audit_' + label + '.csv')
    ok(label + ': 399 complete full-refit audits', len(fits) == 399 and all(int(x['rank_drop']) == 0 and int(x['other_warnings']) == 0 for x in fits))
    # Fractional binomial weighting emits the documented non-integer-success warning.
    ok(label + ': expected weight warnings only', all(int(x['weight_warnings']) == 10 for x in fits))
std = json.loads((O / 'diagnostics/standardization.json').read_text())
ok('Common-support standardization', (std['main_teachers'], std['supported_teachers'], std['support_cells'], std['profiles'], std['draws']) == (2520, 2520, 12, 48, 1999))
provenance = json.loads((O / 'diagnostics/provenance.json').read_text())
ok('Analysis inputs unchanged', all(hashlib.md5((R / p).read_bytes()).hexdigest() == v for p, v in provenance['input_md5'].items()))
manifest = json.loads((O / 'comparison_manifest.json').read_text())
ok('Reset comparison baseline and exhibit inventory', manifest['baseline'] == '8306e22' and manifest['counts'] == {'old': 19, 'new': 20} and manifest['old_figures_preserved'] == 7 and manifest['old_tables_preserved'] == 11)
for asset in [x for x in manifest['exhibits'] if x['side'] == 'old' and x['kind'] == 'figure']:
    name = asset['name'] + '.png'
    baseline = subprocess.check_output(['git', 'show', '8306e22:3. Output/paper_A/figures/' + name], cwd=R)
    ok('Preserved baseline figure: ' + name, (D / 'comparison_baseline' / name).read_bytes() == baseline)

def normalized(s):
    s = s[s.index(r'\begin{document}'):]
    s = re.sub(r'(?<!\\)%[^\n]*', '', s)
    s = s.replace(r'\mbox{', '{').replace(r'\hskip0pt', '')
    s = re.sub(r'\\addtocounter\{section\}\{-1\}', '', s)
    s = re.sub(r'\s+', '', s)
    return re.sub(r'\{(\\cite[pt]\{[^{}]*\})\}', r'\1', s)

def recover(raw, side):
    """Recover both branches directly; avoid platform-dependent latexrevise CRLF parsing."""
    body = raw[raw.index(r'\begin{document}'):]
    state = None
    pieces = []
    pos = 0
    for m in re.finditer(r'\\DIF(add|del)(begin|end)(?:FL)?\b', body):
        if state is None or state == side:
            pieces.append(body[pos:m.start()])
        state = m[1] if m[2] == 'begin' else None
        pos = m.end()
    pieces.append(body[pos:])
    result = ''.join(pieces)
    if side == 'del':
        result = re.sub(r'%DIFDELCMD < ?', '', result)
    # Strip highlight wrappers with balanced TeX braces, preserving their contents.
    while (m := re.search(r'\\DIF(?:add|del)(?:FL)?\{', result)):
        brace = m.end() - 1
        depth, k = 1, brace + 1
        while depth:
            escaped = (len(result[:k]) - len(result[:k].rstrip('\\'))) % 2
            if not escaped:
                depth += (result[k] == '{') - (result[k] == '}')
            k += 1
        result = result[:m.start()] + result[brace + 1:k - 1] + result[k:]
    return result

for side, name in [('del', 'old'), ('add', 'new')]:
    recovered = recover((T / 'raw_diff.tex').read_text(encoding='utf8'), side)
    ok('Raw redline recovers ' + name + ' text and exhibit tokens', normalized(recovered) == normalized((T / (name + '.tex')).read_text(encoding='utf8')))

accepted = (D / 'paper_A_content.tex').read_text(encoding='utf8')
review = (D / 'paper_A_teacher_decisions.tex').read_text(encoding='utf8')
ok('Redundant passages removed from accepted text', all(x not in accepted for x in ['The label does not imply', 'establish general opposition', 'We reconstruct experimental responses from each record']))
ok('Approved judgment framework retained', 'Student information and teacher judgment' in accepted and r'>\tau(z_j,a_j,u_j)' in accepted and r'\theta_{k\ell}' in accepted and r'\tau_0' not in accepted)
ok('Retired headings do not increment current appendix letters', not re.search(r'\\section\{\\DIFdel\{', review))
ok('No unresolved exhibit placeholders', 'COTECEXHIBIT' not in review)
live_review = re.sub(r'(?<!\\)%[^\n]*', '', review)
labels = [x for x in re.findall(r'\\label\{([^}]+)\}', live_review) if '#' not in x]
ok('Unique review labels', len(labels) == len(set(labels)))
ok('Review references resolve', set(re.findall(r'\\ref\{([^}]+)\}', live_review)) <= set(labels))
for name in ['paper_A_content', 'paper_A_teacher_decisions']:
    log = (T / name / (name + '_authorbuild.log')).read_text(encoding='utf8', errors='replace')
    ok(name + ': clean compilation', not re.search(r'Overfull|undefined|multiply defined|destination with the same identifier', log, re.I))
    pdf = T / name / (name + '_authorbuild.pdf')
    ok(name + ': readable PDF', len(PdfReader(pdf).pages) >= 20)
ok('Published PDF matches compiled review', (D / 'paper_A_teacher_decisions.pdf').read_bytes() == (T / 'paper_A_teacher_decisions/paper_A_teacher_decisions_authorbuild.pdf').read_bytes())
result = {'status': 'PASS', 'baseline': '8306e22', 'checks': checks, 'accepted_pages': len(PdfReader(T / 'paper_A_content/paper_A_content_authorbuild.pdf').pages), 'review_pages': len(PdfReader(D / 'paper_A_teacher_decisions.pdf').pages), 'source_sha256': hashlib.sha256((D / 'paper_A_content.tex').read_bytes()).hexdigest(), 'pdf_sha256': hashlib.sha256((D / 'paper_A_teacher_decisions.pdf').read_bytes()).hexdigest(), 'scope': 'Programmatic closure; independent methods/editorial memos and visual QA are separate evidence.'}
(O / 'final_audit.json').write_text(json.dumps(result, indent=2), encoding='utf8')
print('PASS:', len(checks), 'checks; accepted/review pages:', result['accepted_pages'], result['review_pages'])
