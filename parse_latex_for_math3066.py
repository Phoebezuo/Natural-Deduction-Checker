import sys
import json
import re

'''
Use it together with the exported JSON
file from [ProofChecker](https://olligobber.github.io/ProofCheck/)

python3 parse_latex.py something.json
'''

latex_template = '''
\\begin{{tabular}}{{|c|c|c|c|c|}}
    \hline
    Assumptions & Line & Formula & Justification & References \\\\ \hline
{}\\end{{tabular}}
'''
line_template = '    {} & ({}) & ${}$ & {} & {} \\\\ \hline \n'
symbol_dict = {
    # "⊥": " \\bot ",
    "~": " \\sim ",
    "∧": " \\land ",
    "∨": " \\lor ",
    "→": " \\Rightarrow ",
    "∀": " \\forall ",
    "∃": " \\exists ",
}


def parse_formula(formula: str):
    formula = re.sub(r'\(([∀∃][a-zA-Z])\)', r'\1', formula)
    for symbol in symbol_dict:
        formula = formula.replace(symbol, symbol_dict[symbol])
    return formula


def parse_rule(rule: str):
    for symbol in symbol_dict:
        rule = rule.replace(symbol, '$' + symbol_dict[symbol] + '$')
    return rule


f_name = sys.argv[1]
fp = open(f_name)
j = json.load(fp)
fp.close()
lines = j['lines']

proof = ''
line_num = 1
for line in lines:
    # line = {"assumption":[1],"formula":"","rule":"","references":[]}
    assumption = ','.join([str(_) for _ in line['assumptions']]) if len(
        line['assumptions']) > 0 else ''
    formula = parse_formula(line['formula'])
    rule = parse_rule(line['rule'])
    references = ','.join([str(_) for _ in line['references']])
    line_latex = line_template.format(
        assumption, line_num, formula, rule, references)
    line_num += 1
    proof += line_latex

print(latex_template.format(proof))
