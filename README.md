# ProofCheck

Proof Checker for a system of Propositional and Predicate Logic.
<img src='https://i.loli.net/2020/12/12/Grfk8iwC7ydz4lQ.png' alt='Grfk8iwC7ydz4lQ'/>

**Notes:**
- type `|` to indicate $\lor$
- type `&` to indicate $\land$
- type `->` to indicate $\rightarrow$
- type `~` to indicate $\neg$, it need `()` when there are more terms
- type `_|_` to indicate $\bot$, it need `()` when there are more terms
- type `!` to indicate $\exists$, it need `()` when there are more terms
- type `@` to indicate $\forall$, it need `()` when there are more terms

## Usage

1.  Click on `index.html` to input / import your proof.

    <img src='https://i.loli.net/2020/12/12/hvusKCdmEXaiFo7.png' alt='hvusKCdmEXaiFo7'/>

2.  Download json file e.g. `xx.json`.

    <img src='https://i.loli.net/2020/12/12/dl7khV6c3UXP9vt.png' alt='dl7khV6c3UXP9vt'/>

3.  Type `python3 parse_latex.py xx.json` to output in latex.

    <img src='https://i.loli.net/2020/12/12/h9pyJwAgxO4vMer.png' alt='h9pyJwAgxO4vMer'/>

4.  Type `python3 parse_md.py xx.json` to output in markdown.

    <img src='https://i.loli.net/2020/12/12/GMR2hBoXFYQC8mH.png' alt='GMR2hBoXFYQC8mH'/>
