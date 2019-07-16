
PANDOC= pandoc

PDF_OPTIONS= -s --smart --highlight-style monochrome --number-section

HTML_OPTIONS= -s -smart  --mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML --number-sections -H styles.html

%.html : %.md
	$(PANDOC) $(HTML_OPTIONS) $< -o $@

%.pdf : %.md 
	$(PANDOC) $(PDF_OPTIONS) $< -o $@


all :  USER-GUIDE.html   ## USER-GUIDE.pdf

