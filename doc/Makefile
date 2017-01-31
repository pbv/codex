
PANDOC= pandoc

PDF_OPTIONS= -s --highlight-style monochrome --number-section

HTML_OPTIONS= -s --mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML --number-sections

%.html : %.md
	$(PANDOC) $(HTML_OPTIONS) $< -o $@

%.pdf : %.md 
	$(PANDOC) $(PDF_OPTIONS) $< -o $@


all : USER-GUIDE.pdf USER-GUIDE.html

