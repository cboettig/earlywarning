
pdf: 
	pandoc -s -S --template=simple.latex --biblio CommentReply.bib --csl proceedings-of-the-royal-society-b.csl CommentReply.md -o CommentReply.tex
	pdflatex CommentReply.tex	
	pdflatex CommentReply.tex

bib: 


clean:
	rm *.log *.aux *.spl *.out
