slides.html: slides.md
	pandoc --self-contained --standalone --smart -t slidy -o $@ $^
