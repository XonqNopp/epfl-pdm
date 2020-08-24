OS = $(shell (uname | cut -b 1-6 ))
PS2 = $(shell (command -v ps2eps) )
PICDIR = ../matlab/pics
THEME = beamercolorthemeCRPP.sty beamerouterthemeCRPP.sty beamerthemeCRPP.sty
MYWORK = pdm_GaelINDUNI

TEX = latex
LATEX = latex
##### PICTURES #####
include picgen.mk
PICS = ${PICGEN}

ifeq ($(OS),Darwin)
OPEN = open
ACRO = open -a /Applications/Adobe\ Reader\ 9/Adobe\ Reader.app
ECHO = echo -e
else
OPEN = acroread
ECHO = /bin/echo
ACRO = acroread
endif

.PHONY: all makeperl previewall preview previewN previewS clean do_eps

all: makeperl preview

makeperl: main.tex header.tex
	@${ECHO} " Updating the figure dependencies..."
	@perl fetchpics.pl $^

previewall: ${MYWORK}_defense.pdf ${MYWORK}_slides.pdf ${MYWORK}_notes.pdf
	@${OPEN} ${MYWORK}_defense.pdf &
	@${OPEN} ${MYWORK}_slides.pdf &
	@${OPEN} ${MYWORK}_notes.pdf &

preview: ${MYWORK}_defense.pdf
	@${OPEN} $< &

previewS: ${MYWORK}_slides.pdf
	@${OPEN} $< &

previewN: ${MYWORK}_notes.pdf
	@${OPEN} $< &

## PDF dependencies
${MYWORK}_defense.pdf: ${MYWORK}_defense.ps
${MYWORK}_slides.pdf: ${MYWORK}_slides.ps
${MYWORK}_notes.pdf: ${MYWORK}_notes.ps
## DVI dependencies
${MYWORK}_defense.dvi: ${MYWORK}_defense.tex ${MYWORK}_defense.bbl main.tex header.tex ${THEME} ${PICS}
${MYWORK}_slides.dvi: ${MYWORK}_slides.tex ${MYWORK}_slides.bbl main.tex header.tex ${THEME} ${PICS}
${MYWORK}_notes.dvi: ${MYWORK}_notes.tex ${MYWORK}_notes.bbl main.tex header.tex ${THEME} ${PICS}
## AUX dependencies
${MYWORK}_defense.aux: ${MYWORK}_defense.tex header.tex main.tex
${MYWORK}_slides.aux: ${MYWORK}_slides.tex header.tex main.tex
## Special treatement, 'show only notes' does not output the aux file
${MYWORK}_notes.aux: ${MYWORK}_defense.aux
	@cp $< $@

## BBL dependencies
#${MYWORK}_defense.bbl: ${MYWORK}_defense.aux ../report/pdm.bib
#${MYWORK}_slides.bbl: ${MYWORK}_slides.aux ../report/pdm.bib
## Special treatement, 'show only notes' does not output the aux file
${MYWORK}_notes.bbl: ${MYWORK}_defense.bbl
	@latex ${MYWORK}_defense.tex
	@cp $< $@
	@cp ${MYWORK}_defense.aux ${MYWORK}_notes.aux

## Special treatement, 'show only notes' does not output the aux file
#${MYWORK}_notes.toc: ${MYWORK}_defense.aux
#	@cp ${MYWORK}_defense.toc $@
#	@cp ${MYWORK}_defense.nav ${MYWORK}_notes.nav
#	@cp ${MYWORK}_defense.snm ${MYWORK}_notes.snm

${MYWORK}_defense.ps: ${MYWORK}_defense.dvi
	@${ECHO} "  # Doing PS..."
	@dvips -q $<
	@psnup -2 -pa4 -W5.04in -H3.78in -m0.5cm $@ tmp.ps
	@rm $@
	@mv tmp.ps $@
	@${ECHO} "  # PS Done!"

${MYWORK}_slides.ps: ${MYWORK}_slides.dvi
	@${ECHO} "  # Doing PS..."
	@dvips -q $<
	@${ECHO} "  # PS Done!"

${MYWORK}_notes.ps: ${MYWORK}_notes.dvi
	@${ECHO} "  # Doing PS..."
	@dvips -q $<
	@psnup -4 -c -pa4 -W5.04in -H3.78in -m0.5cm $@ tmp.ps
	@rm $@
	@mv tmp.ps $@
	@${ECHO} "  # PS Done!"

%.pdf:
	@${ECHO} -e "  Printing $< in PDF...\c"
	@ps2pdf $< $*.pdf
	@${ECHO} ".. Done!"

do_eps: ${PICS}

%.dvi:
	@${LATEX} $<

%.aux:
	@${LATEX} $<

%.bbl: ../report/pdm.bib
	@${ECHO} "\n\n%%%%%%%%%%% B I B T E X %%%%%%%%%%%\n"
	@bibtex $*.aux
	@${ECHO} "\n%%%%%%%%%%% BIBTEX Done ! %%%%%%%%%%%\n\n"
	@${LATEX} $*.tex

clean:
	@${ECHO} "  Cleaning temporary files..."
	@rm -f *.aux *.log *.dvi *.toc *.bbl *.blg *.ps *.fdb_latexmk *.mk *.snm *.out *.nav

${PICDIR}/%.eps: ${PICDIR}/%.ps
ifeq ($(PS2),)
	@ps2epsi ${PICDIR}/$*.ps ${PICDIR}/$*.eps
else
	@ps2eps -f ${PICDIR}/$*.ps
endif


