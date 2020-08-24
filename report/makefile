OS = $(shell (uname | cut -b 1-6 ))
PS2 = $(shell (command -v ps2eps) )
ifeq ($(OS),Darwin)
OPEN = open
ACRO = open -a /Applications/Adobe\ Reader\ 9/Adobe\ Reader.app
ECHO = echo -e
else
OPEN = acroread
ACRO = acroread
ECHO = /bin/echo -e
endif

PICDIR = ../matlab/pics
TEX = abstract.tex intro.tex mhd.tex confinement.tex implementation.tex simulations.tex conclusion.tex acknowledgements.tex data.tex sbr.tex graphs.tex
SBR = ../ASTRA/sbr/wscaling.f ../ASTRA/sbr/pedscaling.f

all: makeperl preview

makeperl: ${TEX}
	@${ECHO} " Updating the figure dependencies..."
	@perl fetchpics.pl ${TEX}

##### PICTURES #####
include picgen.mk
include picstd.mk
include picX3.mk
include picDn.mk
PICELM = ${PICSTD} ${PICX3} ${PICDN}
PICS = ${PICGEN} ${PICELM}

preview: pdm.pdf
	@${OPEN} $< &

pdm.ps: pdm.dvi
	@${ECHO} "  # Doing PS..."
	@dvips -q $<
	@${ECHO} "  # PS Done!"

#pdm.pdf: pdm.dvi
#	@${ECHO} "  Printing PDF...\c"
#	@dvipdf $<
#	@${ECHO} ".. Done!"
#

pdm.pdf: pdm.ps
	@${ECHO} "  Printing PDF from PS...\c"
	@ps2pdf $<
	@${ECHO} ".. Done!"

do_eps: ${PICS}

pdm.dvi:: pdm.tex ${TEX} variables.tex notes.tex ${SBR} ${PICS}
	latex $<

pdm.dvi:: pdm.bib
	@${ECHO} "\n\n********** L A T E X - 1 **********\n\n"
	@latex pdm.tex
	@${ECHO} "\n\n%%%%%%%%%%% B I B T E X %%%%%%%%%%%\n\n"
	@bibtex pdm.aux
	@${ECHO} "\n\n********** L A T E X - 2 **********"
	@${ECHO} "********** L A T E X - 2 **********\n\n"
	@latex pdm.tex
	@${ECHO} "\n\n********** L A T E X - 3 **********"
	@${ECHO} "********** L A T E X - 3 **********"
	@${ECHO} "********** L A T E X - 3 **********\n\n"
	@latex pdm.tex

clean:
	@${ECHO} "  Cleaning temporary files..."
	@rm -f *.aux *.log *.dvi *.toc *.bbl *.blg *.ps *.fdb_latexmk *.mk

${PICDIR}/%.eps: ${PICDIR}/%.ps
ifeq ($(PS2),)
	@ps2epsi ${PICDIR}/$*.ps ${PICDIR}/$*.eps
else
	@ps2eps -f ${PICDIR}/$*.ps
endif

