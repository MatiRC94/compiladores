# Unix makefile for tigermain example

HOME=/usr/bin
MOSMLHOME=${HOME}
MOSMLTOOLS=camlrunm /usr/share/mosml/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC= mosmlc -c -liberal
MOSMLL= mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo \
	tigertemp.uo tigertopsort.uo tigertree.uo tigermuestratipos.uo ejercicio4.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo printty.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) tiger
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml tigertopsort.sml tigermuestratipos.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerpp.uo: tigerabs.uo 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertab.uo: tigertab.ui 
tigerescap.ui: tigerabs.uo 
tigermuestratipos.ui: tigertips.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigerabs.uo tigertrans.ui printty.uo tigertopsort.uo 
tigergrm.ui: tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo \
    tigermuestratipos.ui 
tigertemp.uo: tigertemp.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo tigertrans.ui
tigermain.uo: tigerseman.ui tigerescap.ui ejercicio4.uo tigergrm.ui \
    tigerlex.uo tigerpp.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
