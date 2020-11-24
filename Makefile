
PC=fpc
DEBUG=false
ifeq ($(DEBUG),true)
	PFLAGS=-FcUTF8 -B -Mobjfpc -Sah -FUunits -dDEBUG -ghl
else
	PFLAGS=-FcUTF8 -B -Mobjfpc -Sh -FUunits -dRELEASE -CX -XX -Xs
endif

alouette: alouette.pas
	mkdir -p units
	$(PC) $^ -o$@ $(PFLAGS)

clean:
	rm -f units/*.o
	rm -f units/*.ppu
