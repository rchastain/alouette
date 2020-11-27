
PC=fpc
DEBUG=false
ifeq ($(DEBUG),true)
	PFLAGS=-FcUTF8 -B -Mobjfpc -Sah -FUunits -dDEBUG -ghl
else
	PFLAGS=-FcUTF8 -B -Mobjfpc -Sh -FUunits -dRELEASE -CX -XX -Xs
endif

alouette: alouette.pas
ifeq ($(OS),Windows_NT)
	if not exist units mkdir units
	$(PC) $^ -o$@.exe $(PFLAGS)
else
	mkdir -p units
	$(PC) $^ -o$@ $(PFLAGS)
endif

clean:
ifeq ($(OS),Windows_NT)
	del /q units\*.o
	del /q units\*.ppu
else
	rm -f units/*.o
	rm -f units/*.ppu
endif
