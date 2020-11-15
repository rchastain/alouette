
alouette: alouette.pas
	mkdir -p units
	fpc $^ -o$@ -Mobjfpc -Sh -FcUTF8 -FUunits -dRELEASE -B -CX -XX -Xs

clean:
	rm -f units/*.o
	rm -f units/*.ppu
