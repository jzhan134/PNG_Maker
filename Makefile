main:
	gfortran -g3 -c movie.f calorder.f
	gfortran -g3 -o moviemaker movie.o calorder.o
convert:
	./moviemaker
	mkdir -p ./POV_Files
	mv ./*.pov ./POV_Files/
clean: 
	rm -f moviemaker *.o *.*~ 
	rm -rf ./POV_Files
