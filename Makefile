F90=ifort
FCFLAGS=-O3
LDFLAGS=

TARGET= LPM.exe
OBJECT= modules.o mod_kinds.o mod_particle_attributes.o \
		mod_plankton_model.o main.o

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.mod
	rm -f *.o
	rm LPM.exe

modules.o: \
	mod_kinds.o
mod_kinds.o: \

mod_particle_attributes.o: \
	mod_kinds.o
mod_plankton_model.o: \
	mod_kinds.o \
	modules.o   \
	mod_particle_attributes.o 
