Instantiates 2 instances of object ModuleHOF to verify if it conflicts when running with OpenMP. No conflicts where detected.

To run it:
 - ulimit -s unlimited
 - export OMP_NUM_THREADS=4
 - f95 -O4 -xopenmp=parallel -o LVopenMP MainMI.f95 ModuleHOF.f95 ModuleFL.f95
 - ./LVopenMP

This code is released under GPL so feel free to use it.

(c) Ricardo Miranda, 2013, mail@ricardomiranda.com.


