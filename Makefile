loan_calculator: loan_calculator.o
	gfortran -o loan_calculator loan_calculator.o

loan_calculator.o: loan_calculator.f90
	gfortran -c loan_calculator.f90 -o loan_calculator.o
