!loan_calculator

!Write a program that calculates information regarding a loan.
program loan_calculator
implicit none
!Define/add variables, integer? array??
real,dimension(3, 5) :: data
integer :: i 

!Prompt the user to enter the name of a file that has the  loan terms.
!print*, "Input the data file: "
!read*, data
open (unit=1, file='loanTerms.tab', status='old', action='read')

read(1,*), data
!read(1,*), n
!read(1,*), m
!allocate(data(n,m))

close(unit=1)
do i = 1, size(data(1,:))
    write(*,*),"loan", i,":", data(:,i)
enddo

!write(*,*),"loan 1: ", data(:,1)
!write(*,*),"loan 2: ", data(:,2)
!write(*,*),"loan 3: ", data(:,3)
write(*,*),"data ", data
!Calculate the following
!Calculate the monthly payment schedule (only display first and last years)
!Calculate Total amount paid
!Calculate Total interest paid
endprogram loan_calculator

