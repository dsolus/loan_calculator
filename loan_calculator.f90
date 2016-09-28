!loan_calculator

!Write a program that calculates information regarding a loan.
program loan_calculator
    implicit none
    !Define/add variables, integer? array??
    real,dimension(3, 5) :: data
    real,dimension(3) :: loanTerms
    integer :: i, N, initial_amount, total_years
    real :: rate, J, payment

    !Prompt the user to enter the name of a file that has the  loan terms.
    !print*, "Input the data file: "

    !open data file
    open (unit=1, file='loanTerms.tab', status='old', action='read')

    !read*, data
    read(1,*), data

    !close data file
    close(unit=1)

    !display data file
    do i = 1, size(data(1,:))
        write(*,*),"loan", i,":", data(:,i)
    enddo

    !Take the first loan_terms from data
    !maybe add a prompt so this can be changed later
    loanTerms =  data(:,1)
    write(*,*),"loan terms", loanTerms

    !seperate loan terms
    initial_amount = loanTerms(1)
    total_years = loanTerms(2)
    rate = loanTerms(3)
    write(*,*),"Initial Balance: $", initial_amount
    write(*,*),"Years to Pay: ", total_years
    write(*,*),"Interest Rate: ", rate,"%"

    !Calculate the following
    !First calculate the monthly payment
    !We need to know the effective interest rate J
    J = rate / (100 * 12)
    !write(*,*),"J: ", J

    !and the total number of payments
    N = total_years * 12
    !write(*,*),"N: ", N

    !and use the formula to calculate the monthly payment
    payment = initial_amount * (J / (1 - (1 + J)**(-N)))
    write(*,*),"Monthly Payment: $", payment
    !Calculate the monthly payment schedule (only display first and last years)
    !Calculate Total amount paid
    !Calculate Total interest paid
endprogram loan_calculator

