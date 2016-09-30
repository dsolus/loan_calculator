!loan_calculator

!Write a program that calculates information regarding a loan.
program loan_calculator
    implicit none
    !Define/add variables, integer? array??
    real,dimension(3, 5) :: data
    real,dimension(3) :: loanTerms
    integer :: i, N, initial_amount, total_years, counter
    real :: rate, J, payment, interest, balance, principal, total


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
    initial_amount = 170000 !loanTerms(1)
    total_years = 15 !loanTerms(2)
    rate = 2.875 !loanTerms(3)
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
    !We need a while loop to print out the number of periods
    counter = 1
    write(*, '(6X, A6, 4X, A9, 9X, A8, 10X, A7)') 'Period', 'Principal', 'Interest', 'Balance' 
   

    !initialize the balance for the loan loop
    balance = initial_amount
    do while (counter < (N + 1))
        !calculate the interest amount paid
        interest = balance*rate/(12*100)
        !calculate the principal amount paid
        principal = payment - interest
        
        balance = (balance + interest) - payment
        !if (balance < payment) then
        !    balance = 0.00
        !endif
        write(*,*), counter, principal, interest, balance
        counter = counter + 1
    enddo

    !Calculate Total amount paid
    total = total_payments(N, payment)
    write(*,*),'Total Payments: $', total

contains
    function total_payments(x, y)
        integer, parameter  :: RP = selected_real_kind(15)
        integer, intent(IN) :: x
        Real, intent(IN) :: y
        Real(kind = RP) :: total_payments

        total_payments = y*x
    end function total_payments

    !Calculate Total interest paid
endprogram loan_calculator

