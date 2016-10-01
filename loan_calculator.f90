!loan_calculator

!Write a program that calculates information regarding a loan.
program loan_calculator
    implicit none
    !Define variable precision
    integer, parameter :: ikind = selected_real_kind(p=3)
    integer, parameter :: dble = selected_real_kind(15, 307)
    
    !Define/add variables
    character (len=25) :: input
    real (kind = dble), dimension(3, 5)  :: data
    real (kind = dble), dimension(3)     :: loanTerms
    integer (kind = ikind) :: i, counter
    integer                :: N, total_years
    real (kind = dble) :: initial_amount, rate, J, payment, interest, balance, principal, total


    !Prompt the user to enter the name of a file that has the  loan terms.
    write(*, '(A44)', advance='no') "Please enter the name of a loan terms file: " 
    read *, input
    write(*,*),'You entered ', input

    !open data file
    open (unit=1, file=input, status='old', action='read')

    !read*, data
    read(1,*), data

    !close data file
    close(unit=1)

    !display data file
    do i = 1, size(data(1,:))
        write(*,*),"loan", i,":", data(:,i)
    enddo
    write(*,*)

    !Take the first loan_terms from data
    !loop through the loan terms
    do i = 1, size(data(1,:))
        loanTerms =  data(:,i)
        write(*,*),"loan", i, loanTerms
    
        !seperate loan terms
        initial_amount = loanTerms(1)
        total_years = loanTerms(2)
        rate = loanTerms(3)
        write(*, '(A19, F12.2)'),"Initial Balance: $", initial_amount
        write(*, '(4X, A14, 10X, I3)'),"Years to Pay: ", total_years
        write(*, '(3X, A15, 7X, F5.3, A1)'),"Interest Rate: ", rate,"%"
        write(*,*)
        
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
            !calculate the balance, conditional to check for end of balance
            if (balance < payment) then
                balance = 0.00
            else
                balance = (balance + interest) - payment
            endif
            ! if (counter < (12 + 1)) then
            !     do counter = 1, 12 
            !         write(*,*), counter, principal, interest, balance
            !     enddo 
            !     write(*,*)
            ! endif
            write(*, '(I5, 6X, F10.2, 6X, F10.2, 6X, F10.2)'), counter, principal, interest, balance
        
            !if (counter > (N - 12)) then
            !    do counter = (N-12), N 
            !        write(*,*), counter, principal, interest, balance
            !    enddo 
            !    write(*,*)
            !endif
            counter = counter + 1
        enddo

        !Calculate Total amount paid
        total = total_payments(N, payment)
        write(*,*),'Total Payments: $', total
        !Calculater Total interest paid
    enddo

contains
    function total_payments(x, y)
        integer, parameter  :: RP = selected_real_kind(15,307)
        integer, intent(IN) :: x
        Real(kind = RP), intent(IN) :: y
        Real(kind = RP) :: total_payments

        total_payments = y*x
    end function total_payments
        
endprogram loan_calculator

