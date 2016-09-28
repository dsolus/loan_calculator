program array7
implicit none
integer, dimension(2,3) :: a

! Read in the data
read*, a

! print out the results
print*,'The whole array: ', a
print*,'column 1 ', a(:, 1)
print*,'column 2 ', a(:, 2)
print*,'column 3 ', a(:, 3)
print*,'row 1 ', a(1,:)
print*,'row 2 ', a(2,:)
end program array7


