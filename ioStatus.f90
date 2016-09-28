program ioStatus
  implicit none
  real, dimension(100) :: x, y
  integer, parameter :: channelNum = 98
  integer :: io = 0, counter = 1
  ! Use the iostat argument for the status of a IO command
  open( channelNum, file="input.dat", status='OLD', iostat=io)
  do while( io == 0) 
     read( channelNum,*,iostat=io), x(counter), y(counter)
     counter = counter + 1
  end do
  close( channelNum)
  counter = counter - 2
  write(*,*),"io: ", io, "counter: ", counter
  write(*,*)," x: ", x(:counter)
  write(*,*)," y: ", y(:counter)
end program ioStatus
