program test_filter
  use fortfilt
  implicit none
  
  double precision, dimension(12,8)     :: data
  double precision, dimension(12,8)     :: filtered_data

  integer                               :: i,j

  print *, 'filter test'

  ! fill data with checkerboard pattern
  ! do i = 1, 12
  !    do j = 1, 8
  !       data(i,j) = mod(i+j,2)
  !    end do
  ! end do

  data = 0
  filtered_data = 0
  data(6,4) = 15

  write(*,"(12f4.1)") data

  call naiveGauss(data, filtered_data, 3)
  print *, "naive Gauss filtered:"
  write(*,"(12f4.1)") filtered_data
  print *, "sum:", sum(filtered_data)
  call fastGauss(data, filtered_data, 1)
  print *, "three times box filtered:"
  write(*,"(12f4.1)") filtered_data
  print *, "sum:", sum(filtered_data)
  data = 0
  data(6,4) = 15
  filtered_data = 0
  call naiveBox(data, filtered_data, 1)
  print *, "naive box filtered:"
  write(*,"(12f4.1)") filtered_data
  print *, "sum:", sum(filtered_data)
  call BoxBlur(data, filtered_data, 1)
  print *, "fast box filtered:"
  write(*,"(12f4.1)") filtered_data
  print *, "sum:", sum(filtered_data)

end program test_filter
