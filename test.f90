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
  data(6,4) = 15

  write(*,"(12f4.1)") data

  ! call naive_gauss(data, filtered_data, 2)
  call BoxBlur(data, filtered_data, 2)
  print *, "filtered:"
  write(*,"(12f4.1)") filtered_data

end program test_filter
