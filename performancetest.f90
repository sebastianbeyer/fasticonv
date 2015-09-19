program test_performance
  use fortfilt
  implicit none

  integer                               :: nx, ny
  integer                               :: cmdstat
  integer                               :: numargs
  integer                               :: cmd_len
  character(len=32)                     :: method
  character(len=32)                     :: nx_char
  character(len=32)                     :: r_char
  integer                               :: r
  
  
  double precision, allocatable         :: data(:,:)
  double precision, allocatable         :: filtered_data(:,:)

  integer                               :: i,j

  numargs = command_argument_count ()
  if (numargs < 3) then
    print *, "usage: ./performacetest <method> <nx> <r>"
    print *, "method is 'naive', 'box', 'fast'"
    print *, ""
    error stop 64  ! 64 is bad usage
  end if

  call get_command_argument (1, method, cmd_len, cmdstat)
  call get_command_argument (2, nx_char, cmd_len, cmdstat)
  call get_command_argument (3, r_char, cmd_len, cmdstat)

  read (nx_char, *) nx
  read (r_char, *) r

  ny = nx

  allocate (data(nx,ny))
  allocate (filtered_data(nx,ny))

  ! fill data with checkerboard pattern
  ! do i = 1, 12
  !    do j = 1, 8
  !       data(i,j) = mod(i+j,2)
  !    end do
  ! end do

  data = 0
  data(nx/2,ny/2) = 15

  select case ( trim (method) )
     case ("naive")
        print *, "Doing naive gauss filter on a field of", nx, "by", ny, "with radius", r
        call naiveGauss(data, filtered_data, r)
        print *, "sum:", sum(filtered_data)
     case ("fast")
        print *, "Doing fast gauss filter on a field of", nx, "by", ny, "with radius", r
        call fastGauss(data, filtered_data, r)
        print *, "sum:", sum(filtered_data)
     case ("box")
        print *, "Doing box filter on a field of", nx, "by", ny, "with radius", r
        call BoxBlur(data, filtered_data, r)
        print *, "sum:", sum(filtered_data)
     case ("naivebox")
        print *, "Doing naive box filter on a field of", nx, "by", ny, "with radius", r
        call naiveBox(data, filtered_data, r)
        print *, "sum:", sum(filtered_data)
        
     case default
        print *, "Method ",method, "not found."
        error stop 64
  end select

end program test_performance
