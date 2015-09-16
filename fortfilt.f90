module fortfilt
  implicit none

contains
  subroutine naive_gauss (source, filtered, size)
    implicit none
    integer, intent(in)                          :: size
    double precision, intent(in)                 :: source(:,:)
    double precision, intent(out)                :: filtered(:,:)

    double precision, parameter                  :: PI = 4.*atan(1.)

    integer                                      :: i, j, k, l
    integer                                      :: ii, jj      ! inside the array
    integer                                      :: nx, ny
    integer                                      :: dim(2)

    double precision                             :: val, wsum
    double precision                             :: dsq         ! distance squared
    double precision                             :: wght        ! weight

    dim = shape(source)
    nx = dim(1)
    ny = dim(2)

    print *, nx, ny
    do i = 1, nx
       do j = 1, ny
          val = 0
          wsum = 0
          do k = i-size, i+size     ! inner loop over kernel
             do l = j-size, j+size  ! inner loop over kernel
                ii = min(nx, max(1,k))   ! make sure i is always inside the grid (this implies values are extendet (stretched at the boundaries))
                jj = min(ny, max(1,l))   ! make sure j is always inside the grid (this implies values are extendet (stretched at the boundaries))
                dsq = (j-l)**2 + (i-k)**2
                wght = exp(-dsq / (2*size**2)) / (2*PI*size**2)
                val = val + source(ii,jj) * wght
                wsum = wsum + wght
                ! print *, i,j, k, l, ii, jj, dsq
             end do
          end do
          filtered(i,j) = val / wsum
       end do
    end do
    

    
  end subroutine naive_gauss

end module fortfilt
