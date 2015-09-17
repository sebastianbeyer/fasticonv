module fortfilt
  implicit none

contains
  subroutine naive_gauss (source, filtered, r)
    implicit none
    integer, intent(in)                          :: r
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

    do i = 1, nx
       do j = 1, ny
          val = 0
          wsum = 0
          do k = i-r, i+r     ! inner loop over kernel
             do l = j-r, j+r  ! inner loop over kernel
                ii = min(nx, max(1,k))   ! make sure i is always inside the grid (this implies values are extendet (stretched at the boundaries))
                jj = min(ny, max(1,l))   ! make sure j is always inside the grid (this implies values are extendet (stretched at the boundaries))
                dsq = (j-l)**2 + (i-k)**2
                wght = exp(-dsq / (2.d0*r**2)) / (2.d0*PI*r**2)
                val = val + source(ii,jj) * wght
                wsum = wsum + wght
                ! print *, i,j, k, l, ii, jj, dsq
             end do
          end do
          filtered(i,j) = val / wsum
       end do
    end do
    
  end subroutine naive_gauss

  subroutine fast_gauss (source, filtered, r)
    implicit none
    integer, intent(in)                          :: r
    double precision, intent(in)                 :: source(:,:)
    double precision, intent(out)                :: filtered(:,:)

    integer                                      :: nx, ny
    integer                                      :: dim(2)
    
    dim = shape(source)
    nx = dim(1)
    ny = dim(2)

  end subroutine fast_gauss
  
  subroutine BoxBlurH (source, filtered, r)
    ! computes horizontal blur
    implicit none
    integer, intent(in)                          :: r
    double precision, intent(in)                 :: source(:,:)
    double precision, intent(out)                :: filtered(:,:)

    integer                                      :: nx, ny
    integer                                      :: dim(2)

    double precision                             :: wght  ! weight
    double precision                             :: sum  
    integer                                      :: i,j
    integer                                      :: il    ! leftmost  pixel which should be removed from the accumulator
    integer                                      :: ir    ! rightmost pixel which should be added   to   the accumulator

    dim = shape(source)
    nx = dim(1)
    ny = dim(2)

    wght = 1.d0 / (2.d0*r+1.d0)

    do j = 1, ny   ! loop over all rows
       
       ! compute sum at first pixel
       sum = source(1,j)
       do i = 1, r  ! loop over box kernel
          sum = sum + source(1,j) + source(i+1,j) ! always take 1 as left pixel, to not get out of grid
          ! print *, sum
       end do

       ! generate output pixel, then update running sum
       do i = 1, nx
          ! print *, j, i, sum
          filtered(i,j) = sum * wght
          il = max(i-r, 1)     ! make sure we dont get off the grid
          ir = min(i+r+1, nx)  ! make sure we dont get off the grid
          sum = sum + source(ir,j) - source(il,j)
       end do
       
       
    end do
    
       
  end subroutine BoxBlurH

  
  subroutine BoxBlur (source, filtered, r)
    ! computes box blur, by calling horizontal boxblur twice, the second time with tansposed matrix
    implicit none
    integer, intent(in)                          :: r
    double precision, intent(in)                 :: source(:,:)
    double precision, intent(out)                :: filtered(:,:)
    
    integer                                      :: nx, ny
    integer                                      :: dim(2)
    
    double precision, allocatable                :: source_t(:,:)   ! source transposed
    double precision, allocatable                :: filtered_t(:,:)   ! filtered transposed
    
    dim = shape(source)
    nx = dim(1)
    ny = dim(2)
    allocate(source_t(ny,nx))
    allocate(filtered_t(ny,nx))


    ! first horizontal blur
    call BoxBlurH (source, filtered, r)
    ! then transpose result and call horizontal blur again, now really doing vertical blur
    source_t = transpose (filtered)
    call BoxBlurH (source_t, filtered_t, r)
    ! transpose again, to get back initial shape
    filtered = transpose (filtered_t)
    
  end subroutine BoxBlur

  
  subroutine fastGauss (source, filtered, r)
    ! computes a fast approximation to a Gaussian filter, by applying a series of box filters
    implicit none
    integer, intent(in)                          :: r
    double precision, intent(in)                 :: source(:,:)
    double precision, intent(out)                :: filtered(:,:)
    
    integer                                      :: nx, ny
    integer                                      :: dim(2)
    
    double precision, allocatable                :: tmpfilter1(:,:)   ! tmp array to store intermediate results
    double precision, allocatable                :: tmpfilter2(:,:)   ! tmp array to store intermediate results
    
    dim = shape(source)
    nx = dim(1)
    ny = dim(2)
    allocate(tmpfilter1(nx,ny))
    allocate(tmpfilter2(nx,ny))

    call BoxBlur (source, tmpfilter1, r)
    call BoxBlur (tmpfilter1, tmpfilter2, r)
    call BoxBlur (tmpfilter2, filtered, r)

  end subroutine fastGauss
  
  
end module fortfilt
