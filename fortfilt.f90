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

    print *, nx, ny
    do i = 1, nx
       do j = 1, ny
          val = 0
          wsum = 0
          do k = i-r, i+r     ! inner loop over kernel
             do l = j-r, j+r  ! inner loop over kernel
                ii = min(nx, max(1,k))   ! make sure i is always inside the grid (this implies values are extendet (stretched at the boundaries))
                jj = min(ny, max(1,l))   ! make sure j is always inside the grid (this implies values are extendet (stretched at the boundaries))
                dsq = (j-l)**2 + (i-k)**2
                wght = exp(-dsq / (2*r**2)) / (2*PI*r**2)
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

    double precision                             :: iarr  ! normalize TODO: change name!
    double precision                             :: val  
    integer                                      :: i,j
    integer                                      :: il   ! left end of accumulator
    integer                                      :: ir   ! right end of accumulator

    dim = shape(source)
    nx = dim(1)
    ny = dim(2)

    iarr = 1. / (2.*r+1.)
    print *, iarr

    do j = 1, ny   ! loop over all rows
       val = 0
       il = r-1
       ir = il+ 1 + r
       ! handle left edge
       ! handle center
       do i = r+1, nx-r
          val = val + source(ir,j) - source(il,j)
          ! print *,j, i, il, ir, val
          ir = ir + 1    ! move on
          il = il + 1    ! move on
          filtered(i,j) = val * iarr
       end do
       
       ! handle right edge
       
    end do
    
       
  end subroutine BoxBlurH
  
  subroutine BoxBlurV (source, filtered, r)
    ! computes horizontal blur
    implicit none
    integer, intent(in)                          :: r
    double precision, intent(in)                 :: source(:,:)
    double precision, intent(out)                :: filtered(:,:)

    integer                                      :: nx, ny
    integer                                      :: dim(2)

    double precision                             :: iarr  ! normalize TODO: change name!
    double precision                             :: val  
    integer                                      :: i,j
    integer                                      :: jt   ! top end of accumulator
    integer                                      :: jb   ! bottom end of accumulator

    dim = shape(source)
    nx = dim(1)
    ny = dim(2)

    iarr = 1. / (2.*r+1.)
    print *, iarr
    print *, 1./5.

    do i = 1, nx   ! loop over all columns
       val = 0
       jt = r-1
       jb = jt + 1 + r
       ! handle left edge
       ! handle center
       do j = r+1, ny-r
          val = val + source(i,jb) - source(i,jt)
          ! print *,i, j, jt, jb, val
          jt = jt + 1    ! move on
          jb = jb + 1    ! move on
          filtered(i,j) = val * iarr
       end do
       
       ! handle right edge
       
    end do
    
       
  end subroutine BoxBlurV

end module fortfilt
