module mat_util_mod
use kind_mod, only: dp
implicit none
private
public :: show_matrix
contains

subroutine show_matrix(a, tag)
! print a matrix if small or print stats on it if it has many rows
real(kind=dp), intent(in) :: a(:,:)
character(len=*), intent(in) :: tag
integer :: i, m, n, max_row_print_
character (len=*), parameter :: fmt_cr = "(a8, *(1x,f8.3))"
max_row_print_ = 10
m = size(a, 1)
n = size(a, 2)
write(*, '(a," = [",i0,"x",i0,"]")') trim(tag), m, n
if (m <= max_row_print_) then
   do i = 1, m
      print "(*(1x,f8.3))", a(i,:)
   end do
else
   print fmt_cr, "mean", sum(a,1)/m
   print fmt_cr,  "min", minval(a,1)
   print fmt_cr,  "max", maxval(a,1)
end if
print*
end subroutine show_matrix

end module mat_util_mod
