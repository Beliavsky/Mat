module mat_mod
use kind_mod, only: dp
implicit none
private
public :: eval_print

integer, parameter :: max_vars = 100, len_name = 32
logical, save :: eval_error = .false.

type :: var_t
   character(len=len_name) :: name = ""
   real(kind=dp), allocatable :: val(:,:)
end type var_t

type(var_t) :: vars(max_vars)
integer :: n_vars = 0

contains

subroutine report_error(msg)
   character(len=*), intent(in) :: msg
   print *, 'Error: ', trim(msg)
   eval_error = .true.
end subroutine report_error

subroutine eval_print(line)
   character(len=*), intent(in) :: line
   character(len=:), allocatable :: l, lhs, rhs, tag
   real(kind=dp),  allocatable :: result(:,:)
   integer :: pos
   eval_error = .false.        ! clear any previous error
   l = trim(adjustl(line))
   if (l == "") return

   call split_assignment(l, lhs, rhs)
   pos = 1
   result = parse_expression(rhs, pos)

   if (lhs /= "") call set_variable(trim(lhs), result)

   if (lhs == "") then
      tag = "ans"
   else
      tag = lhs
   end if
   if (eval_error) return       ! on any error, skip printing
   call show_matrix(result, tag)
end subroutine eval_print

subroutine split_assignment(str, lhs, rhs)
   character(len=*), intent(in) :: str
   character(len=:), allocatable :: lhs, rhs
   integer :: p

   p = index(str, "=")
   if (p == 0) then
      lhs = ""
      rhs = str
   else
      lhs = trim(adjustl(str(:p-1)))
      rhs = trim(adjustl(str(p+1:)))
   end if
end subroutine split_assignment

recursive function parse_expression(src, pos) result(mat)
   character(len=*), intent(in) :: src
   integer, intent(inout) :: pos
   real(kind=dp), allocatable :: mat(:,:)
   integer :: c
   real(kind=dp), allocatable :: rhs(:,:)

   mat = parse_term(src, pos)
   do
      call skip_ws(src, pos)
      if (pos > len_trim(src)) exit

      c = iachar(src(pos:pos))
      if (c == iachar("+") .or. c == iachar("-")) then
         pos = pos + 1
         rhs = parse_term(src, pos)
         if (c == iachar("+")) then
            mat = element_op(mat, rhs, "+")
         else
            mat = element_op(mat, rhs, "-")
         end if
      else
         exit
      end if
   end do
end function parse_expression

recursive function parse_term(src, pos) result(mat)
   character(len=*), intent(in) :: src
   integer, intent(inout) :: pos
   real(kind=dp), allocatable :: mat(:,:)
   integer :: c
   real(kind=dp), allocatable :: rhs(:,:)

   mat = parse_factor(src, pos)
   do
      call skip_ws(src, pos)
      if (pos > len_trim(src)) exit

      c = iachar(src(pos:pos))
      if (any(c == [iachar("*"), iachar("/"), iachar("@")])) then
         pos = pos + 1
         rhs = parse_factor(src, pos)
         select case (c)
         case (iachar("*"))
            mat = element_op(mat, rhs, "*")
         case (iachar("/"))
            mat = element_op(mat, rhs, "/")
         case (iachar("@"))
            mat = matmul(mat, rhs)
         end select
      else
         exit
      end if
   end do
end function parse_term

recursive function parse_factor(src, pos) result(mat)
   character(len=*), intent(in) :: src
   integer         , intent(inout) :: pos
   real(kind=dp), allocatable     :: mat(:,:)
   character(len=64) :: id
   integer :: m, n, ios, nt

   nt = len_trim(src)
   call skip_ws(src, pos)

   if (pos > nt) then
      call report_error("syntax error – operand expected")
      allocate(mat(0,0))
      return
   end if

   if (src(pos:pos) == "(") then
      pos = pos + 1
      mat = parse_expression(src, pos)
      call expect_char(src, pos, ")")
      return
   end if

   id = get_identifier(src, pos)
   call skip_ws(src, pos)
   nt = len_trim(src)

   if (pos <= nt) then
      if (src(pos:pos) == "(") then
         if (trim(id) /= "runif") then
            call report_error("only runif(m,n) is allowed")
            allocate(mat(0,0))
            return
         end if
         pos = pos + 1
         m = int(get_number(src, pos))
         call expect_char(src, pos, ",")
         n = int(get_number(src, pos))
         call expect_char(src, pos, ")")
         mat = runif(m, n)
         return
      end if
   end if

   read(id, *, iostat = ios) n
   if (ios == 0) then
      allocate(mat(1,1))
      mat = n
   else
      mat = get_variable(trim(id))
   end if
end function parse_factor

function runif(m, n) result(r)
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: r(:,:)
   allocate(r(m, n))
   call random_number(r)
end function runif

function element_op(a, b, op) result(r)
   real(kind=dp), intent(in)  :: a(:,:), b(:,:)
   character(len=1), intent(in) :: op
   real(kind=dp), allocatable :: r(:,:)
   logical :: a_scalar, b_scalar
   real(kind=dp) :: sa, sb
   integer :: m, n

   a_scalar = size(a,1) == 1 .and. size(a,2) == 1
   b_scalar = size(b,1) == 1 .and. size(b,2) == 1

   if (a_scalar .and. .not. b_scalar) then
      m = size(b,1); n = size(b,2)
   else if (b_scalar .and. .not. a_scalar) then
      m = size(a,1); n = size(a,2)
   else
      if (any(shape(a) /= shape(b))) then
         call report_error("shape mismatch")
         allocate(r(0,0))
         return
      end if
      m = size(a,1); n = size(a,2)
   end if
   allocate(r(m,n))

   select case (op)
   case ("+")
      if (a_scalar) then
         sa = a(1,1); r = sa + b
      else if (b_scalar) then
         sb = b(1,1); r = a + sb
      else
         r = a + b
      end if
   case ("-")
      if (a_scalar) then
         sa = a(1,1); r = sa - b
      else if (b_scalar) then
         sb = b(1,1); r = a - sb
      else
         r = a - b
      end if
   case ("*")
      if (a_scalar) then
         sa = a(1,1); r = sa * b
      else if (b_scalar) then
         sb = b(1,1); r = a * sb
      else
         r = a * b
      end if
   case ("/")
      if (a_scalar) then
         sa = a(1,1); r = sa / b
      else if (b_scalar) then
         sb = b(1,1); r = a / sb
      else
         r = a / b
      end if
   end select
end function element_op

subroutine show_matrix(a, tag)
   real(kind=dp), intent(in) :: a(:,:)
   character(len=*), intent(in) :: tag
   integer :: i, m, n

   m = size(a, 1)
   n = size(a, 2)
   write(*, '(a," = [",i0,"x",i0,"]")') trim(tag), m, n
   do i = 1, m
      write(*, "(*(1x,f8.3))") a(i,:)
   end do
   print*
end subroutine show_matrix

function get_variable(name) result(v)
   character(len=*), intent(in) :: name
   real(kind=dp), allocatable :: v(:,:)
   integer :: k

   do k = 1, n_vars
      if (vars(k)%name == name) then
         v = vars(k)%val
         return
      end if
   end do
   call report_error("undefined variable: " // trim(name))
   allocate(v(0,0))
end function get_variable

subroutine set_variable(name, val)
   character(len=*), intent(in) :: name
   real(kind=dp), intent(in) :: val(:,:)
   integer :: k

   do k = 1, n_vars
      if (vars(k)%name == name) then
         call copy_matrix(val, vars(k)%val)
         return
      end if
   end do

   if (n_vars == max_vars) then
      call report_error("symbol table full")
      return
   end if
   n_vars = n_vars + 1
   vars(n_vars)%name = name
   call copy_matrix(val, vars(n_vars)%val)
end subroutine set_variable

subroutine copy_matrix(src, dst)
   real(kind=dp), intent(in) :: src(:,:)
   real(kind=dp), allocatable :: dst(:,:)

   if (allocated(dst)) deallocate(dst)
   allocate(dst(size(src,1), size(src,2)))
   dst = src
end subroutine copy_matrix

subroutine skip_ws(s, p)
   character(len=*), intent(in) :: s
   integer         , intent(inout) :: p
   integer :: n

   n = len_trim(s)
   do while (p <= n)
      if (s(p:p) /= " ") exit
      p = p + 1
   end do
end subroutine skip_ws

function get_identifier(s, p) result(id)
   character(len=*), intent(in) :: s
   integer         , intent(inout) :: p
   character(len=64) :: id
   integer :: q, n
   character(len=*), parameter :: ok = &
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

   n = len_trim(s)
   q = p

   do while (q <= n)
      if (verify(s(q:q), ok) == 0) then
         q = q + 1
      else
         exit
      end if
   end do

   id = s(p:q-1)
   p  = q
end function get_identifier

function get_number(s, p) result(x)
   character(len=*), intent(in) :: s
   integer         , intent(inout) :: p
   real(kind=dp) :: x
   integer :: q, n
   character(len=64) :: tmp

   n = len_trim(s)
   q = p

   do while (q <= n)
      if (s(q:q) == "," .or. s(q:q) == ")") exit
      q = q + 1
   end do

   tmp = s(p:q-1)
   read(tmp, *) x
   p = q
end function get_number

subroutine expect_char(s, p, ch)
   character(len=*), intent(in) :: s
   integer, intent(inout) :: p
   character(len=1), intent(in) :: ch

   call skip_ws(s, p)
   if (s(p:p) /= ch) then
      call report_error('syntax error – expected "'//ch//'""')
      return
   end if
   p = p + 1
end subroutine expect_char

end module mat_mod
