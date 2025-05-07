module mat_mod
use kind_mod, only: dp
implicit none
private
public :: eval_print

integer, parameter :: max_vars = 100, len_name = 32

type :: var_t
   character(len=len_name) :: name = ''
   real(kind=dp), allocatable :: val(:,:)
end type var_t

type(var_t) :: vars(max_vars)
integer :: nvars = 0

contains

subroutine eval_print(line)
   character(len=*), intent(in) :: line
   character(len=:), allocatable :: l, lhs, rhs, tag
   real(kind=dp),  allocatable :: result(:,:)
   integer :: pos

   l = trim(adjustl(line))
   if (l == '') return

   call split_assignment(l, lhs, rhs)
   pos = 1
   result = parse_expr(rhs, pos)

   if (lhs /= '') call set_var(trim(lhs), result)

   if (lhs == '') then
      tag = 'ans'
   else
      tag = lhs
   end if

   call show_matrix(result, tag)
end subroutine eval_print

subroutine split_assignment(str, lhs, rhs)
   character(len=*), intent(in) :: str
   character(len=:), allocatable :: lhs, rhs
   integer :: p

   p = index(str, '=')
   if (p == 0) then
      lhs = ''
      rhs = str
   else
      lhs = trim(adjustl(str(:p-1)))
      rhs = trim(adjustl(str(p+1:)))
   end if
end subroutine split_assignment

recursive function parse_expr(src, pos) result(mat)
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
      if (c == iachar('+') .or. c == iachar('-')) then
         pos = pos + 1
         rhs = parse_term(src, pos)
         if (c == iachar('+')) then
            mat = element_op(mat, rhs, '+')
         else
            mat = element_op(mat, rhs, '-')
         end if
      else
         exit
      end if
   end do
end function parse_expr

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
      if (any(c == [iachar('*'), iachar('/'), iachar('@')])) then
         pos = pos + 1
         rhs = parse_factor(src, pos)
         select case (c)
         case (iachar('*'))
            mat = element_op(mat, rhs, '*')
         case (iachar('/'))
            mat = element_op(mat, rhs, '/')
         case (iachar('@'))
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

   if (pos > nt) stop 'syntax error – operand expected'

   if (src(pos:pos) == '(') then
      pos = pos + 1
      mat = parse_expr(src, pos)
      call expect_char(src, pos, ')')
      return
   end if

   id = get_identifier(src, pos)
   call skip_ws(src, pos)
   nt = len_trim(src)

   if (pos <= nt) then
      if (src(pos:pos) == '(') then
         if (trim(id) /= 'runif') stop 'only runif(m,n) is allowed'
         pos = pos + 1
         m = int(get_number(src, pos))
         call expect_char(src, pos, ',')
         n = int(get_number(src, pos))
         call expect_char(src, pos, ')')
         mat = runif(m, n)
         return
      end if
   end if

   read(id, *, iostat = ios) n
   if (ios == 0) then
      allocate(mat(1,1))
      mat = n
   else
      mat = get_var(trim(id))
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
      if (any(shape(a) /= shape(b))) stop 'shape mismatch'
      m = size(a,1); n = size(a,2)
   end if
   allocate(r(m,n))

   select case (op)
   case ('+')
      if (a_scalar) then
         sa = a(1,1); r = sa + b
      else if (b_scalar) then
         sb = b(1,1); r = a + sb
      else
         r = a + b
      end if
   case ('-')
      if (a_scalar) then
         sa = a(1,1); r = sa - b
      else if (b_scalar) then
         sb = b(1,1); r = a - sb
      else
         r = a - b
      end if
   case ('*')
      if (a_scalar) then
         sa = a(1,1); r = sa * b
      else if (b_scalar) then
         sb = b(1,1); r = a * sb
      else
         r = a * b
      end if
   case ('/')
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
   integer :: i, j, m, n

   m = size(a, 1)
   n = size(a, 2)
   write(*, '(a," = [",i0,"x",i0,"]")') trim(tag), m, n
   do i = 1, m
      write(*, '(999(1x,g0.6))') (a(i,j), j = 1, n)
   end do
end subroutine show_matrix

function get_var(name) result(v)
   character(len=*), intent(in) :: name
   real(kind=dp), allocatable :: v(:,:)
   integer :: k

   do k = 1, nvars
      if (vars(k)%name == name) then
         v = vars(k)%val
         return
      end if
   end do
   stop 'undefined variable: '//trim(name)
end function get_var

subroutine set_var(name, val)
   character(len=*), intent(in) :: name
   real(kind=dp), intent(in) :: val(:,:)
   integer :: k

   do k = 1, nvars
      if (vars(k)%name == name) then
         call copy_matrix(val, vars(k)%val)
         return
      end if
   end do

   if (nvars == max_vars) stop 'symbol table full'
   nvars = nvars + 1
   vars(nvars)%name = name
   call copy_matrix(val, vars(nvars)%val)
end subroutine set_var

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
      if (s(p:p) /= ' ') exit
      p = p + 1
   end do
end subroutine skip_ws

function get_identifier(s, p) result(id)
   character(len=*), intent(in) :: s
   integer         , intent(inout) :: p
   character(len=64) :: id
   integer :: q, n
   character(len=*), parameter :: ok = &
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'

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
      if (s(q:q) == ',' .or. s(q:q) == ')') exit
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
   if (s(p:p) /= ch) stop 'syntax error – expected "'//ch//'"'
   p = p + 1
end subroutine expect_char

end module mat_mod
