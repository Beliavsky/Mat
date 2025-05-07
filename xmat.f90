program demo
   use mat_mod
   implicit none

   character(len=256) :: line

   ! Initial examples
   call eval_print("a = runif(3, 3)")
   call eval_print("b = a + 1")
   call eval_print("c = a @ b")
   call eval_print("c")

   ! Prompt user for input
   print *, "Enter expressions below. Empty line to quit."
   do
      write(*,"(a)", advance="no") ">> "
      read(*,"(A)") line
      if (line == "") exit
      call eval_print(line)
   end do
end program demo
