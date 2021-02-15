module dir

implicit none

character(len=64) :: dir_name ! путь к выходным данным
character(len=64) :: base_dir_name = '../output/'

contains
subroutine dir_init()

	character(8) date
	character(10) time
	character(21) unique_name

	call system("if [ -d " // trim(base_dir_name) // " ]; then echo ''; else mkdir " // trim(base_dir_name) // "; fi")

	call date_and_time ( DATE=date, TIME=time )
	
	unique_name = date // '_' // time(1:6)
	
	dir_name = trim(base_dir_name) // trim(unique_name) // '/'
	
	call system('mkdir ' // trim(dir_name))

	! open(99,file = trim(dir_name) // 'debug.txt', status = 'NEW')
	open(99, file = trim(dir_name) // 'debug.txt')
	
	write(*,*) 'Output is written to ', dir_name
	
end subroutine dir_init

end module dir