module dfs_mod
	implicit none
	private
	public :: dfs
	
	integer, dimension(:), allocatable :: color, entry, leave
	integer :: timer
	contains

	subroutine dfs(G_in, v, color_in, entry_in, leave_in)
		integer, dimension(:,:), intent(in) :: G_in
		integer :: v
		integer, dimension(:) :: color_in, entry_in, leave_in

		integer :: max
		

		max = size(G_in(1,:))
		allocate(color(max), entry(max), leave(max))
		color = 0
		entry = 0
		leave = 0
		timer = 0
		call dfs_help(G_in, v)
		
		! 0 -- не посещена, 1 -- в ней, -1 -- посещена
		color_in = color
		entry_in = entry
		leave_in = leave
		

		deallocate(color, entry, leave)
	end subroutine dfs

	recursive subroutine dfs_help(G_in, v)
		integer, dimension(:,:), intent(in) :: G_in
		integer :: i, v
		
		timer = timer + 1
		entry(v) = timer 
		color(v) = 1
		!TODO Слишком много раз смотрю размер
		do i = 1, size(G_in(:,1))
			if (G_in(v,i) >= 0 .and. color(i) == 0) then
				call dfs_help(G_in, i)
			endif	
		enddo
		timer = timer + 1 
		color(v) = -1
		leave(v) = timer
	end subroutine dfs_help
end module dfs_mod