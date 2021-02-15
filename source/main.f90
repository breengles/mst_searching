program probability 
use wood
use wood_pot, only : search_forest_pot => search_forest
use func
use dir
implicit none

integer, allocatable :: G(:,:), forest(:,:), tree(:,:), forest_pot(:,:), G_t(:,:), V(:,:)
integer :: root, k, i, j
logical :: check_potential
character(3) :: wood_answer, mst_answer

! Инициализация папок
call dir_init

! Считываем граф и корень из файла (0 -- нулевое ребро, -1 -- отсутствие ребра)
call get_graph(G, root)

check_potential = .true.
G_t = transpose(G)


do i = 1, size(G,1)
  do j = 1, size(G,1)
    if (G(i,j) /= G_t(i,j) .and. check_potential) then
      check_potential = .false.
    endif
  enddo
enddo

! Ask the number of connected components
write(*,*) '-- k = ?'
read(*,*) k
if (check_potential) then
  write(*,*) '-- Built for potential graph!'
  allocate(V(size(G,1),size(G,1)))
  
  do i = 1, size(G,1)
    if (G(i,i) > 0) then
      V(i,:) = G(i,:) - G(i,i)
    else
      V(i,:) = G(i,:)
    endif
    V(i,i) = -1
  enddo


  call search_forest_pot(G, k, forest_pot)

  call print_graph('origin_wood_pot.eps', V, forest_pot, .true.)
  call print_graph('wood_pot.eps', forest_pot, forest_pot, .false.)
  call print_nondirected_graph('nondirected_graph.eps', G)

else

  write(*,*) '-- Built for non-potential graph!'
  write(*,*) '-- Should I build MST?'
  read(*,*) mst_answer
  if (mst_answer(:1) == "y") then
    if (check(G, root)) then
      write(*,*) '-- MST succesfully was built!'
      call chinese(G, root, tree)
      call print_graph('G.eps', G, tree, .false.)
      call print_graph('origin_tree.eps', G, tree, .true.)
  
      ! open(20, file = trim(dir_name) // 'tree.txt', status = 'NEW')
      ! do i = 1, size(G,1)
      ! 	write(20,*) tree(i,:)
      ! enddo
      ! close(20)

    else
      write(*,*) '-- Несвязный с таким корнем!'
    endif
  endif

  write(*,*) '-- Should I build forest?'
  read(*,*) wood_answer

  if (wood_answer(:1) == "y") then
    call search_forest(G, k, forest)

    call print_graph('forest.eps', forest, forest, .false.)
    call print_graph('origin_wood.eps', G, forest, .true.)
  endif
endif


end program probability