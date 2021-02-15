module tree_mod
  implicit none
  private
  public :: search_tree
  
  integer, dimension(:), allocatable :: tree, color
  
  contains

  subroutine search_tree(G_in, v, tree_in)
    integer, dimension(:,:), intent(in) :: G_in
    integer :: v
    integer, dimension(:,:) ::tree_in
    
    integer :: n, i

    

    n = size(G_in(1,:))
    allocate(tree(n), color(n))
    tree = 0
    color = 0
    
    call search_tree_help(G_in, v)
    
    do i = 1, size(tree)
      if (tree(i) /= 0) then
        tree_in(tree(i),i) = G_in(tree(i),i)
      endif
    enddo
    deallocate(tree, color)
  end subroutine search_tree


  recursive subroutine search_tree_help(G_in, v)
    integer, dimension(:,:), intent(in) :: G_in
    integer :: i, v
    
    color(v) = 1
    !TODO Слишком много раз смотрю размер
    do i = 1, size(G_in(:,1))
      if (G_in(v,i) >= 0 .and. color(i) == 0) then
        tree(i) = v	
        call search_tree_help(G_in, i)
      endif	
    enddo
    color(v) = -1
  end subroutine search_tree_help
end module tree_mod