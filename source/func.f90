module func
  use dfs_mod
  use tree_mod
  use dir
  implicit none
  private
  public :: get_graph, zeros, check, print_graph, search, chinese, kruskal, print_nondirected_graph

  contains
  
  
  subroutine get_graph(G, root)
    integer, allocatable :: G(:,:)
    integer :: n, i, root, k
    character(25) :: input_file
    
    write(*,*) '-- Which file from input/ should I take?'
    read(*,*) input_file

    open(10,file = '../input/' // input_file)

    ! TODO Сделать без считывания размера
    read(10,*) n
    allocate(G(n,n))
    ! Читаем матрицу смежности
    do i = 1, n
      read(10,*) G(i,:)
      where (G(i,:) == 0)
        G(i,:) = -1
      endwhere
    enddo
    read(10,*) root

    close(10)
  end subroutine get_graph

  subroutine print_graph(output_name, G, tree, check)
    integer, intent(in) :: G(:,:), tree(:,:)
    character(len = *), intent(in) :: output_name		
    character(len = :), allocatable :: command
    integer :: i, ii
    logical :: mark, check 
      

    if ( .not. check) then
      !TODO Сделано откровенно ебано
      open(21, file = 'tmp.txt')
      write(21,*) "digraph prof {"
      ! write(21,*) "	ratio = fill;"
      write(21,*) "node [style=filled];"
      write(21,*) "node [shape = circle];"
      do i = 1, size(G(1,:))
        mark = .true.
        do ii = 1, size(G(1,:))
          if (G(i,ii) >= 0) then
            write(21,"(i2,a,i2,a,i2,a)") i," -> ", ii, '[ label = "', G(i,ii), '" ];'
            mark = .false.
          endif
        enddo
        if (mark) then
          write(21,*) i,";"
        endif
      enddo
      write(21,*) "}"
      close(21)
    else
      open(21, file = 'tmp.txt')
      write(21,*) "digraph prof {"
      ! write(21,*) "	ratio = fill;"
      write(21,*) "node [style=filled];"
      write(21,*) "node [shape = circle];"
      do i = 1, size(G(1,:))
        mark = .true.
        do ii = 1, size(G(1,:))
          if (G(i,ii) >= 0 .and. G(i,ii) /= tree(i,ii)) then
            write(21,"(i2,a,i2,a,i2,a)") i," -> ", ii, '[ label = "', G(i,ii), '" ];'
            mark = .false.
          endif
        enddo
      enddo
      do i = 1, size(tree(1,:))
        do ii = 1, size(G(1,:))
          if (tree(i,ii) >= 0) then
            write(21,*) i, '[color = "#1694dd"];'
            write(21,*) ii, '[color = "#1694dd"];' 
            
            write(21,"(i2,a,i2,a,i2,a)") i," -> ", ii, '[ color = "#1694dd", label ="', tree(i,ii), '" ];'
          endif
        enddo
        ! if (maxval(tree(i,:)) < 0 .or. maxval(tree(:,i)) < 0) then
        ! 	write(21,*) i, '[color = "#cc4106"]'
        ! endif
        if (mark) then
          write(21,*) i,";"
        endif
      enddo
      write(21,*) "}"
      close(21)
    endif
    call execute_command_line("dot -Teps -o" // trim(dir_name) // trim(output_name) // " tmp.txt")
    call execute_command_line('rm -f tmp.txt')
  end subroutine print_graph

  subroutine print_nondirected_graph(output_name, G)
    integer, allocatable, intent(in) :: G(:,:)
     character(len = *), intent(in) :: output_name		
     character(len = :), allocatable :: command
     integer :: i, ii 
     logical :: mark

    open(21, file = 'tmp.txt')
    write(21,*) "graph prof {"
    ! write(21,*) "	ratio = fill;"
    write(21,*) "node [style=filled];"
    write(21,*) "node [shape = circle];"
    do i = 1, size(G(1,:))
      mark = .true.
      do ii = i, size(G(1,:))
        mark = .false.
        if (G(i,ii) >= 0) then
          write(21,"(i2,a,i2,a,i2,a)") i," -- ", ii, '[ label = "', G(i,ii), '" ];'
        endif
      enddo
      if (mark) then
        write(21,*) i,";"
      endif
    enddo
    write(21,*) "}"
    close(21)
  call execute_command_line("dot -Teps -o" // trim(dir_name) // trim(output_name) // " tmp.txt")
  call execute_command_line('rm -f tmp.txt')
  end subroutine print_nondirected_graph

  subroutine zeros(G_in, root, G_out, G_out_zeros, min_weights)
    integer, allocatable, intent(in) :: G_in(:,:)
    integer, allocatable :: G_out(:,:), G_out_zeros(:,:), min_weights(:)	
    integer :: i, root, min
     
    allocate(min_weights(size(G_in,1)))
    min_weights = 0
    G_out = G_in
    
    ! Граф с новой весовой функцией
    do i = 1, size(G_out(1,:))
      if (i /= root) then
        min = minval(G_out(:,i), G_out(:,i) >= 0) 
        where ( G_out(:,i) >= 0  )
          G_out(:,i) = G_out(:,i) - min
        end where
        min_weights(i) = min
      endif
    enddo

    G_out_zeros = G_out
    
    ! Граф на нулевых рёбрах
    where (G_out_zeros /= 0)
      G_out_zeros = -1
    end where
  end subroutine zeros

  function check(G_in, root)
    integer, dimension(:,:), intent(in) :: G_in
    integer :: root
    logical :: check
    
    integer, dimension(:), allocatable :: color, entry, leave
    integer :: MAX_N, i
    
    
    MAX_N = size(G_in(:,1))
    allocate(color(MAX_N), entry(MAX_N), leave(MAX_N))
    
    call dfs(G_in, root, color, entry, leave)
    
    check = .true.
    do i = 1, MAX_N
      if ( color(i) == 0 ) then
        check = .false.
        exit
      endif
    enddo
  end function check

  subroutine search(G_in, root, comp, num_comp, root_comp)
    integer, allocatable, intent(in) :: G_in(:,:)
    integer, intent(in) :: root
    integer, allocatable :: G(:,:)
    integer, allocatable :: comp(:,:)
    integer, dimension(:), allocatable :: order, color, entry, leave
    logical, dimension(:), allocatable :: smth
    integer :: i, ii, j, tmp, num_comp, root_comp
    
    
    tmp = size(G_in,1)
    allocate(order(tmp), color(tmp), entry(tmp), leave(tmp), smth(tmp), comp(tmp,tmp))
    order = 0
    color = 0
    entry = 0
    leave = 0
    comp = 0
    smth = .false.
    
    G = G_in
    ! Для каждой непосещённой вершины запускаем dfs и записываем время выхода.
    do i = 1, tmp
      if (order(i) == 0) then
        call dfs(G, i, color, entry, leave)
        do ii = 1, tmp
          if (color(ii) <= 0 .and. order(ii) == 0) then
            order(ii) = leave(ii)
          endif
        enddo
      endif
    enddo

    ! Получили список вершин с временами выхода, осталось запускать на транспонированном графе
    ! в порядке уменьшения, вычёркивая полученные при каждом проходе
    
    ! Получаем транспонированный граф
    G = transpose(G_in)
    
    j = 1
    do while (.not. all(smth))
      tmp = maxval(order, .not. smth)
      do i = 1, size(order)
        if (order(i) == tmp .and. .not. smth(i)) then
          ii = i
        endif
      enddo
      
      ! Решили, что следующая вершина ii, и понеслась
      call dfs(G, ii, color, entry, leave)
    
      ! парсим результат
      ii = 0
      do i = 1, size(color)
        if (color(i) == -1) then
          ii = ii + 1
          G(i,:) = -1
          G(:,i) = -1 ! вычёркиваем посещённый элемент (принадлежащие одной компаненте) 
          comp(ii,j) = i ! приписываем компоненте связности
          smth(i) = .true. ! чтобы не учитывать при сортировки
          if (i == root) then
            root_comp = j
          endif
        endif
      enddo
      j = j + 1 ! Меняем компоненту связности
    enddo
    num_comp = j - 1
  end subroutine
          
  recursive subroutine chinese(G_in, root, tree)
    integer, allocatable, intent(in) :: G_in(:,:)
    integer :: root
    integer, allocatable :: tree(:,:)

    integer, allocatable :: G(:,:), zeros_G(:,:), new_weight_G(:,:), comp(:,:), condensed_G(:,:)
    integer, allocatable :: roots_comp(:), empty_G(:,:), tree_get(:,:), tmp_G(:,:), min_weights(:)
    integer :: i, ii, j, jj, m_1, m_2, n, tmp,i_mem_1,i_mem_2, ii_mem_1,ii_mem_2
    logical :: mark = .false.
    integer :: num_comp, root_comp

    G = G_in
    


    
                                    
                                    
    allocate(empty_G(size(G,1),size(G,1)))
    allocate(tree(size(G,1),size(G,1)))
    allocate(tmp_G(size(G,1),size(G,1)))
    tree = -1
    

    ! Получаем граф с новой весовой функцией и на нулевых рёбрах
    call zeros(G, root, new_weight_G, zeros_G, min_weights)

    
      
    if (.not. check(zeros_G, root)) then
      
      !! Строим конденсацию и получаем новый граф, где рёбра между компанентами сильной связности
      ! взяты наименьшие

      ! Получаем вершины с номером компоненты, к которым они принадлежат
      ! num_comp (число компонент связности)
      ! root_comp (номер компоненты связности, куда попал корень)
      ! comp (матрица компонент)
      call search(zeros_G, root, comp, num_comp, root_comp)
      
      
      ! Генерируем пустую матрицу графа конденсации
      allocate(condensed_G(num_comp,num_comp))
      condensed_G = -1

      ! Создаём пустую матрицу рёбер, которые не входят в компоненты связности
      empty_G = -1
      
      ! Добавляем минимальные рёбра между компонентами связности
      
      
      ! Выбрали компоненту связности
      do j = 1, num_comp
        ! Ищем рёбра между компонентами с номерами j и jj
        do jj = j + 1, num_comp
          m_1 = -1 
          m_2 = -1
          ! Ищем узел в компоненте связности j (i)
          do i = 1, size(comp,1)
            if (comp(i,j) == 0 ) then
              exit
            endif	
            ! Теперь узел в компоненте связности jj (ii)	
            do ii = 1, size(comp,1)
              if (comp(ii,jj) == 0) then
                exit
              endif	
                            ! Проверяем, наличие и минимальноость ребра
              if ((new_weight_G(comp(i,j),comp(ii,jj)) <= m_1 .and. new_weight_G(comp(i,j),comp(ii,jj)) >= 0) &
               .or. (m_1 < 0 .and. new_weight_G(comp(i,j),comp(ii,jj)) >= 0)) then
                m_1 = new_weight_G(comp(i,j),comp(ii,jj))
                i_mem_1 = comp(i,j)
                ii_mem_1 = comp(ii,jj)
              endif
              if ((new_weight_G(comp(ii,jj),comp(i,j)) <= m_2 .and. new_weight_G(comp(ii,jj),comp(i,j)) >= 0) &
               .or. (m_2 < 0 .and. new_weight_G(comp(ii,jj),comp(i,j)) >= 0)) then
                m_2 = new_weight_G(comp(ii,jj),comp(i,j))
                i_mem_2 = comp(i,j)
                ii_mem_2 = comp(ii,jj)
              endif
            enddo
          enddo
          if (m_1 /= -1) then
            condensed_G(j, jj) = m_1
            empty_G(i_mem_1, ii_mem_1) = m_1
          endif
          if (m_2 /= -1) then 
            condensed_G(jj, j) = m_2
            empty_G(ii_mem_2, i_mem_2) = m_2
          endif
        enddo
      enddo
      
      
            
            
            
      ! Снова вызываем процедуру на графе из компонент связности
      
      
      call chinese(condensed_G, root_comp, tree_get)


! ***************************************************************************************************			
        ! Находим корни в каждой компоненте связности и заодно добавляем рёбра в наше дерево
    ! Выбираем компоненту связности
    
    allocate(roots_comp(size(tree_get,1)))

    do i = 1, size(tree_get,1)
      ! Выбераем вторую компонентысвязноости
      do ii = 1, size(tree_get,1)
        if (tree_get(i,ii) >= 0) then
          ! Идём по элементам компоненты связности
          do j = 1, size(comp,1)
            if (comp(j,i) == 0 ) then
              exit
            endif
            do jj = 1, size(comp,1)
              if (comp(jj,ii) == 0 ) then
                exit
              endif
              if (empty_G(comp(j,i), comp(jj,ii)) >= 0) then
                roots_comp(ii) = comp(jj,ii)
                tree(comp(j,i), comp(jj,ii)) = empty_G(comp(j,i), comp(jj,ii))
              endif
            enddo
          enddo
        endif
      enddo
    enddo
  
      
      
    
    ! Находим корень компоненты связности, в которой лежит корень
    roots_comp(root_comp) = root
     
    
    
    
    
    ! ************Тут дерево ещё правильное********************** проблема на j = 1
    ! Строим кончательное дерево, путём добавления в него деревьев на компонентах связности
    tmp_G = -1
    do j = 1, num_comp
      ! Строим подграфы для компонент связности
      do i = 1, size(G,2)
        if (comp(i,j) == 0 ) then
          exit
        endif
        do ii = i + 1, size(G,2)		
          if (comp(ii,j) == 0 ) then
            exit
          endif
          tmp_G(comp(i,j),comp(ii,j)) = zeros_G(comp(i,j),comp(ii,j))
          tmp_G(comp(ii,j),comp(i,j)) = zeros_G(comp(ii,j),comp(i,j))		
        enddo
      enddo
      ! Для каждого подграфа (компоненты связности) запускаем dfs из корня и доавляем в конечное дерево
      
      
      call search_tree(tmp_G, roots_comp(j), tree)
      
    enddo
    ! Восстанавливаем веса рёбер
    do i = 1, size(tree,1)
      do ii = 1, size(tree,1)
        if (tree(ii,i) >= 0) then
        tree(ii,i) = tree(ii,i) + min_weights(i)
        endif
      enddo
    enddo
    



  else
      
      ! Возвращаем дерево в качестве ответа
      call search_tree(zeros_G, root, tree)
      do i = 1, size(tree,1)
        do ii = 1, size(tree,1)
          if (tree(ii,i) >= 0) then
            tree(ii,i) = tree(ii,i) + min_weights(i)
          endif
        enddo
      enddo
  endif

    
  end subroutine chinese

  subroutine kruskal(G)
    integer, allocatable :: G(:,:), tree(:,:)
    logical, allocatable :: b(:)
    integer :: n, i, ii, j, jj, min

    n = size(G,1)

    
    allocate(tree(n,n),b(n))
    tree = -1
    b = .false.
    b(1) = .true.
    do while (.not. all(b))
      min = sum(G, G > 0)
      do i = 1, n
          if (b(i)) then
            do ii = 1, n
              if (.not. b(ii) .and. G(i,ii) > 0 .and. G(i,ii) < min) then
                min = G(i,ii)
                j = i
                jj = ii
              endif
            enddo
          endif
      enddo	
      b(jj) = .true.	
      tree(j,jj) = G(j,jj)
      tree(jj,j) = G(jj,j)
    enddo
  G = tree
  end subroutine kruskal
end module func