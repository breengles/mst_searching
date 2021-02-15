module wood_pot
  use func
  use tree_mod
  implicit none
  private
  public :: search_forest
  
  integer, allocatable :: G(:,:), P(:,:), forest_glob(:,:), G_forest(:,:)
  integer :: inf, k, timer, knots
  
  contains

  subroutine search_forest(P_in, k_in, G_forest_out)  
    integer, allocatable, intent(in) :: P_in(:,:)
    integer, intent(in) :: k_in
    integer, allocatable :: min_arg(:,:), forest_in(:,:), G_forest_out(:,:)
    integer :: i, ii, n, min

    ! Делаем входящие параметры глобальными
    n = size(P_in,1)
    allocate(G(n,n))

    do i = 1, n
      if (P_in(i,i) > 0) then
        G(i,:) = P_in(i,:) - P_in(i,i)
      else
        G(i,:) = P_in(i,:)
      endif
      G(i,i) = -1
    enddo
    
    k = n - k_in
    knots = n
    
    P = P_in
    do i = 1, n
      P(i,i) = -1
    enddo
    ! Инициализируем бесконечность
    inf = 0
    do i = 1, n
      do ii = 1, n
        if (G(i,ii) > 0) then 
          inf = inf + G(i,ii)
        endif
      enddo
    enddo
    inf = inf * 10
    
    
    
    ! Генерируем пустой начальный остовный лес
    allocate(forest_in(n,n + 1))
    allocate(min_arg(3,n))
    forest_in = -1
    
    do i = 1, n
      forest_in(i,i) = 0
      forest_in(i,n + 1) = i
      min = minloc(G(i,:), 1, G(i,:) >= 0)

      ! Записываем минимальные веса возможных деревьев
      if (min /= 0) then
        min_arg(1,i) = G(i,min)
      else
        min_arg(1,i) = inf
      endif
      min_arg(2,i) = i
      min_arg(3,i) = min
    enddo

    timer = 1
    if (k_in /= n) then
      call forest_help(forest_in, min_arg)
    else
      forest_glob = forest_in
    endif 
                                                                                        
    ! Восстанавливаем веса и готовим вывод
    allocate(G_forest_out(n,n)); G_forest_out = -1

    do i = 1, size(forest_glob,1)
      do ii = 1, knots
        if (forest_glob(i,ii) > 0) then
          G_forest_out(ii,forest_glob(i,ii)) = G(ii,forest_glob(i,ii))
        endif
      enddo
    enddo


  end subroutine search_forest

  recursive subroutine forest_help(forest, min_arg)
    integer, allocatable, intent(in) :: forest(:,:)
    integer, allocatable :: G_tmp(:,:), G_tmp_t(:,:), forest_out(:,:), min_arg_out(:,:), tree(:,:), min_arg(:,:), b(:), P_tmp(:,:)
    integer :: minweight, weight
    integer :: comp, comp_1, root, m, tmp, i, j, n, ii, jj, active_comp, end_root, min, root_b, tmp_root
    logical :: mark
    
    ! Определяем компоненту связности, для которой надо найти минимумум (она < 0 просто)
    active_comp = minloc(min_arg(1,:), 1)

    !! Строим подграф исходного графа на вершинах принадлежащих выбранной компоненте леса (дереву) active_comp
    
    ! Выясняем сколько вершин в компоненте связности
    tmp = 0
    ! Вектор биекции между двуми графами b
    allocate(b(knots)); b = 0
    do i = 1, knots
      if (forest(active_comp,i) >= 0) then
        tmp = tmp + 1
        b(tmp) = i 
      endif
    enddo
    
    ! Создаём подграф компоненты связности со всеми рёбрами из графа
    allocate(G_tmp(tmp,tmp)); G_tmp = -1
    allocate(P_tmp(tmp,tmp)); P_tmp = -1
    allocate(tree(tmp,tmp)); tree = -1
    
    do i = 1, tmp
      do ii = 1, tmp
        G_tmp(i,ii) = G(b(i),b(ii))
        P_tmp(i,ii) = P(b(i),b(ii))
      enddo
    enddo

    ! Считаем минимум в квадратных скобках для задроченной компоненты
    minweight = inf
    call kruskal(P_tmp)

    do i = 1, tmp
      tree = -1
      call search_tree(P_tmp, i, tree)    
      ! Восстанавливаем веса
      do ii = 1, tmp
        do jj = 1, tmp
          if (tree(ii,jj) > 0) then
            tree(ii,jj) = G_tmp(ii,jj)
          endif
        enddo
      enddo
      tree = transpose(tree)
      ! Добавляем минимальную дугу в наше дерево
      min = inf
      do j = 1, knots
        ! Проверяем: не попали ли мы в нашу же компоненту нашей дугой
        mark = .true.
        if (G(b(i),j) >= 0) then
          do jj = 1, tmp
            if (b(jj) == j) then
              mark = .false.
            endif
          enddo
          if (min >= G(b(i),j) .and. mark) then
            min = G(b(i),j)
            tmp_root = j
          endif
        endif
    enddo 
              

        ! Считаем получившейся вес суммарный
        weight = sum(tree, tree > 0)
        weight = weight + min

        

        ! Записали минимальный из всех получившихся
        if (weight < minweight) then
          minweight = weight
          root = b(i)
          end_root = tmp_root
        endif
        
    ! endif
    enddo
      
    
    ! На выходе имеем минимум внутри первых квадратных скобок и вершину, которую нужно взять как корень ,чтобы получить минимум 
    min_arg(1,active_comp) = minweight
    min_arg(2,active_comp) = root
    min_arg(3,active_comp) = end_root
    deallocate(G_tmp,P_tmp, tree)

    
    !******ура! с обдроченной компонентой закончили***********************************************************
    
    ! Сравниваем полученный вес с теми, что имеют остальные компоненты связности и ищем минимальный

    ! Компонента связности, в которой реализуется минимум
    comp = minloc(min_arg(1,:),1)
    
    ! Вершина, на которой реализуется минимум
    root = min_arg(2, comp)
    
    ! Фиксируем, что эта компонента связности задрочена
    min_arg(1,comp) = -1

    tmp = 0
    ! Вектор биекции между двуми графами b
    b = 0
    do i = 1, knots
      if (forest(comp,i) >= 0) then
        tmp = tmp + 1
        b(tmp) = i 
        if (i == root) then
          root_b = tmp
        endif
      endif
    enddo

    ! Создаём подграф компоненты связности со всеми рёбрами из графа
    allocate(G_tmp(tmp,tmp)); G_tmp = -1
    allocate(P_tmp(tmp,tmp)); P_tmp = -1
    allocate(tree(tmp,tmp)); tree = -1
    do i = 1, tmp
      do ii = 1, tmp
        G_tmp(i,ii) = G(b(i),b(ii))
        P_tmp(i,ii) = P(b(i),b(ii))
      enddo
    enddo

    G_tmp_t = transpose(G_tmp)
    
    call kruskal(P_tmp)

    
    
    call search_tree(P_tmp, root_b, tree)    
    ! Восстанавливаем веса
    do ii = 1, tmp
      do jj = 1, tmp
        if (tree(ii,jj) > 0) then
          tree(ii,jj) = G_tmp(ii,jj)
        endif
      enddo
    enddo
      

    tree = transpose(tree)
    
    ! Ищем компоненту (дерево), в которую увела нас наша дуга
    comp_1 = maxloc(forest(:,min_arg(3,comp)),1) 
    
    ! Считаем суммарный вес, получившегося дерева
    weight = sum(tree, tree > 0) + G(root,min_arg(3,comp)) 
    
    ! Докидываем вес подклеевшейся компоненты
    do i = 1, knots
      if (forest(comp_1,i) > 0) then
        weight = weight + G(i,forest(comp_1,i))
      endif
    enddo
    

    allocate(min_arg_out(3,size(min_arg,2) - 1))

    ! Записываем с маркером в виде отрицательного числа на первое место
    min_arg_out(1,1) = -weight
    min_arg_out(2,1) = root

    
    
    ! Генерируем пустой лес 
    allocate(forest_out(size(forest,1) - 1,knots + 1))
    forest_out = -1
    
    ! Копируем оставшиеся компоненты и минимальные вещи
    m = 0
    do i = 1, size(forest_out,1) - 1
      if ((i + m) /= comp .and. (i + m) /= comp_1) then
        forest_out(i + 1,:) = forest(i + m,:)
        min_arg_out(:,i + 1) = min_arg(:,i + m)
      else
        m = m + 1
        if ((i + m) /= comp .and. (i + m) /= comp_1) then
          forest_out(i + 1,:) = forest(i + m,:)
          min_arg_out(:,i + 1) = min_arg(:,i + m)
        else
          m = m + 1
          forest_out(i + 1,:) = forest(i + m,:)
          min_arg_out(:,i + 1) = min_arg(:,i + m)
        endif
      endif
    enddo

    ! Парсим дерево и подклеиваем
    do i = 1, tmp
      forest_out(1,b(i)) = b(maxloc(tree(i,:), 1))
    enddo

    ! Впихиваем ребро
    forest_out(1, root) = min_arg(3,comp)
    
    ! Копируем подклеянную компоненту
    do i = 1, knots
      if (forest(comp_1,i) >= 0) then
        forest_out(1,i) = forest(comp_1,i)
      endif
    enddo

    ! Записываем новый корень, КОТОРЫЙ РАВЕН КОРНЮ ПОКЛЕЯННОЙ КОМПОНЕНТЫ

    forest_out(1,knots + 1) = forest(comp_1, knots + 1)

    if (timer /= k) then
      timer = timer + 1
      call forest_help(forest_out, min_arg_out)
    else
      forest_glob = forest_out
    endif
  
        

  end subroutine forest_help
end module wood_pot