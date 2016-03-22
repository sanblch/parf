MODULE parf
  USE options
  USE instancesets
  USE forests
  USE parallel
  USE bitvectors
  USE utilities
  IMPLICIT NONE
  TYPE (datadescription), POINTER :: datadesc
  TYPE (forest), POINTER :: rfptr
  INTEGER :: fill_pass
  LOGICAL :: last_pass
CONTAINS
  FUNCTION parse(isptr, data, size, var)
    TYPE (instanceset), POINTER :: isptr
    INTEGER :: size, var
    DOUBLE PRECISION, DIMENSION(size) :: data
    LOGICAL :: parse
    CHARACTER(LEN=linelen) :: origline, line
    INTEGER :: line_no, attribute_count, cat_count, instance_count, cat_start
    INTEGER :: contvar_count, catvar_count, ignorevar_count
    INTEGER :: pos, pos2, pass, i, attr, attr2, j
    CHARACTER(LEN=tokenlen) :: cat_name
    CHARACTER :: delimiter
    LOGICAL :: statement_attribute, statement_ignored
    LOGICAL :: eof, err
    INTEGER :: parse_line_result, constant_attrs
    LOGICAL :: loaddd ! load data description
    INTEGER :: skip_attr
    LOGICAL, ALLOCATABLE :: usedvars(:)

    parse = .FALSE.
    loaddd = .NOT.ASSOCIATED(isptr%dd)
      attribute_count = var
      contvar_count = var - 1
      catvar_count = 1
      ignorevar_count = 0
      cat_count = 2
      
      instance_count = size / var    
      isptr%dd => new_datadescription()
      isptr%relation_name = "harddrive"
      isptr%dd%contvar_count = contvar_count
      isptr%dd%catvar_count = catvar_count
      opts%class_attribute_num = attribute_count
      ALLOCATE (isptr%dd%attributes(attribute_count),&
        & isptr%dd%categories(cat_count),&
        & usedvars(attribute_count), isptr%missing_count(attribute_count))
      isptr%constant_attr => new_bitvector(attribute_count)
      usedvars = opts%used_default
      usedvars(opts%class_attribute_num) = .FALSE.
      isptr%missing_count = 0
      skip_attr = attribute_count + 1 ! i.e. don't skip.
      IF (opts%split_node_size.LT.0) THEN
        opts%split_node_size = -opts%split_node_size * instance_count / 100
      END IF
      IF (opts%split_node_size.GT.instance_count) THEN
        opts%split_node_size = instance_count
      END IF
      isptr%missing_data => new_bitvector2(instance_count, &
        & attribute_count)
      ALLOCATE (isptr%contvars(instance_count, contvar_count),&
        & isptr%catvars(instance_count, catvar_count),&
        & isptr%ignorevars(instance_count, ignorevar_count),&
        & isptr%dd%usedconts(attribute_count - 1),&
        & isptr%dd%usedvars(attribute_count - 1))
      DO i = 1, attribute_count
        IF (i.EQ.attribute_count) THEN                       
          write(cat_name,'(A5)') "failure"
          isptr%dd%attributes(i)%name = cat_name
          isptr%dd%attributes(i)%cat_start = 1
          isptr%dd%attributes(i)%cat_count = 2
          isptr%dd%attributes(i)%mapping = 1
        ELSE
          write(cat_name,'(A6 I2)') "double", i
          isptr%dd%attributes(i)%name = cat_name
          isptr%dd%attributes(i)%cat_start = 0
          isptr%dd%attributes(i)%cat_count = 1
          isptr%dd%attributes(i)%mapping = i
          isptr%dd%usedvars(i) = i
          isptr%dd%usedconts(i) = i
        END IF
      END DO
      DO i = 1, cat_count
        write(isptr%dd%categories(i)%name, '(I1)') i - 1
	    isptr%dd%categories(i)%attribute = attribute_count
	    isptr%dd%categories(i)%weight = 1
	  END DO
	  DO i = 1, instance_count
	    isptr%catvars(i, 1) = &
	      & data(i * attribute_count) + 1
	    DO j = 1, contvar_count
	      isptr%contvars(i, j) = &
	      & data((i - 1) * attribute_count + j)
	    END DO
	  END DO
    CALL index_usedconts(isptr%dd)
    parse = .TRUE.
  END FUNCTION parse
END MODULE parf

SUBROUTINE start()
  USE parf
  IMPLICIT NONE
  CALL par_init()
  CALL opts_default()
  NULLIFY(testset, trainset, datadesc, rfptr)
END SUBROUTINE start

SUBROUTINE load_train_data(data, size, var)
  USE parf
  IMPLICIT NONE
  INTEGER :: size, var
  DOUBLE PRECISION, DIMENSION(size) :: data
  opts%trainset = "trainset"
  trainset => new_instanceset(trainset_type)
  IF(.NOT.parse(trainset, data, size, var)) CALL clean()
  CALL fix_num_prox(UBOUND(trainset%catvars, 1))
  datadesc => trainset%dd
  trainset%classes => trainset%catvars(:, &
    & datadesc%attributes(opts%class_attribute_num)%mapping)
END SUBROUTINE load_train_data

SUBROUTINE load_test_data(data, size, var)
  USE parf
  IMPLICIT NONE
  INTEGER :: size, var
  DOUBLE PRECISION, DIMENSION(size) :: data
  opts%testset = "testset"
  testset => new_instanceset(testset_type)
  testset%dd => datadesc
  IF(.NOT.parse(testset, data, size, var)) CALL clean()
END SUBROUTINE load_test_data

SUBROUTINE load_trainset(filename)
  USE parf
  IMPLICIT NONE
  CHARACTER(*) :: filename
  opts%trainset = filename
  trainset => new_instanceset(trainset_type)
  IF(.NOT.parse_arff(trainset, opts%trainset)) CALL clean()
  CALL fix_num_prox(UBOUND(trainset%catvars, 1))
  datadesc => trainset%dd
  trainset%classes => trainset%catvars(:, &
    & datadesc%attributes(opts%class_attribute_num)%mapping)
END SUBROUTINE load_trainset

SUBROUTINE load_testset(filename)
  USE parf
  IMPLICIT NONE
  CHARACTER(*) :: filename
  opts%testset = filename
  testset => new_instanceset(testset_type)
  testset%dd => datadesc
  IF(.NOT.parse_arff(testset, opts%testset)) CALL clean()
END SUBROUTINE load_testset

SUBROUTINE load_trees(filename)
  USE parf
  IMPLICIT NONE
  CHARACTER(*) :: filename
  CALL free_instanceset(trainset)
  CALL free_datadescription(datadesc)
  CALL free_forest(rfptr)
  NULLIFY(trainset, datadesc, rfptr)    
  trainset => new_instanceset(trainset_type)
  opts%load_forest = filename
  CALL load_forest(rfptr, datadesc)
END SUBROUTINE load_trees

SUBROUTINE save_trees(filename)
  USE parf
  IMPLICIT NONE
  CHARACTER(*) :: filename
  opts%save_forest = filename
  CALL save_forest(rfptr)
END SUBROUTINE save_trees

SUBROUTINE calculate()
  USE parf
  IMPLICIT NONE
  CALL count_classes(trainset)
  IF (opts%fill_passes.NE.0) THEN
    CALL calculate_rough_fills(trainset)
    CALL fill_missing_rough(trainset)
  END IF
  CALL allocate_importance_arrays(trainset)
  CALL init_bootstraps(trainset)
  DO
    CALL get_num_split_variables(datadesc)
    CALL zero_importance_arrays()
    fill_pass = 1
    DO WHILE (fill_pass.LE.MAX(1, opts%fill_passes)) ! at least 1 pass
    ! this is a while loop and not a for loop,
    ! to allow early exit in case proximities can't be calculated
      CALL sort_and_rank(trainset, fill_pass.GT.1)
      rfptr => new_forest(trainset)
      last_pass = fill_pass.GE.opts%fill_passes &
        & .AND.opts%redo_with_important_vars.EQ.0 &
        & .AND.opts%redo_with_significant_vars.EQ.0
      IF (last_pass) THEN
        CALL calc_training_error(trainset)
        IF (LEN_TRIM(opts%testset).NE.0) THEN
          CALL classify_instanceset(testset, rfptr)
          testset%classes => testset%estimated_class
        END IF
      END IF
      IF (fill_pass.LT.opts%fill_passes.OR.opts%last_prox_required) THEN
        CALL calculate_proximities(rfptr, trainset)
        IF (opts%calc_test_prox.AND.last_pass) THEN
          ! test set proximities only on the very last pass
          CALL calculate_proximities(rfptr, testset)
        END IF
      END IF
      IF (fill_pass.NE.MAX(1, opts%fill_passes)) THEN ! each pass but last
        CALL fill_missing_by_prox(trainset)
        CALL free_forest(rfptr)
      END IF
      fill_pass = fill_pass + 1
    END DO
    ! redo with most important variables?
    CALL finalize_importance_arrays(trainset)
    IF (opts%redo_with_important_vars.NE.0) THEN
      opts%redo_with_important_vars = 0 ! redo just once
    ELSE
      EXIT
    END IF
  END DO
END SUBROUTINE calculate

SUBROUTINE predict()
  USE parf
  IMPLICIT NONE
  IF (LEN_TRIM(opts%load_forest).GT.0) THEN
    trainset%classes => trainset%estimated_class
    CALL fix_num_prox(UBOUND(trainset%estimated_class, 1))
    IF (LEN_TRIM(opts%testset).GT.0) THEN
      CALL classify_instanceset(testset, rfptr)
      testset%classes => testset%estimated_class
    END IF  
    IF (opts%last_prox_required) THEN
      CALL calculate_proximities(rfptr, trainset)
      IF (opts%calc_test_prox) THEN
        CALL calculate_proximities(rfptr, testset)
      END IF
    END IF
  ENDIF
  opts%test_confusion = "acm"
  CALL process_confusion_matrix(testset, opts%test_confusion)
END SUBROUTINE predict

SUBROUTINE opts_default()
  USE options  
  opts%do_graphics = .FALSE.
  opts%calc_test_prox = .FALSE.
  opts%summary = .TRUE.
  opts%graphics = ""
  opts%graphics_term = ""
  opts%fill_passes = 1 ! 0 none, 1 rough, n>1 full with n passes
  opts%redo_with_important_vars = 0
  opts%redo_with_important_vars = 0
  opts%class_attribute_num = -1 ! -1 last, -2 new
  opts%new_class_quantity = 1.0
  opts%split_variables = -1 ! -1 means sqrt(non-ignored-vars)
  opts%split_variables_important = -1 ! -1 means sqrt(non-ignored-vars)
  opts%split_node_size = 2 ! i.e. split every node you can
  opts%split_ratio_limit = 0 ! i.e. any split is okay
  opts%usedvars = ""
  opts%class_attribute = ""
  opts%positive_category = ""
  opts%unusedvars = ""
  opts%label = ""
  opts%test_results = ""
  opts%test_arff = ""
  opts%train_test_arff = ""
  opts%big_catvar_cat_count = 12
  opts%big_catvar_iterations = 0 ! 2 ** (big_catvar_cat_count - 1)
  opts%trainset = ""
  opts%class_weights = ""
  opts%testset = ""
  opts%num_trees = MAX(par_processes, 100)
  opts%dump_forest = ""
  opts%save_forest = ""
  opts%load_forest = ""
  opts%fast_importances = ""
  opts%importances = ""
  opts%interaction = ""
  opts%case_importances = ""
  opts%train_outliers = ""
  opts%test_outliers = ""
  opts%train_confusion = ""
  opts%test_confusion = ""
  opts%train_votes = ""
  opts%test_votes = ""
  opts%verbose = .FALSE.
  opts%verboseall = .FALSE.
  opts%num_prox = 0
  opts%prox_for_prot = 0
  opts%num_scale = 0
  opts%scaling_divergence = 10
  opts%num_prot = 0
  opts%train_scaling = ""
  opts%test_scaling = ""
  opts%proto_scaling = ""
  opts%prototypes = ""
  opts%prototype_analysis = ""
  opts%outlier_cutoff = -HUGE(1.0)
  
  opts%used_default = (LEN_TRIM(opts%unusedvars).NE.0).OR. &
    & (LEN_TRIM(opts%usedvars).EQ.0)
END SUBROUTINE opts_default

SUBROUTINE clean()
  USE parf
  IMPLICIT NONE
  CALL free_forest(rfptr)
  CALL free_instanceset(trainset)
  CALL free_instanceset(testset)
  CALL free_datadescription(datadesc)
  CALL par_finalize()
END SUBROUTINE clean
