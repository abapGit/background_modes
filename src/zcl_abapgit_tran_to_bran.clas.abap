CLASS zcl_abapgit_tran_to_bran DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_stage,
        comment TYPE zif_abapgit_definitions=>ty_comment,
        stage   TYPE REF TO zcl_abapgit_stage,
      END OF ty_stage .
    TYPES:
      ty_stage_tt TYPE STANDARD TABLE OF ty_stage WITH EMPTY KEY .

    DATA mo_log TYPE REF TO zcl_abapgit_log .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .

    METHODS build_stage
      IMPORTING
        !iv_trkorr      TYPE trkorr
      RETURNING
        VALUE(rt_stage) TYPE ty_stage_tt
      RAISING
        zcx_abapgit_exception .
    METHODS create_or_set_branch
      IMPORTING
        !iv_name TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS push
      IMPORTING
        !iv_trkorr TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS is_relevant
      IMPORTING
        !iv_main           TYPE devclass
        !it_objects        TYPE e071_t
      RETURNING
        VALUE(rv_relevant) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS determine_user_details
      IMPORTING
        !iv_changed_by TYPE xubname
      RETURNING
        VALUE(rs_user) TYPE zif_abapgit_definitions=>ty_git_user .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_TRAN_TO_BRAN IMPLEMENTATION.


  METHOD build_stage.

    TYPES: BEGIN OF ty_changed,
             filename   TYPE string,
             path       TYPE string,
             changed_by TYPE xubname,
           END OF ty_changed.

    DATA: lt_users   TYPE SORTED TABLE OF xubname WITH UNIQUE KEY table_line,
          lt_changed TYPE STANDARD TABLE OF ty_changed WITH DEFAULT KEY.


    DATA(ls_files) = zcl_abapgit_stage_logic=>get( mo_repo ).
    DATA(lt_objects) = zcl_bg_factory=>get_objects( )->to_r3tr( zcl_bg_factory=>get_transports( )->list_contents( iv_trkorr ) ).

    LOOP AT ls_files-local ASSIGNING FIELD-SYMBOL(<ls_local>) WHERE NOT item-obj_type IS INITIAL.
      IF NOT line_exists( lt_objects[ object = <ls_local>-item-obj_type obj_name = <ls_local>-item-obj_name ] ).
        CONTINUE.
      ENDIF.
      mo_log->add_info( |{ <ls_local>-item-obj_type } { <ls_local>-item-obj_name } relevant| ).

      APPEND INITIAL LINE TO lt_changed ASSIGNING FIELD-SYMBOL(<ls_changed>).
      <ls_changed>-changed_by = zcl_abapgit_objects=>changed_by( <ls_local>-item ).
      <ls_changed>-filename   = <ls_local>-file-filename.
      <ls_changed>-path       = <ls_local>-file-path.

      INSERT <ls_changed>-changed_by INTO TABLE lt_users.
    ENDLOOP.

    LOOP AT lt_users INTO DATA(lv_changed_by).
      DATA(ls_comment) = VALUE zif_abapgit_definitions=>ty_comment(
        committer = determine_user_details( lv_changed_by )
        comment   = zcl_bg_factory=>get_transports( )->read_description( iv_trkorr ) ).

      DATA(lo_stage) = NEW zcl_abapgit_stage(
        iv_branch_name = mo_repo->get_branch_name( )
        iv_branch_sha1 = mo_repo->get_sha1_remote( ) ).

      LOOP AT ls_files-local ASSIGNING <ls_local>.
        READ TABLE lt_changed WITH KEY
          path = <ls_local>-file-path
          filename = <ls_local>-file-filename
          changed_by = lv_changed_by
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          mo_log->add_info( |stage: {
            ls_comment-committer-name } {
            <ls_local>-file-path } {
            <ls_local>-file-filename }| ).

          lo_stage->add( iv_path     = <ls_local>-file-path
                         iv_filename = <ls_local>-file-filename
                         iv_data     = <ls_local>-file-data ).
        ENDIF.
      ENDLOOP.

      APPEND VALUE #( comment = ls_comment stage = lo_stage ) TO rt_stage.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_or_set_branch.

    DATA(lt_branches) = zcl_abapgit_factory=>get_branch_overview( mo_repo )->get_branches( ).

    IF NOT line_exists( lt_branches[ name = iv_name ] ).
      mo_repo->create_branch(
        iv_name = iv_name
        iv_from = lt_branches[ is_head = abap_true ]-sha1 ).

      mo_log->add_info( |Branch { iv_name } created| ).
    ENDIF.

    mo_repo->set_branch_name( iv_name ).

  ENDMETHOD.


  METHOD determine_user_details.

    DATA: lo_user_master_record TYPE REF TO zcl_abapgit_user_master_record.


    lo_user_master_record = zcl_abapgit_user_master_record=>get_instance( iv_changed_by ).
    rs_user-name = lo_user_master_record->get_name( ).
    rs_user-email = lo_user_master_record->get_email( ).

*   If no email, fall back to localhost/default email
    IF rs_user-email IS INITIAL.
      rs_user-email = |{ iv_changed_by }@localhost|.
    ENDIF.

*   If no full name maintained, just use changed by user name
    IF rs_user-name IS INITIAL.
      rs_user-name  = iv_changed_by.
    ENDIF.

  ENDMETHOD.


  METHOD is_relevant.

    rv_relevant = abap_false.

    IF lines( it_objects ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_sub) = zcl_abapgit_factory=>get_sap_package( iv_main )->list_subpackages( ).
    APPEND iv_main TO lt_sub.

    LOOP AT it_objects INTO DATA(ls_object).
      DATA(ls_tadir) = zcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = ls_object-object
        iv_obj_name = CONV #( ls_object-obj_name ) ).
      IF line_exists( lt_sub[ table_line = ls_tadir-devclass ] ).
        rv_relevant = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD push.

    create_or_set_branch( |refs/heads/{ iv_trkorr }| ).

    DATA(lt_stage) = build_stage( iv_trkorr ).

    LOOP AT lt_stage INTO DATA(ls_stage).
      mo_log->add_info( |push| ).

* output info here?

      mo_repo->push( is_comment = ls_stage-comment
                     io_stage   = ls_stage-stage ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_description.

    rv_description = 'Push: Transport to branch'.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.

    RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_background~run.

    mo_log = io_log.
    mo_repo = io_repo.

    LOOP AT zcl_bg_factory=>get_transports( )->list_open( ) INTO DATA(lv_trkorr).

      DATA(lt_objects) = zcl_bg_factory=>get_objects( )->to_r3tr( zcl_bg_factory=>get_transports( )->list_contents( lv_trkorr ) ).

      IF is_relevant( iv_main    = io_repo->get_package( )
                      it_objects = lt_objects ) = abap_false.
        io_log->add_info( |{ lv_trkorr } not relevant| ).
        CONTINUE.
      ELSE.
        io_log->add_info( |{ lv_trkorr } relevant| ).
      ENDIF.

      push( lv_trkorr ).

* todo, list objects in commit body
* todo, handle released transports
* todo, handle deletions
* todo, TABU, table contents?
* todo, LIMU object in multiple transports?
* todo, Moving objects
* todo, Objects outside of repo

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
