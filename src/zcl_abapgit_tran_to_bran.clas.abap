CLASS zcl_abapgit_tran_to_bran DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_stage,
        comment TYPE zif_abapgit_git_definitions=>ty_comment,
        stage   TYPE REF TO zcl_abapgit_stage,
      END OF ty_stage .
    TYPES:
      ty_stage_tt TYPE STANDARD TABLE OF ty_stage WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_file,
        filename TYPE string,
        path     TYPE string,
      END OF ty_file .
    TYPES:
      BEGIN OF ty_changed,
        username TYPE xubname,
        files    TYPE STANDARD TABLE OF ty_file WITH EMPTY KEY,
      END OF ty_changed .
    TYPES:
      ty_changed_tt TYPE SORTED TABLE OF ty_changed WITH UNIQUE KEY username .

    DATA mi_log TYPE REF TO zif_abapgit_log .
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
    METHODS determine_changed
      IMPORTING
        !iv_trkorr        TYPE trkorr
      RETURNING
        VALUE(rt_changed) TYPE ty_changed_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_user_details
      IMPORTING
        !iv_changed_by TYPE xubname
      RETURNING
        VALUE(rs_user) TYPE zif_abapgit_git_definitions=>ty_git_user .
    METHODS is_relevant
      IMPORTING
        !iv_main           TYPE devclass
        !it_objects        TYPE e071_t
      RETURNING
        VALUE(rv_relevant) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS push
      IMPORTING
        !iv_trkorr TYPE trkorr
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_tran_to_bran IMPLEMENTATION.


  METHOD build_stage.


    DATA(ls_files) = zcl_abapgit_stage_logic=>get_stage_logic( )->get( mo_repo ).
    DATA(lt_file_status) = zcl_abapgit_repo_status=>calculate( mo_repo ).

    DATA(lt_changed) = determine_changed( iv_trkorr ).

    LOOP AT lt_changed INTO DATA(ls_changed).
      DATA(ls_comment) = VALUE zif_abapgit_git_definitions=>ty_comment(
        committer = determine_user_details( ls_changed-username )
        comment   = zcl_bg_factory=>get_transports( )->read_description( iv_trkorr ) ).

      DATA(lo_stage) = NEW zcl_abapgit_stage( ).

      LOOP AT lt_file_status ASSIGNING FIELD-SYMBOL(<ls_status>)
          WHERE lstate <> zif_abapgit_definitions=>c_state-unchanged.
        READ TABLE ls_changed-files WITH KEY
          path       = <ls_status>-path
          filename   = <ls_status>-filename
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CASE <ls_status>-lstate.
          WHEN zif_abapgit_definitions=>c_state-modified
              OR zif_abapgit_definitions=>c_state-added.
            mi_log->add_info( |stage: {
              ls_comment-committer-name } {
              <ls_status>-path } {
              <ls_status>-filename }| ).

            DATA(lv_data) = ls_files-local[ file-filename = <ls_status>-filename
                                            file-path     = <ls_status>-path ]-file-data.

            lo_stage->add( iv_path     = <ls_status>-path
                           iv_filename = <ls_status>-filename
                           iv_data     = lv_data ).

          WHEN zif_abapgit_definitions=>c_state-deleted.
            mi_log->add_info( |rm: {
              ls_comment-committer-name } {
              <ls_status>-path } {
              <ls_status>-filename }| ).

            lo_stage->rm( iv_path     = <ls_status>-path
                          iv_filename = <ls_status>-filename ).
        ENDCASE.
      ENDLOOP.

      IF lo_stage->count( ) > 0.
        APPEND VALUE #( comment = ls_comment stage = lo_stage ) TO rt_stage.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_or_set_branch.

    DATA(lo_branches) = zcl_abapgit_git_transport=>branches( mo_repo->get_url( ) ).
    DATA(lt_branches) = lo_branches->get_branches_only( ).

    IF NOT line_exists( lt_branches[ name = iv_name ] ).
      mo_repo->create_branch(
        iv_name = iv_name
        iv_from = lt_branches[ is_head = abap_true ]-sha1 ).

      mi_log->add_info( |Branch { iv_name } created| ).
    ENDIF.

    mo_repo->select_branch( iv_name ).

  ENDMETHOD.


  METHOD determine_changed.

    DATA(li_transports) = zcl_bg_factory=>get_transports( ).
    DATA(lv_transport_owner) = li_transports->read_owner( iv_trkorr ).
    DATA(lt_objects) = zcl_bg_factory=>get_objects( )->to_r3tr( li_transports->list_contents( iv_trkorr ) ).
    DATA(lt_file_status) = zcl_abapgit_repo_status=>calculate( mo_repo ).

    LOOP AT lt_file_status ASSIGNING FIELD-SYMBOL(<ls_status>) WHERE obj_type IS NOT INITIAL.
      IF NOT line_exists( lt_objects[ object   = <ls_status>-obj_type
                                      obj_name = <ls_status>-obj_name ] ).
        CONTINUE.
      ENDIF.

      TRY.
          DATA(lv_changed_by) = zcl_abapgit_objects=>changed_by( VALUE #(
            obj_type = <ls_status>-obj_type
            obj_name = <ls_status>-obj_name
            devclass = <ls_status>-package ) ).
        CATCH zcx_abapgit_exception.
* this happens if the object type is not supported by abapGit?
          lv_changed_by = zcl_abapgit_objects_super=>c_user_unknown.
      ENDTRY.

      IF lv_changed_by = zcl_abapgit_objects_super=>c_user_unknown
          AND <ls_status>-lstate = zif_abapgit_definitions=>c_state-deleted.
        lv_changed_by = lv_transport_owner.
      ENDIF.

      INSERT VALUE #( username = lv_changed_by ) INTO TABLE rt_changed.

      ASSIGN rt_changed[ username = lv_changed_by ]-files TO FIELD-SYMBOL(<lt_files>).
      APPEND VALUE #(
        filename   = <ls_status>-filename
        path       = <ls_status>-path
        ) TO <lt_files>.
    ENDLOOP.

  ENDMETHOD.


  METHOD determine_user_details.

    DATA: lo_user_record TYPE REF TO zcl_abapgit_user_record.


    lo_user_record = zcl_abapgit_user_record=>get_instance( iv_changed_by ).
    rs_user-name = lo_user_record->get_name( ).
    rs_user-email = lo_user_record->get_email( ).

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

    DATA(lv_base_branch) = mo_repo->get_selected_branch( ).
    create_or_set_branch( |refs/heads/{ iv_trkorr }| ).

    DATA(lt_stage) = build_stage( iv_trkorr ).

    LOOP AT lt_stage INTO DATA(ls_stage).
      mi_log->add_info( |push| ).

* output info here?

      mo_repo->push( is_comment = ls_stage-comment
                     io_stage   = ls_stage-stage ).
    ENDLOOP.

    mo_repo->select_branch( lv_base_branch ).

  ENDMETHOD.


  METHOD zif_abapgit_background~get_description.

    rv_description = |Push: Transport to branch|.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.

    APPEND VALUE #( key = 'ERRORS_EMAIL' value = '' ) TO ct_settings.

  ENDMETHOD.


  METHOD zif_abapgit_background~run.

    mi_log  = ii_log.
    mo_repo = io_repo.

    LOOP AT zcl_bg_factory=>get_transports( )->list_open( ) INTO DATA(lv_trkorr).

      DATA(lt_objects) = zcl_bg_factory=>get_objects( )->to_r3tr(
        zcl_bg_factory=>get_transports( )->list_contents( lv_trkorr ) ).

      IF is_relevant( iv_main    = io_repo->get_package( )
                      it_objects = lt_objects ) = abap_false.
        ii_log->add_info( |{ lv_trkorr } not relevant| ).
        CONTINUE.
      ELSE.
        ii_log->add_info( |{ lv_trkorr } relevant| ).
      ENDIF.

      push( lv_trkorr ).

* __ 1st priority __
* todo, objects not supported by abapGit
* todo, handle deletions
* todo, Moving objects between transports
* todo, Objects outside of repo
* todo, R3TR->LIMU object in multiple transports
* todo, transport with changes for 2 repos

* __ 2nd priority __
* todo, TABU, table contents
* todo, list objects in commit body
* todo, how to handle released transports

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
