CLASS zcl_abapgit_tran_to_bran DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
protected section.

  methods IS_RELEVANT
    importing
      !IV_MAIN type DEVCLASS
      !IT_OBJECTS type E071_T
    returning
      value(RV_RELEVANT) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_TRAN_TO_BRAN IMPLEMENTATION.


  METHOD is_relevant.

    rv_relevant = abap_false.

    IF lines( it_objects ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_tadir) = VALUE tt_tadir( FOR ls_object IN it_objects (
      pgmid = ls_object-pgmid
      object = ls_object-object
      obj_name = ls_object-obj_name ) ).

    SELECT DISTINCT devclass
      FROM tadir INTO TABLE @DATA(lt_packages)
      FOR ALL ENTRIES IN @lt_tadir
      WHERE pgmid = @lt_tadir-pgmid
      AND object = @lt_tadir-object
      AND obj_name = @lt_tadir-obj_name.

    DATA(lt_sub) = zcl_abapgit_factory=>get_sap_package( iv_main )->list_subpackages( ).
    APPEND iv_main TO lt_sub.

    LOOP AT lt_packages INTO DATA(lv_package).
      rv_relevant = boolc( line_exists( lt_sub[ table_line = lv_package ] ) ).
      IF rv_relevant = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_description.

    rv_description = 'Push: Transport to branch'.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.

    RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_background~run.

    LOOP AT lcl_transports=>list_open( ) INTO DATA(lv_trkorr).

      DATA(lt_objects) = lcl_objects=>to_r3tr( lcl_transports=>list_contents( lv_trkorr ) ).

      IF is_relevant( iv_main    = io_repo->get_package( )
                      it_objects = lt_objects ) = abap_false.
        CONTINUE.
      ENDIF.

* todo
      BREAK-POINT.

* todo, deletions

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
