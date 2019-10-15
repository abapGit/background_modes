
CLASS ltcl_helper DEFINITION FOR TESTING CREATE PRIVATE FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_tadir,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_tadir.

    TYPES: ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH EMPTY KEY.

    TYPES: BEGIN OF ty_package,
             package TYPE devclass,
             parent  TYPE devclass,
           END OF ty_package.

    TYPES: ty_packages_tt TYPE STANDARD TABLE OF ty_package WITH EMPTY KEY.

    CLASS-METHODS:
      inject
        IMPORTING
          it_tadir    TYPE ty_tadir_tt OPTIONAL
          it_packages TYPE ty_packages_tt OPTIONAL.

    INTERFACES:
      zif_abapgit_tadir PARTIALLY IMPLEMENTED,
      zif_abapgit_sap_package PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.
    DATA:
      mv_package TYPE devclass.

    CLASS-DATA:
      gt_packages TYPE ty_packages_tt,
      gt_tadir    TYPE ty_tadir_tt.

    METHODS: constructor IMPORTING iv_package TYPE devclass OPTIONAL.

ENDCLASS.

CLASS ltcl_helper IMPLEMENTATION.

  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.

  METHOD inject.

    gt_packages = it_packages.
    gt_tadir    = it_tadir.

    zcl_abapgit_injector=>set_tadir( NEW ltcl_helper( ) ).

    LOOP AT it_packages INTO DATA(ls_package).
      zcl_abapgit_injector=>set_sap_package(
        iv_package = ls_package-package
        ii_sap_package = NEW ltcl_helper( ls_package-package ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_subpackages.

    LOOP AT gt_packages INTO DATA(ls_package) WHERE parent = mv_package.
      APPEND ls_package-package TO rt_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_abapgit_tadir~read_single.

    READ TABLE gt_tadir INTO DATA(ls_tadir) WITH KEY
      object = iv_object
      obj_name = iv_obj_name.

    cl_abap_unit_assert=>assert_subrc( ).

    MOVE-CORRESPONDING ls_tadir TO rs_tadir.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_is_relevant DEFINITION DEFERRED.
CLASS zcl_abapgit_tran_to_bran DEFINITION LOCAL FRIENDS ltcl_is_relevant.

CLASS ltcl_is_relevant DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_tran_to_bran.

    METHODS:
      setup,
      empty FOR TESTING RAISING zcx_abapgit_exception,
      nope FOR TESTING RAISING zcx_abapgit_exception,
      sub FOR TESTING RAISING zcx_abapgit_exception,
      outside FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_is_relevant IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD empty.

    DATA(lv_relevant) = mo_cut->is_relevant(
      iv_main    = '$MAIN'
      it_objects = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_false ).

  ENDMETHOD.

  METHOD nope.

    ltcl_helper=>inject( it_tadir = VALUE #( (
      object   = 'CLAS'
      obj_name = 'ZCL_FOOBAR'
      devclass = '$MAIN' ) ) ).

    DATA(lv_relevant) = mo_cut->is_relevant(
      iv_main    = '$MAIN'
      it_objects = VALUE #( (
        object   = 'CLAS'
        obj_name = 'ZCL_FOOBAR' ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_true ).

  ENDMETHOD.

  METHOD sub.

    ltcl_helper=>inject(
      it_tadir = VALUE #( (
        object   = 'CLAS'
        obj_name = 'ZCL_FOOBAR'
        devclass = '$SUB' ) )
      it_packages = VALUE #(
        ( package = '$MAIN' )
        ( package = '$SUB' parent = '$MAIN' ) ) ).

    DATA(lv_relevant) = mo_cut->is_relevant(
      iv_main    = '$MAIN'
      it_objects = VALUE #( (
        object   = 'CLAS'
        obj_name = 'ZCL_FOOBAR' ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_true ).

  ENDMETHOD.

  METHOD outside.

    ltcl_helper=>inject( it_tadir = VALUE #( (
      object   = 'CLAS'
      obj_name = 'ZCL_FOOBAR'
      devclass = '$OTHER' ) ) ).

    DATA(lv_relevant) = mo_cut->is_relevant(
      iv_main    = '$MAIN'
      it_objects = VALUE #( (
        object   = 'CLAS'
        obj_name = 'ZCL_FOOBAR' ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_build_stage DEFINITION DEFERRED.
CLASS zcl_abapgit_tran_to_bran DEFINITION LOCAL FRIENDS ltcl_build_stage.

CLASS ltcl_build_stage DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS FINAL.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_stage_logic PARTIALLY IMPLEMENTED,
      zif_bg_transports PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_tran_to_bran.

    METHODS:
      setup,
      test01 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_build_stage IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
    mo_cut->mi_log = NEW zcl_abapgit_log( ).

    zcl_abapgit_injector=>set_stage_logic( me ).
    zcl_bg_injector=>set_transports( me ).
  ENDMETHOD.

  METHOD zif_abapgit_stage_logic~get.
    rs_files = VALUE #(
      local = VALUE #( ( item-obj_type = 'PROG' item-obj_name = 'ZFOOBAR' ) ) ).
  ENDMETHOD.

  METHOD zif_bg_transports~read_description.
    rv_description = |Hello World|.
  ENDMETHOD.

  METHOD zif_bg_transports~list_contents.
    rt_list = VALUE #( (
      pgmid    = 'R3TR'
      object   = 'PROG'
      obj_name = 'ZFOOBAR' ) ).
  ENDMETHOD.

  METHOD zif_bg_transports~read_owner.
    rv_owner = sy-uname.
  ENDMETHOD.

  METHOD test01.

* todo, ZCL_ABAPGIT_REPO should be changed to implement an interface

*    DATA(lt_result) = mo_cut->build_stage( 'ABC123' ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = lines( lt_result )
*      exp = 1  ).

  ENDMETHOD.

ENDCLASS.
