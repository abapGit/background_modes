*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_transports DEFINITION.

  PUBLIC SECTION.
    TYPES: ty_list_tt TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY.

    CLASS-METHODS:
      read_description
        IMPORTING
          iv_trkorr             TYPE trkorr
        RETURNING
          VALUE(rv_description) TYPE string,
      list_contents
        IMPORTING iv_trkorr      TYPE trkorr
        RETURNING VALUE(rt_list) TYPE e071_t,
      list_sub
        IMPORTING iv_trkorr      TYPE trkorr
        RETURNING VALUE(rt_list) TYPE ty_list_tt,
      list_open
        RETURNING VALUE(rt_list) TYPE ty_list_tt.

ENDCLASS.

CLASS lcl_transports IMPLEMENTATION.

  METHOD read_description.

    SELECT SINGLE as4text FROM e07t
      INTO rv_description
      WHERE trkorr = iv_trkorr AND langu = sy-langu.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD list_contents.

    DATA(lt_list) = list_sub( iv_trkorr ).

    ASSERT lines( lt_list ) > 0.

    SELECT DISTINCT pgmid object obj_name
      FROM e071
      INTO CORRESPONDING FIELDS OF TABLE rt_list
      FOR ALL ENTRIES IN lt_list
      WHERE trkorr = lt_list-table_line.

  ENDMETHOD.

  METHOD list_sub.

    APPEND iv_trkorr TO rt_list.

    SELECT trkorr FROM e070 APPENDING TABLE rt_list
      WHERE strkorr = iv_trkorr.

  ENDMETHOD.

  METHOD list_open.

    SELECT trkorr FROM e070
      INTO TABLE rt_list
      WHERE trfunction = 'K'
      AND trstatus = 'D'
      AND korrdev = 'SYST'
      AND strkorr = ''.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_objects DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      to_r3tr
        IMPORTING
          it_list        TYPE e071_t
        RETURNING
          VALUE(rt_list) TYPE e071_t.

ENDCLASS.

CLASS lcl_objects IMPLEMENTATION.

  METHOD to_r3tr.

    LOOP AT it_list INTO DATA(ls_list).
      IF ls_list-pgmid = 'R3TR'.
        APPEND ls_list TO rt_list.
      ELSE.
        DATA(ls_tadir) = cl_wb_object_type=>get_tadir_from_limu(
          p_object   = ls_list-object
          p_obj_name = ls_list-obj_name ).

        APPEND CORRESPONDING #( ls_tadir ) TO rt_list.
      ENDIF.
    ENDLOOP.

    SORT rt_list BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_list COMPARING pgmid object obj_name.

  ENDMETHOD.

ENDCLASS.
