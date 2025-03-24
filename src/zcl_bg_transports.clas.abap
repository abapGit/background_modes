CLASS zcl_bg_transports DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bg_transports .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bg_transports IMPLEMENTATION.


  METHOD zif_bg_transports~list_contents.

    SELECT SINGLE trkorr FROM e070 INTO @DATA(lv_trkorr) WHERE trkorr = @iv_trkorr.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.

    DATA(lt_list) = zif_bg_transports~list_sub( iv_trkorr ).
    ASSERT lines( lt_list ) > 0.

    SELECT DISTINCT pgmid object obj_name
      FROM e071
      INTO CORRESPONDING FIELDS OF TABLE rt_list
      FOR ALL ENTRIES IN lt_list
      WHERE trkorr = lt_list-table_line.                  "#EC CI_SUBRC

  ENDMETHOD.


  METHOD zif_bg_transports~list_open.

    SELECT trkorr FROM e070
      INTO TABLE rt_list
      WHERE trfunction = 'K'
      AND trstatus = 'D'
      AND korrdev = 'SYST'
      AND strkorr = ''.                                   "#EC CI_SUBRC

  ENDMETHOD.


  METHOD zif_bg_transports~list_sub.

    APPEND iv_trkorr TO rt_list.

    SELECT trkorr FROM e070 APPENDING TABLE rt_list
      WHERE strkorr = iv_trkorr.                          "#EC CI_SUBRC

  ENDMETHOD.


  METHOD zif_bg_transports~read_description.

    SELECT SINGLE as4text FROM e07t
      INTO rv_description
      WHERE trkorr = iv_trkorr AND langu = sy-langu.
    IF sy-subrc <> 0. " If language does not exist try to find another description.
      SELECT SINGLE as4text FROM e07t
           INTO rv_description
           WHERE trkorr = iv_trkorr.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abapgit_desc_exception.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_bg_transports~read_owner.
    SELECT SINGLE as4user INTO @rv_owner
      FROM e070
      WHERE trkorr = @iv_trkorr.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_user_exception.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
