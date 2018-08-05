CLASS zcl_bg_transports DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bg_transports .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BG_TRANSPORTS IMPLEMENTATION.


  METHOD zif_bg_transports~list_contents.

    DATA(lt_list) = zif_bg_transports~list_sub( iv_trkorr ).

    ASSERT lines( lt_list ) > 0.

    SELECT DISTINCT pgmid object obj_name
      FROM e071
      INTO CORRESPONDING FIELDS OF TABLE rt_list
      FOR ALL ENTRIES IN lt_list
      WHERE trkorr = lt_list-table_line.

  ENDMETHOD.


  METHOD zif_bg_transports~list_open.

    SELECT trkorr FROM e070
      INTO TABLE rt_list
      WHERE trfunction = 'K'
      AND trstatus = 'D'
      AND korrdev = 'SYST'
      AND strkorr = ''.

  ENDMETHOD.


  METHOD zif_bg_transports~list_sub.

    APPEND iv_trkorr TO rt_list.

    SELECT trkorr FROM e070 APPENDING TABLE rt_list
      WHERE strkorr = iv_trkorr.

  ENDMETHOD.


  METHOD zif_bg_transports~read_description.

    SELECT SINGLE as4text FROM e07t
      INTO rv_description
      WHERE trkorr = iv_trkorr AND langu = sy-langu.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
