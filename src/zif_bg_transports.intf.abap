INTERFACE zif_bg_transports
  PUBLIC .

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

ENDINTERFACE.
