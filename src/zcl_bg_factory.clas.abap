CLASS zcl_bg_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_bg_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_objects
      RETURNING
        VALUE(ri_objects) TYPE REF TO zif_bg_objects .
    CLASS-METHODS get_transports
      RETURNING
        VALUE(ri_transports) TYPE REF TO zif_bg_transports .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_objects TYPE REF TO zif_bg_objects .
    CLASS-DATA gi_transports TYPE REF TO zif_bg_transports .
ENDCLASS.



CLASS ZCL_BG_FACTORY IMPLEMENTATION.


  METHOD get_objects.

    IF gi_objects IS INITIAL.
      gi_objects = NEW zcl_bg_objects( ).
    ENDIF.

    ri_objects = gi_objects.

  ENDMETHOD.


  METHOD get_transports.

    IF gi_transports IS INITIAL.
      gi_transports = NEW zcl_bg_transports( ).
    ENDIF.

    ri_transports = gi_transports.

  ENDMETHOD.
ENDCLASS.
