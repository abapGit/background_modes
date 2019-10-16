CLASS zcl_bg_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS set_objects
      IMPORTING
        !ii_objects TYPE REF TO zif_bg_objects .
    CLASS-METHODS set_transports
      IMPORTING
        !ii_transports TYPE REF TO zif_bg_transports .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BG_INJECTOR IMPLEMENTATION.


  METHOD set_objects.

    zcl_bg_factory=>gi_objects = ii_objects.

  ENDMETHOD.


  METHOD set_transports.

    zcl_bg_factory=>gi_transports = ii_transports.

  ENDMETHOD.
ENDCLASS.
