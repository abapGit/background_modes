class ZCL_BG_FACTORY definition
  public
  create private

  global friends ZCL_BG_INJECTOR .

public section.

  class-methods GET_OBJECTS
    returning
      value(RI_OBJECTS) type ref to ZIF_BG_OBJECTS .
  class-methods GET_TRANSPORTS
    returning
      value(RI_TRANSPORTS) type ref to ZIF_BG_TRANSPORTS .
protected section.
private section.

  class-data GI_OBJECTS type ref to ZIF_BG_OBJECTS .
  class-data GI_TRANSPORTS type ref to ZIF_BG_TRANSPORTS .
ENDCLASS.



CLASS ZCL_BG_FACTORY IMPLEMENTATION.


  METHOD get_objects.

    IF gi_objects IS INITIAL.
      CREATE OBJECT gi_objects TYPE zcl_bg_objects.
    ENDIF.

    ri_objects = gi_objects.

  ENDMETHOD.


  METHOD get_transports.

    IF gi_transports IS INITIAL.
      CREATE OBJECT gi_transports TYPE zcl_bg_transports.
    ENDIF.

    ri_transports = gi_transports.

  ENDMETHOD.
ENDCLASS.
