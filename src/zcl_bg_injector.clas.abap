class ZCL_BG_INJECTOR definition
  public
  create private
  for testing .

public section.

  class-methods SET_OBJECTS
    importing
      !II_OBJECTS type ref to ZIF_BG_OBJECTS .
  class-methods SET_TRANSPORTS
    importing
      !II_TRANSPORTS type ref to ZIF_BG_TRANSPORTS .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_BG_INJECTOR IMPLEMENTATION.


  METHOD set_objects.

    zcl_bg_factory=>gi_objects = ii_objects.

  ENDMETHOD.


  METHOD set_transports.

    zcl_bg_factory=>gi_transports = ii_transports.

  ENDMETHOD.
ENDCLASS.
