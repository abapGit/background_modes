CLASS zcl_abapgit_bran_to_tran DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_BRAN_TO_TRAN IMPLEMENTATION.


  METHOD zif_abapgit_background~get_description.

    rv_description = 'Pull: Branch to transport'.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.

    RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_background~run.

* todo
    BREAK-POINT.

  ENDMETHOD.
ENDCLASS.
