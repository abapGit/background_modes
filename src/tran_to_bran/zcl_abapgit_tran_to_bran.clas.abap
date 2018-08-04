CLASS zcl_abapgit_tran_to_bran DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_TRAN_TO_BRAN IMPLEMENTATION.


  METHOD zif_abapgit_background~get_description.

    rv_description = 'Push: Transport to branch'.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.

    RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_background~run.

* todo

  ENDMETHOD.
ENDCLASS.
