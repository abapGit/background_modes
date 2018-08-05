class ZCL_BG_OBJECTS definition
  public
  create public .

public section.

  interfaces ZIF_BG_OBJECTS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BG_OBJECTS IMPLEMENTATION.


  METHOD zif_bg_objects~to_r3tr.

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
