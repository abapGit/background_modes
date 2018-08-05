INTERFACE zif_bg_objects
  PUBLIC .

  CLASS-METHODS:
    to_r3tr
      IMPORTING
        it_list        TYPE e071_t
      RETURNING
        VALUE(rt_list) TYPE e071_t.

ENDINTERFACE.
