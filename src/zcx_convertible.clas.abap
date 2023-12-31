CLASS zcx_convertible DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL.

    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.

    CONSTANTS move_not_supported            TYPE sotr_conc VALUE 'E944E2A3C0081EEEA785C2C5296D4B44'.
    CONSTANTS move_oref_not_convertible     TYPE sotr_conc VALUE 'E944E2A3C0081EEEA788A21DA6FC8DAD'.
    CONSTANTS objects_move_not_supported    TYPE sotr_conc VALUE 'E944E2A3C0081EEEA7851C0CA75D0AB9'.
    CONSTANTS objects_not_compatible        TYPE sotr_conc VALUE 'E944E2A3C0081EEEA78516C9807FCAAA'.
    CONSTANTS objects_tables_not_compatible TYPE sotr_conc VALUE 'C53FB6F765531EDEA8D2AD30F49387AC'.
    CONSTANTS move_iref_not_convertible     TYPE sotr_conc VALUE 'C53FB6F765531EDEA8D2F7169E5E47FC'.
    CONSTANTS move_iref_to_oref             TYPE sotr_conc VALUE 'C53FB6F765531EDEA8D37C48C1384873'.
    CONSTANTS move_interface_not_supported  TYPE sotr_conc VALUE 'C53FB6F765531EDEA8D445118B814935'.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_convertible IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
  ENDMETHOD.

  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.

  METHOD get_text.
    result = SWITCH #( textid
                       WHEN move_not_supported THEN
                         'move_not_supported'
                       WHEN move_oref_not_convertible THEN
                         'move_oref_not_convertible'
                       WHEN objects_move_not_supported THEN
                         'objects_move_not_supported'
                       WHEN objects_not_compatible THEN
                         'objects_not_compatible'
                       WHEN objects_tables_not_compatible THEN
                         'objects_tables_not_compatible' ).
  ENDMETHOD.
ENDCLASS.
