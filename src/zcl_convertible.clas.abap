CLASS zcl_convertible DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create_by_source_data_object
      IMPORTING
        source_dobj   TYPE any
      RETURNING
        VALUE(result) TYPE REF TO zcl_convertible.

    CLASS-METHODS create_by_source_rtti
      IMPORTING
        source_rtti   TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(result) TYPE REF TO zcl_convertible.

    "! If the target has a compatible type, the convertibility is not checked and the method doesn't fail. <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter target_rtti | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_convertible | <p class="shorttext synchronized" lang="en"></p>
    METHODS check_convertible_to ABSTRACT
      IMPORTING
        target_rtti TYPE REF TO cl_abap_datadescr
      RAISING
        zcx_convertible.

    CLASS-METHODS move
      IMPORTING
        from TYPE any
      EXPORTING
        to   TYPE any
      RAISING
        zcx_convertible.

  PROTECTED SECTION.

    DATA source_rtti        TYPE REF TO cl_abap_datadescr.
    DATA source_struct_rtti TYPE REF TO cl_abap_structdescr.
    DATA source_table_rtti  TYPE REF TO cl_abap_tabledescr.
    DATA source_ref_rtti    TYPE REF TO cl_abap_refdescr.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_convertible IMPLEMENTATION.
  METHOD move.
    " ASSIGNMENT AND CONVERSION RULES
    "
    " ABAP - Keyword Documentation →  ABAP - Programming Language →  Processing Internal Data →  Assignments
    "
    " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_rules.htm
    "
    "   - When the value of a source object (source) is assigned to a destination object (destination), three
    "     cases can be distinguished with respect to the data type:
    "
    "       - source and destination are compatible, which means that all technical type properties match. The
    "         content is transferred from source to destination without being converted. [...]
    "
    "       - source and destination are not compatible but can be converted. The content of source is converted
    "         in accordance with the conversion rules and then transferred to destination. Two data types are
    "         convertible if a conversion rule exists for them. An exception is raised if the content of source
    "         cannot be handled in accordance with the conversion rules. After an exception, the content of
    "         destination depends on the category of the data type. An assignment where a conversion takes place
    "         is always slower than an assignment without conversion.
    "
    "   - If the data objects are neither compatible nor convertible, no assignment can take place. If the
    "     syntax check recognizes this state, a syntax error is raised, otherwise an uncatchable exception is
    "     raised when the program is executed.

    DATA(source_dobj_checker) = create_by_source_data_object( source_dobj = from ).
    DATA(to_rtti) = cl_abap_typedescr=>describe_by_data( to ).
    IF NOT source_dobj_checker->source_rtti->applies_to_data( to ).
      IF     source_dobj_checker->source_rtti->type_kind = to_rtti->type_kind
         AND to_rtti->kind = cl_abap_typedescr=>kind_ref
         AND lcl_convertible_ref=>is_downcast( from_rtti = CAST #( source_dobj_checker->source_rtti )
                                               to_rtti   = CAST #( to_rtti ) ).
        RAISE EXCEPTION TYPE zcx_convertible
          EXPORTING textid = zcx_convertible=>objects_not_compatible.
      ELSE.
        source_dobj_checker->check_convertible_to( CAST #( to_rtti ) ).
      ENDIF.
    ENDIF.
    to = from.
  ENDMETHOD.

  METHOD create_by_source_data_object.
    result = create_by_source_rtti( CAST #( cl_abap_typedescr=>describe_by_data( source_dobj ) ) ).
  ENDMETHOD.

  METHOD create_by_source_rtti.
    CASE source_rtti->kind.
      WHEN cl_abap_datadescr=>kind_elem.
*        IF rtti->type_kind = cl_abap_datadescr=>typekind_enum.
*          srtti = NEW lcl_convertible_enum( ).
*        ELSE.
        result = NEW lcl_convertible_elem( ).
*        ENDIF.
      WHEN cl_abap_datadescr=>kind_struct.
        result = NEW lcl_convertible_struct( ).
        result->source_struct_rtti = CAST cl_abap_structdescr( source_rtti ).
      WHEN cl_abap_datadescr=>kind_table.
        result = NEW lcl_convertible_table( ).
        result->source_table_rtti = CAST cl_abap_tabledescr( source_rtti ).
      WHEN cl_abap_datadescr=>kind_ref.
        result = NEW lcl_convertible_ref( ).
        result->source_ref_rtti = CAST cl_abap_refdescr( source_rtti ).
      WHEN OTHERS.
        " Unsupported (new ABAP features in the future)
        RAISE EXCEPTION TYPE zcx_convertible_no_check.
    ENDCASE.
    result->source_rtti = source_rtti.
  ENDMETHOD.
ENDCLASS.
