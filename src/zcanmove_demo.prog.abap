*&---------------------------------------------------------------------*
*& Report zcanmove_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcanmove_demo.

CLASS lcl_struct DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS is_convertible
      IMPORTING
        source        TYPE any
        target        TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS lcl_struct IMPLEMENTATION.

  METHOD is_convertible.

*data(rtti_struct) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( source ) ).
*
*if rtti_struct->
*  loop at rtti_struct->components assigning FIELD-SYMBOL(<component>).
*    IF lcl_data=>is_convertible( source = <component> )
*  ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_elem_xstring DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS is_convertible
      IMPORTING
        source        TYPE f
        target        TYPE f
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS lcl_elem_xstring IMPLEMENTATION.

  METHOD is_convertible.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_data DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS is_convertible
      IMPORTING
        source        TYPE any
        target        TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS lcl_data IMPLEMENTATION.

  METHOD is_convertible.

    DATA(rtti_source) = cl_abap_typedescr=>describe_by_data( source ).
    DATA(rtti_target) = cl_abap_typedescr=>describe_by_data( target ).

    CASE rtti_source->kind.
        "========================
        " Conversion Rules for Elementary Data Objects
        "========================
      WHEN rtti_source->kind_elem.
        "------------------------
        " Numeric Source Fields
        "------------------------
        CASE rtti_source->type_kind.
          WHEN rtti_source->typekind_int
                OR rtti_source->typekind_int1
                OR rtti_source->typekind_int2
                OR rtti_source->typekind_int8.
            result = abap_true.
          WHEN rtti_source->typekind_packed.
            result = abap_true.
          WHEN rtti_source->typekind_decfloat16
                OR rtti_source->typekind_decfloat34.
            result = abap_true.
          WHEN rtti_source->typekind_float.
            result = abap_true.
        ENDCASE.
        "------------------------
        " Character-Like Source Fields
        "------------------------
        CASE rtti_source->type_kind.
          WHEN rtti_source->typekind_char.
            result = abap_true.
          WHEN rtti_source->typekind_num.
            result = abap_true.
          WHEN rtti_source->typekind_string.
            result = abap_true.
        ENDCASE.
        "------------------------
        " Byte-Like Source Fields
        "------------------------
        CASE rtti_source->type_kind.
          WHEN rtti_source->typekind_hex.
            result = abap_true.
          WHEN rtti_source->typekind_xstring.
            result = abap_true.
        ENDCASE.
        "------------------------
        " Date Fields, Time Fields, and Time Stamp Fields as Source Fields
        "------------------------
        CASE rtti_source->type_kind.
          WHEN rtti_source->typekind_date.
            result = xsdbool( rtti_target->type_kind <> rtti_target->typekind_time ).
          WHEN rtti_source->typekind_time.
            result = xsdbool( rtti_target->type_kind <> rtti_target->typekind_date ).
*          WHEN rtti_source->typekind_utclong.
*            result = lcl_elem_utclong=>is_convertible( source = source
*                                                       target = target ).
        ENDCASE.
        "========================
        " Conversion Rules for Structures
        "========================
      WHEN rtti_source->kind_struct.
            result = lcl_struct=>is_convertible( source = source
                                                 target = target ).
        "========================
        " Conversion Rules for Tables
        "========================
      WHEN rtti_source->kind_table.
        "========================
        " Conversion Rules for Meshes
        "========================
        "      WHEN rtti_source->kind_mesh.
        "========================
        " Conversion Rules for Enumerated Types
        "========================
        "      WHEN rtti_source->kind_enum.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS move_date_to_time FOR TESTING.
    METHODS table_not_convertible FOR TESTING.
    METHODS structure_not_convertible FOR TESTING.

    METHODS common_processing
      IMPORTING
        source TYPE any
        target TYPE any.

ENDCLASS.


CLASS ltc_main IMPLEMENTATION.

  METHOD move_date_to_time.
    " MOVE_NOT_SUPPORTED
    common_processing( source = VALUE d( )
                       target = VALUE t( ) ).
  ENDMETHOD.


  METHOD table_not_convertible.
    " OBJECTS_TABLES_NOT_COMPATIBLE
    common_processing( source = VALUE bapirettab( )
                       target = VALUE sflight_tab1( ) ).
  ENDMETHOD.


  METHOD structure_not_convertible.
    " UC_OBJECTS_NOT_CONVERTIBLE
    common_processing( source = VALUE bapiret2( )
                       target = VALUE sflight( ) ).
  ENDMETHOD.


  METHOD common_processing.

*data(is_compatible) = xsdbool( CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( source ) )->applies_to_data( target ) ).

    DATA(is_convertible) = lcl_data=>is_convertible( source = source
                                                 target = target ).

    DATA(xml) = ``.
    DATA(source_srtti) = zcl_srtti_typedescr=>create_by_data_object( source ).
    DATA(target_srtti) = zcl_srtti_typedescr=>create_by_data_object( target ).
    CALL TRANSFORMATION id
        SOURCE
            source_srtti = source_srtti
            source       = source
            target_srtti = target_srtti
            target       = target
        RESULT
            XML xml
        OPTIONS
            initial_components = 'suppress'.
    CALL FUNCTION 'SO_CALLBACK_RFC'
      DESTINATION 'NONE'
      EXPORTING
        program        = sy-repid
        form           = 'TEST' "to call the sub-routine TEST and TEST_XCHECK
        params         = xml
      EXCEPTIONS
        system_failure = 1.
    IF sy-subrc <> 0 AND is_convertible = abap_true.
      cl_abap_unit_assert=>fail( 'Method says it is convertible but MOVE failed' ).
    ELSEIF sy-subrc = 0 AND is_convertible = abap_false.
      cl_abap_unit_assert=>fail( 'Method says it is not convertible but MOVE succeeded' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.


FORM test_xcheck CHANGING xcheck TYPE flag.
  xcheck = 'X'.
ENDFORM.


FORM test USING xml TYPE string.

  TYPES ty_ref_to_data TYPE REF TO data.

  DATA source_srtti TYPE REF TO zcl_srtti_typedescr.
  DATA target_srtti TYPE REF TO zcl_srtti_typedescr.
  CALL TRANSFORMATION id
      SOURCE
        XML xml
      RESULT
        source_srtti = source_srtti
        target_srtti = target_srtti.

  DATA(source_rtti) = CAST cl_abap_datadescr( source_srtti->get_rtti( ) ).
  DATA(target_rtti) = CAST cl_abap_datadescr( target_srtti->get_rtti( ) ).

  DATA(ref_to_data) = VALUE ty_ref_to_data( ).
  CREATE DATA ref_to_data TYPE HANDLE source_rtti.
  ASSIGN ref_to_data->* TO FIELD-SYMBOL(<source>).
  CREATE DATA ref_to_data TYPE HANDLE target_rtti.
  ASSIGN ref_to_data->* TO FIELD-SYMBOL(<target>).

  CALL TRANSFORMATION id
      SOURCE
        XML xml
      RESULT
        source = <source>
        target = <target>.

  <target> = <source>.

ENDFORM.
