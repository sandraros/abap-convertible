*"* use this source file for your ABAP unit test classes

"! The method SYNTAX_CHECK of class LTH_ABAP_CODE is copyright (MIT License)
"! https://github.com/sandraros/shrinker (code copied from the method
"! SYNTAX_CHECK of class ZCL_SHRINKER_ABAP_SCAN).
CLASS lth_abap_code DEFINITION.
  PUBLIC SECTION.
    TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_syntax_check,
        itab  TYPE string_table,
        mess  TYPE string,
        lin   TYPE i,
        wrd   TYPE string,
        prog  TYPE syrepid,
        dir   TYPE trdir,
        incl  TYPE string,
        off   TYPE i,
        mid   TYPE trmsg_key,
        subrc TYPE sysubrc,
      END OF ty_syntax_check.
    TYPES:
      BEGIN OF ty_message_of_syntax_check,
        keyword   TYPE trmsg_key-keyword,
        msgnumber TYPE trmsg_key-msgnumber,
      END OF ty_message_of_syntax_check.

    CLASS-METHODS get_message_of_syntax_check
      IMPORTING
        abap_source_code TYPE ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE ty_message_of_syntax_check.

    CLASS-METHODS syntax_check
      IMPORTING
        abap_source_code TYPE ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE ty_syntax_check.

ENDCLASS.


CLASS lth_abap_code IMPLEMENTATION.
  METHOD get_message_of_syntax_check.
    DATA(sc_result) = syntax_check( abap_source_code ).
    result = VALUE #( keyword   = sc_result-mid-keyword
                      msgnumber = sc_result-mid-msgnumber ).
  ENDMETHOD.

  METHOD syntax_check.
    DATA(synt) = VALUE ty_syntax_check( dir = VALUE #( name = '$$DUMMY' subc = '1' fixpt = 'X' uccheck = 'X' ) ).
    " X   VARCL *S   DBAPL *D$  DBNA
    SYNTAX-CHECK FOR abap_source_code MESSAGE synt-mess LINE synt-lin WORD synt-wrd DIRECTORY ENTRY synt-dir INCLUDE synt-incl OFFSET synt-off MESSAGE-ID synt-mid.
    synt-subrc = sy-subrc.
    " SYNTAX-CHECK FOR itab MESSAGE mess LINE lin WORD wrd
    "                  [PROGRAM prog] [DIRECTORY ENTRY dir]
    "                  [WITH CURRENT SWITCHSTATES]
    " ... [INCLUDE incl]
    "     [OFFSET off]
    "     [MESSAGE-ID mid] ...

    result = synt.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_convertible DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS comp_dref_downcast FOR TESTING RAISING cx_static_check.
    METHODS d_to_t FOR TESTING RAISING cx_static_check.
    METHODS deep_struct_diff_number_compon FOR TESTING RAISING cx_static_check.
    METHODS deep_table_diff_number_compon FOR TESTING RAISING cx_static_check.
    METHODS dref_downcast FOR TESTING RAISING cx_static_check.
    METHODS dref_upcast FOR TESTING RAISING cx_static_check.
    METHODS oref_downcast FOR TESTING RAISING cx_static_check.
    METHODS oref_upcast FOR TESTING RAISING cx_static_check.
    METHODS t_to_d FOR TESTING RAISING cx_static_check.
    METHODS t_to_string FOR TESTING RAISING cx_static_check.

    DATA d                TYPE d.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.
    DATA ref_to_cl_ixml   TYPE REF TO cl_ixml.
    DATA ref_to_data      TYPE REF TO data.
    DATA ref_to_object    TYPE REF TO object.
    DATA string           TYPE string.
    DATA t                TYPE t.
ENDCLASS.


CLASS ltc_convertible IMPLEMENTATION.
  METHOD comp_dref_downcast.
    FIELD-SYMBOLS <structure_1> TYPE any.
    FIELD-SYMBOLS <structure_2> TYPE any.

    DATA:
      BEGIN OF structure_1,
        ref_to_data TYPE REF TO data,
      END OF structure_1.
    DATA:
      BEGIN OF structure_2,
        ref_to_abap_bool TYPE REF TO abap_bool,
      END OF structure_2.

    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                    ` )
                                                                   ( `DATA:                                      ` )
                                                                   ( `  BEGIN OF structure_1,                    ` )
                                                                   ( `    ref_to_data TYPE REF TO data,          ` )
                                                                   ( `  END OF structure_1.                      ` )
                                                                   ( `DATA:                                      ` )
                                                                   ( `  BEGIN OF structure_2,                    ` )
                                                                   ( `    ref_to_abap_bool TYPE REF TO abap_bool,` )
                                                                   ( `  END OF structure_2.                      ` )
                                                                   ( `structure_2 = structure_1.                 ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword = 'MESSAGE' msgnumber = 'GXD' ) ).

    TRY.
        zcl_convertible=>move( EXPORTING from = structure_1 IMPORTING to = structure_2 ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_not_compatible )
                                          msg = 'OBJECTS_NOT_COMPATIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD d_to_t.
    cl_abap_unit_assert=>assert_false(
        act = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( t ) )->applies_to_data( d )
        msg = 'T is not compatible with D, so APPLIES_TO_DATA should return false' ).
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.       ` )
                                                                   ( `DATA d TYPE d.` )
                                                                   ( `DATA t TYPE t.` )
                                                                   ( `t = d.        ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword = 'MESSAGE' msgnumber = 'GXD' ) ).

    TRY.
        zcl_convertible=>move( EXPORTING from = d IMPORTING to = t ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_not_supported )
                                          msg = 'MOVE_NOT_SUPPORTED is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD deep_struct_diff_number_compon.
    DATA:
      BEGIN OF from_struct,
        comp1 TYPE string,
      END OF from_struct.
    DATA:
      BEGIN OF to_struct,
        comp2 TYPE string,
        comp3 TYPE string,
      END OF to_struct.

    TRY.
        zcl_convertible=>move( EXPORTING from = from_struct IMPORTING to = to_struct ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_move_not_supported )
                                          msg = 'OBJECTS_MOVE_NOT_SUPPORTED is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD deep_table_diff_number_compon.
    TYPES:
      BEGIN OF ty_from_table_line,
        comp1 TYPE string,
      END OF ty_from_table_line.
    TYPES:
      BEGIN OF ty_to_table_line,
        comp2 TYPE string,
        comp3 TYPE string,
      END OF ty_to_table_line.

    DATA from_table TYPE TABLE OF ty_from_table_line.
    DATA to_table   TYPE TABLE OF ty_to_table_line.

    TRY.
        zcl_convertible=>move( EXPORTING from = from_table IMPORTING to = to_table ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_move_not_supported )
                                          msg = 'OBJECTS_MOVE_NOT_SUPPORTED is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD dref_downcast.
    DATA ref_to_data      TYPE REF TO data.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.

    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                     ` )
                                                                   ( `DATA ref_to_data      TYPE REF TO data.     ` )
                                                                   ( `DATA ref_to_abap_bool TYPE REF TO abap_bool.` )
                                                                   ( `ref_to_abap_bool = ref_to_data.             ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword = 'MESSAGE' msgnumber = 'GXD' ) ).

    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_data IMPORTING to = ref_to_abap_bool ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_not_compatible )
                                          msg = 'OBJECTS_NOT_COMPATIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD dref_upcast.
    ref_to_data = ref_to_abap_bool.
    zcl_convertible=>move( EXPORTING from = ref_to_abap_bool IMPORTING to = ref_to_data ).
  ENDMETHOD.

  METHOD oref_downcast.
    " The execution of the dynamic version of this code would lead to short dump MOVE_OREF_NOT_CONVERTIBLE (not catchable).
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                 ` )
                                                                   ( `DATA ref_to_object  TYPE REF TO object. ` )
                                                                   ( `DATA ref_to_cl_ixml TYPE REF TO cl_ixml.` )
                                                                   ( `ref_to_cl_ixml = ref_to_object.         ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword = 'MESSAGE' msgnumber = 'GXD' ) ).

    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_object IMPORTING to = ref_to_cl_ixml ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>MOVE_OREF_NOT_CONVERTIBLE )
                                          msg = 'MOVE_OREF_NOT_CONVERTIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD oref_upcast.
*    DATA ref_to_cx_root TYPE REF TO CX_root.
*    DATA ref_to_cx_sy_conversion_error TYPE REF TO cx_sy_conversion_error.
    ref_to_object = ref_to_cl_ixml.
    zcl_convertible=>move( EXPORTING from = ref_to_cl_ixml IMPORTING to = ref_to_object ).
  ENDMETHOD.

  METHOD t_to_d.
    cl_abap_unit_assert=>assert_false(
        act = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( d ) )->applies_to_data( t )
        msg = 'D is not compatible with T, so APPLIES_TO_DATA should return false' ).
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.       ` )
                                                                   ( `DATA d TYPE d.` )
                                                                   ( `DATA t TYPE t.` )
                                                                   ( `t = d.        ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword = 'MESSAGE' msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = t IMPORTING to = d ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_not_supported )
                                          msg = 'MOVE_NOT_SUPPORTED is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD t_to_string.
    string = t.
    zcl_convertible=>move( EXPORTING from = t IMPORTING to = string ).
  ENDMETHOD.
ENDCLASS.


"! The compatibility check is done via the method APPLIES_TO_DATA of CL_ABAP_DATADESCR.
CLASS ltc_compatible DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS downcast FOR TESTING RAISING cx_static_check.
    METHODS two_structures FOR TESTING RAISING cx_static_check.
    METHODS upcast FOR TESTING RAISING cx_static_check.
    METHODS oref_cast FOR TESTING RAISING cx_static_check.

    METHODS assert_compatible
      IMPORTING
        source TYPE any
        target TYPE any.
    METHODS assert_not_compatible
      IMPORTING
        source TYPE any
        target TYPE any.
ENDCLASS.


CLASS ltc_compatible IMPLEMENTATION.
  METHOD assert_compatible.
    cl_abap_unit_assert=>assert_true(
        CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( target ) )->applies_to_data( source ) ).
  ENDMETHOD.

  METHOD assert_not_compatible.
    cl_abap_unit_assert=>assert_false(
        CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( target ) )->applies_to_data( source ) ).
  ENDMETHOD.

  METHOD downcast.
    DATA ref_to_data      TYPE REF TO data.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.

    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_data IMPORTING to = ref_to_abap_bool ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_not_compatible )
                                          msg = 'OBJECTS_NOT_COMPATIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD oref_cast.
    DATA ref_to_cl_abap_typedescr TYPE REF TO cl_abap_typedescr.
    DATA ref_to_cl_abap_datadescr TYPE REF TO cl_abap_datadescr.

    DATA(rtti_ref_to_cl_abap_typedescr) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data(
                                                                                               ref_to_cl_abap_typedescr ) ).
*    DATA(rtti_ref_to_cl_abap_typedescr) = CAST cl_abap_objectdescr( CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data(
*                                                                      ref_to_cl_abap_typedescr ) )->get_referenced_type( ) ).
    DATA(rtti_ref_to_cl_abap_datadescr) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data(
                                                                      ref_to_cl_abap_datadescr ) ).
*    DATA(rtti_ref_to_cl_abap_datadescr) = CAST cl_abap_objectdescr( CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data(
*                                                                                               ref_to_cl_abap_datadescr ) )->get_referenced_type( ) ).

    cl_abap_unit_assert=>assert_true( msg = 'Upcast must be allowed'
                                      act = rtti_ref_to_cl_abap_typedescr->applies_to_data( ref_to_cl_abap_datadescr ) ).

*    cl_abap_unit_assert=>assert_true( msg = 'Downcast must be forbidden'
*                                      act = rtti_ref_to_cl_abap_datadescr->applies_to( ref_to_cl_abap_typedescr ) ).
  ENDMETHOD.

  METHOD two_structures.
    DATA:
      BEGIN OF source,
        company TYPE string,
      END OF source.
    DATA:
      BEGIN OF target,
        "! Typo on purpose
        copany TYPE string,
      END OF target.

    source-company = 'A'.
    assert_compatible( source = source
                       target = target ).
    target = source.
  ENDMETHOD.

  METHOD upcast.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.
    DATA ref_to_data      TYPE REF TO data.

    zcl_convertible=>move( EXPORTING from = ref_to_abap_bool IMPORTING to = ref_to_data ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_upcast_downcast DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS upcast_elem_to_data FOR TESTING RAISING cx_static_check.
    METHODS assert_only_upcast_is_possible
      IMPORTING
        source TYPE any
        target TYPE any.
ENDCLASS.


CLASS ltc_upcast_downcast IMPLEMENTATION.
  METHOD assert_only_upcast_is_possible.
    DATA(source_rtti) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( source ) ).
    DATA(target_rtti) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( target ) ).
    cl_abap_unit_assert=>assert_true( target_rtti->applies_to_data( source_rtti ) ).
    cl_abap_unit_assert=>assert_false( source_rtti->applies_to_data( target_rtti ) ).
  ENDMETHOD.

  METHOD upcast_elem_to_data.
    DATA(dobj_of_type_cl_abap_elemdescr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE d( ) ) ).
    DATA(dobj_of_type_cl_abap_datadescr) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( VALUE d( ) ) ).
    assert_only_upcast_is_possible( source = dobj_of_type_cl_abap_elemdescr
                                    target = dobj_of_type_cl_abap_datadescr ).
  ENDMETHOD.
ENDCLASS.
