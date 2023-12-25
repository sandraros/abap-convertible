*"* use this source file for your ABAP unit test classes

CLASS lth_implements_intf_with_intf DEFINITION DEFERRED.


INTERFACE lih_any.
ENDINTERFACE.


INTERFACE lih_with_component_intf.
  INTERFACES lih_any.
ENDINTERFACE.


"! Used to understand what level of compatibility is checked by CL_ABAP_DATADESCR->APPLIES_TO_DATA
"! i.e. upcast is not understood as being compatible.
CLASS ltc_applies_to_data DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS comp_dref_down_up_cast FOR TESTING RAISING cx_static_check.
    METHODS d_and_t FOR TESTING RAISING cx_static_check.
    METHODS dref_down_up_cast FOR TESTING RAISING cx_static_check.
    METHODS itab_key FOR TESTING RAISING cx_static_check.
    METHODS itab_key_category FOR TESTING RAISING cx_static_check.
    METHODS oref_down_up_cast FOR TESTING RAISING cx_static_check.
    METHODS struct FOR TESTING RAISING cx_static_check.
    METHODS struct_component_names FOR TESTING RAISING cx_static_check.
    METHODS struct_extra_components FOR TESTING RAISING cx_static_check.
    METHODS struct_flat_char FOR TESTING RAISING cx_static_check.
    METHODS struct_length FOR TESTING RAISING cx_static_check.
    METHODS struct_type FOR TESTING RAISING cx_static_check.
    METHODS struct_flat_char_and_c FOR TESTING RAISING cx_static_check.

    METHODS assert_compatible
      IMPORTING
        source TYPE any
        target TYPE any.

    METHODS assert_not_compatible
      IMPORTING
        source TYPE any
        target TYPE any.
ENDCLASS.


CLASS ltc_convertible DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS comp_dref_upcast FOR TESTING RAISING cx_static_check.
    METHODS dref_to_i_twice FOR TESTING RAISING cx_static_check.
    METHODS dref_upcast FOR TESTING RAISING cx_static_check.
    METHODS oref_upcast_clas_to_clas FOR TESTING RAISING cx_static_check.
    METHODS oref_upcast_clas_to_intf FOR TESTING RAISING cx_static_check.
    METHODS oref_upcast_intf_to_clas FOR TESTING RAISING cx_static_check.
    METHODS oref_upcast_intf_to_intf FOR TESTING RAISING cx_static_check.
    METHODS struct_dref_to_i_twice FOR TESTING RAISING cx_static_check.
    METHODS t_to_string FOR TESTING RAISING cx_static_check.
    METHODS two_structures FOR TESTING RAISING cx_static_check.

    DATA ref_to_class_w_intf_w_intf TYPE REF TO lth_implements_intf_with_intf.
    DATA ref_to_intf_with_intf      TYPE REF TO lih_with_component_intf.
    DATA ref_to_intf                TYPE REF TO lih_any.
    DATA ref_to_abap_bool           TYPE REF TO abap_bool.
    DATA ref_to_cl_ixml             TYPE REF TO cl_ixml.
    DATA ref_to_data                TYPE REF TO data.
    DATA ref_to_object              TYPE REF TO object.
    DATA string                     TYPE string.
    DATA t                          TYPE t.

    METHODS move IMPORTING from TYPE any
                 EXPORTING to   TYPE any
                 RAISING
                           zcx_convertible.
ENDCLASS.


CLASS ltc_convertible_short_dump DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS itab_duplicate_key FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_not_compatible DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS downcast FOR TESTING RAISING cx_static_check.

    DATA d TYPE d.
    DATA t TYPE t.

    METHODS assert_not_compatible
      IMPORTING
        source TYPE any
        target TYPE any.
ENDCLASS.


"! Tests were assignments are not possible because the source data object
"! is not convertible to the target data object.
CLASS ltc_not_convertible DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS comp_dref_downcast FOR TESTING RAISING cx_static_check.
    METHODS d_to_t FOR TESTING RAISING cx_static_check.
    METHODS deep_struct_diff_number_compon FOR TESTING RAISING cx_static_check.
    METHODS deep_table_diff_number_compon FOR TESTING RAISING cx_static_check.
    METHODS dref_downcast FOR TESTING RAISING cx_static_check.
    METHODS oref_downcast_clas_to_clas FOR TESTING RAISING cx_static_check.
    METHODS oref_downcast_clas_to_intf FOR TESTING RAISING cx_static_check.
    METHODS oref_downcast_intf_to_clas FOR TESTING RAISING cx_static_check.
    METHODS oref_downcast_intf_to_intf FOR TESTING RAISING cx_static_check.
    METHODS t_to_d FOR TESTING RAISING cx_static_check.
    METHODS test FOR TESTING RAISING cx_static_check.

    METHODS move IMPORTING from TYPE any
                 EXPORTING to   TYPE any.

    DATA d                TYPE d.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.
    DATA ref_to_cl_ixml   TYPE REF TO cl_ixml.
    DATA ref_to_data      TYPE REF TO data.
    DATA ref_to_object    TYPE REF TO object.
    DATA t                TYPE t.
    DATA new_variable     TYPE abap_bool.
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


CLASS lth_implements_intf_with_intf DEFINITION.
  PUBLIC SECTION.
    INTERFACES lih_with_component_intf.
ENDCLASS.


CLASS lth_superclass DEFINITION.
ENDCLASS.


CLASS lth_subclass DEFINITION INHERITING FROM lth_superclass.
ENDCLASS.


CLASS lth_without_intf DEFINITION.
  PUBLIC SECTION.
ENDCLASS.


CLASS ltc_applies_to_data IMPLEMENTATION.
  METHOD assert_compatible.
    DATA(rtti_target) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( target ) ).
    cl_abap_unit_assert=>assert_true( rtti_target->applies_to_data( source ) ).
  ENDMETHOD.

  METHOD assert_not_compatible.
    DATA(rtti_target) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( target ) ).
    cl_abap_unit_assert=>assert_false( rtti_target->applies_to_data( source ) ).
  ENDMETHOD.

  METHOD comp_dref_down_up_cast.
    DATA:
      BEGIN OF struct_ref_to_data,
        ref_to_data TYPE REF TO data,
      END OF struct_ref_to_data.
    DATA:
      BEGIN OF struct_ref_to_abap_bool,
        ref_to_abap_bool TYPE REF TO abap_bool,
      END OF struct_ref_to_abap_bool.

    assert_not_compatible( source = struct_ref_to_abap_bool
                           target = struct_ref_to_data ).

    assert_not_compatible( source = struct_ref_to_data
                           target = struct_ref_to_abap_bool ).
  ENDMETHOD.

  METHOD d_and_t.
    DATA d TYPE d.
    DATA t TYPE t.

    assert_not_compatible( source = d
                           target = t ).

    assert_not_compatible( source = t
                           target = d ).
  ENDMETHOD.

  METHOD dref_down_up_cast.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.
    DATA ref_to_data      TYPE REF TO data.

    assert_compatible( source = ref_to_abap_bool
                       target = ref_to_abap_bool ).

    assert_compatible( source = ref_to_data
                       target = ref_to_data ).

    assert_not_compatible( source = ref_to_abap_bool
                           target = ref_to_data ).

    assert_not_compatible( source = ref_to_data
                           target = ref_to_abap_bool ).
  ENDMETHOD.

  METHOD itab_key.
    DATA itab_default_key TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA itab_empty_key   TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    assert_not_compatible( source = itab_default_key
                           target = itab_empty_key ).
  ENDMETHOD.

  METHOD itab_key_category.
    DATA itab_hashed TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
    DATA itab_sorted TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

    assert_not_compatible( source = itab_hashed
                           target = itab_sorted ).
  ENDMETHOD.

  METHOD oref_down_up_cast.
    DATA ref_to_subclass   TYPE REF TO lth_subclass.
    DATA ref_to_superclass TYPE REF TO lth_superclass.
    DATA ref_to_object     TYPE REF TO object.

    assert_compatible( source = ref_to_subclass
                       target = ref_to_subclass ).

    assert_compatible( source = ref_to_superclass
                       target = ref_to_superclass ).

    assert_compatible( source = ref_to_object
                       target = ref_to_object ).

    assert_not_compatible( source = ref_to_subclass
                           target = ref_to_object ).

    assert_not_compatible( source = ref_to_subclass
                           target = ref_to_superclass ).

    assert_not_compatible( source = ref_to_superclass
                           target = ref_to_subclass ).
  ENDMETHOD.

  METHOD struct.
    DATA:
      BEGIN OF struct_1,
        comp TYPE c LENGTH 10,
      END OF struct_1.
    DATA:
      BEGIN OF struct_2,
        comp TYPE c LENGTH 10,
      END OF struct_2.

    assert_compatible( source = struct_1
                       target = struct_2 ).

    assert_compatible( source = struct_2
                       target = struct_1 ).
  ENDMETHOD.

  METHOD struct_component_names.
    DATA:
      BEGIN OF struct_comp1,
        comp1 TYPE c LENGTH 10,
      END OF struct_comp1.
    DATA:
      BEGIN OF struct_comp2,
        comp2 TYPE c LENGTH 10,
      END OF struct_comp2.

    assert_compatible( source = struct_comp1
                       target = struct_comp2 ).

    assert_compatible( source = struct_comp2
                       target = struct_comp1 ).
  ENDMETHOD.

  METHOD struct_extra_components.
    DATA:
      BEGIN OF struct_comp1,
        comp1 TYPE c LENGTH 10,
      END OF struct_comp1.
    DATA:
      BEGIN OF struct_comp1_comp2,
        comp1 TYPE c LENGTH 10,
        comp2 TYPE c LENGTH 10,
      END OF struct_comp1_comp2.

    assert_not_compatible( source = struct_comp1
                           target = struct_comp1_comp2 ).

    assert_not_compatible( source = struct_comp1_comp2
                           target = struct_comp1 ).
  ENDMETHOD.

  METHOD struct_length.
    DATA:
      BEGIN OF struct_10,
        comp TYPE c LENGTH 10,
      END OF struct_10.
    DATA:
      BEGIN OF struct_20,
        comp TYPE c LENGTH 20,
      END OF struct_20.

    assert_not_compatible( source = struct_10
                           target = struct_20 ).

    assert_not_compatible( source = struct_20
                           target = struct_10 ).
  ENDMETHOD.

  METHOD struct_type.
    DATA:
      BEGIN OF struct_c,
        comp TYPE c LENGTH 10,
      END OF struct_c.
    DATA:
      BEGIN OF struct_n,
        comp TYPE n LENGTH 10,
      END OF struct_n.

    assert_not_compatible( source = struct_c
                           target = struct_n ).

    assert_not_compatible( source = struct_n
                           target = struct_c ).
  ENDMETHOD.

  METHOD struct_flat_char.
    DATA:
      BEGIN OF struct_flat_char,
        comp1 TYPE c LENGTH 10,
        comp2 TYPE c LENGTH 20,
      END OF struct_flat_char.

    DATA(rtti_clike) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'CLIKE' ) ).
    cl_abap_unit_assert=>assert_true( rtti_clike->applies_to_data( struct_flat_char ) ).
  ENDMETHOD.

  METHOD struct_flat_char_and_c.
    DATA c TYPE c LENGTH 10.
    DATA:
      BEGIN OF struct_flat_char,
        comp1 TYPE c LENGTH 10,
        comp2 TYPE c LENGTH 20,
      END OF struct_flat_char.

    assert_not_compatible( source = struct_flat_char
                           target = c ).

    assert_not_compatible( source = c
                           target = struct_flat_char ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_convertible IMPLEMENTATION.
  METHOD comp_dref_upcast.
    DATA:
      BEGIN OF structure_1,
        ref_to_data TYPE REF TO data,
      END OF structure_1.
    DATA:
      BEGIN OF structure_2,
        ref_to_abap_bool TYPE REF TO abap_bool,
      END OF structure_2.

    zcl_convertible=>move( EXPORTING from = structure_2
                           IMPORTING to   = structure_1 ).

    structure_1 = structure_2.
  ENDMETHOD.

  METHOD dref_to_i_twice.
    data ref_to_i_1 type ref to i.
    data ref_to_i_2 type ref to i.
    zcl_convertible=>move( EXPORTING from = ref_to_i_2
                           IMPORTING to   = ref_to_i_1 ).
    ref_to_i_1 = ref_to_i_2.
  ENDMETHOD.

  METHOD dref_upcast.
    ref_to_data = ref_to_abap_bool.
    zcl_convertible=>move( EXPORTING from = ref_to_abap_bool
                           IMPORTING to   = ref_to_data ).
  ENDMETHOD.

  METHOD move.
    zcl_convertible=>move( EXPORTING from = from
                           IMPORTING to   = to ).
  ENDMETHOD.

  METHOD oref_upcast_clas_to_clas.
    ref_to_object = ref_to_cl_ixml.
    zcl_convertible=>move( EXPORTING from = ref_to_cl_ixml
                           IMPORTING to   = ref_to_object ).
  ENDMETHOD.

  METHOD oref_upcast_clas_to_intf.
    ref_to_intf_with_intf = ref_to_class_w_intf_w_intf.
    zcl_convertible=>move( EXPORTING from = ref_to_class_w_intf_w_intf
                           IMPORTING to   = ref_to_intf_with_intf ).
  ENDMETHOD.

  METHOD oref_upcast_intf_to_clas.
    ref_to_object = ref_to_intf.
    zcl_convertible=>move( EXPORTING from = ref_to_intf
                           IMPORTING to   = ref_to_object ).
  ENDMETHOD.

  METHOD oref_upcast_intf_to_intf.
    ref_to_intf = ref_to_intf_with_intf.
    zcl_convertible=>move( EXPORTING from = ref_to_intf_with_intf
                           IMPORTING to   = ref_to_intf ).
  ENDMETHOD.

  METHOD struct_dref_to_i_twice.
    DATA:
      BEGIN OF struct_ref_to_i_ref_to_i,
        ref_to_i_1 TYPE ref to i,
        ref_to_i_2 TYPE ref to i,
      END OF struct_ref_to_i_ref_to_i.
    DATA:
      BEGIN OF struct_ref_to_i_ref_to_data,
        ref_to_i TYPE ref to i,
        ref_to_data TYPE ref to data,
      END OF struct_ref_to_i_ref_to_data.
    zcl_convertible=>move( EXPORTING from = struct_ref_to_i_ref_to_i
                           IMPORTING to   = struct_ref_to_i_ref_to_data ).
    struct_ref_to_i_ref_to_data = struct_ref_to_i_ref_to_i.
  ENDMETHOD.

  METHOD t_to_string.
    string = t.
    zcl_convertible=>move( EXPORTING from = t
                           IMPORTING to   = string ).
  ENDMETHOD.

  METHOD two_structures.
    DATA:
      BEGIN OF source,
        company TYPE n LENGTH 5,
      END OF source.
    DATA:
      BEGIN OF target,
        "! Typo on purpose
        copany TYPE c LENGTH 10,
      END OF target.

    zcl_convertible=>move( EXPORTING from = source
                           IMPORTING to   = target ).
    target = source.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_convertible_short_dump IMPLEMENTATION.
  METHOD itab_duplicate_key.
    DATA itab_non_unique TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA itab_unique     TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

    itab_non_unique = VALUE #( ( 1 ) ( 1 ) ).

    " short dump ITAB_DUPLICATE_KEY
    IF 0 = 1.
      itab_unique = itab_non_unique.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_not_compatible IMPLEMENTATION.
  METHOD assert_not_compatible.
    DATA(rtti_source) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( source ) ).
    DATA(rtti_target) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( target ) ).
    IF     rtti_source->kind = cl_abap_typedescr=>kind_ref
       AND rtti_target->kind = cl_abap_typedescr=>kind_ref.
      IF source IS BOUND.
        IF rtti_source->type_kind = cl_abap_typedescr=>typekind_dref.
          cl_abap_unit_assert=>assert_false(
              act = CAST cl_abap_datadescr( CAST cl_abap_refdescr( rtti_target )->get_referenced_type( ) )->applies_to_data_ref(
                                                                                                             source )
              msg = 'asserts it is NOT compatible, but it is actually compatible' ).
        ELSE.
          cl_abap_unit_assert=>assert_false(
              act = CAST cl_abap_objectdescr( CAST cl_abap_refdescr( rtti_target )->get_referenced_type( ) )->applies_to(
                                                                                                               source )
              msg = 'asserts it is NOT compatible, but it is actually compatible' ).
        ENDIF.
      ENDIF.
    ELSE.
      cl_abap_unit_assert=>assert_false( act = rtti_target->applies_to_data( source )
                                         msg = 'asserts it is NOT compatible, but it is actually compatible' ).
    ENDIF.
  ENDMETHOD.

  METHOD downcast.
    DATA ref_to_data      TYPE REF TO data.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.

    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                     ` )
                                                                   ( `DATA ref_to_data      TYPE REF TO data.     ` )
                                                                   ( `DATA ref_to_abap_bool TYPE REF TO abap_bool.` )
                                                                   ( `ref_to_abap_bool = ref_to_data.             ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).

    cl_abap_unit_assert=>assert_initial(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                     ` )
                                                                   ( `DATA ref_to_data      TYPE REF TO data.     ` )
                                                                   ( `DATA ref_to_abap_bool TYPE REF TO abap_bool.` )
                                                                   ( `ref_to_abap_bool ?= ref_to_data.            ` ) ) ) ).

    assert_not_compatible( source = ref_to_data
                           target = ref_to_abap_bool ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_not_convertible IMPLEMENTATION.
  METHOD comp_dref_downcast.
    DATA:
      BEGIN OF structure_1,
        ref_to_data TYPE REF TO data,
      END OF structure_1.
    DATA:
      BEGIN OF structure_2,
        ref_to_abap_bool TYPE REF TO abap_bool,
      END OF structure_2.

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump OBJECTS_NOT_COMPATIBLE (not catchable).
      move( EXPORTING from = structure_1
            IMPORTING to   = structure_2 ).
    ENDIF.
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
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = structure_1
                               IMPORTING to   = structure_2 ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_not_compatible )
                                          msg = 'OBJECTS_NOT_COMPATIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD d_to_t.
    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump MOVE_NOT_SUPPORTED (not catchable).
      move( EXPORTING from = d
            IMPORTING to   = t ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.       ` )
                                                                   ( `DATA d TYPE d.` )
                                                                   ( `DATA t TYPE t.` )
                                                                   ( `t = d.        ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = d
                               IMPORTING to   = t ).
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

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump OBJECTS_TABLES_NOT_COMPATIBLE (not catchable).
      move( EXPORTING from = from_struct
            IMPORTING to   = to_struct ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                 ` )
                                                                   ( `DATA:                   ` )
                                                                   ( `  BEGIN OF from_struct, ` )
                                                                   ( `    comp1 TYPE string,  ` )
                                                                   ( `  END OF from_struct.   ` )
                                                                   ( `DATA:                   ` )
                                                                   ( `  BEGIN OF to_struct,   ` )
                                                                   ( `    comp2 TYPE string,  ` )
                                                                   ( `    comp3 TYPE string,  ` )
                                                                   ( `  END OF to_struct.     ` )
                                                                   ( `to_struct = from_struct.` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = from_struct
                               IMPORTING to   = to_struct ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_not_compatible )
                                          msg = 'OBJECTS_NOT_COMPATIBLE is expected' ).
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

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump OBJECTS_TABLES_NOT_COMPATIBLE (not catchable).
      move( EXPORTING from = from_table
            IMPORTING to   = to_table ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = lth_abap_code=>get_message_of_syntax_check(
                                                  VALUE #( ( `REPORT.                                          ` )
                                                           ( `TYPES:                                           ` )
                                                           ( `  BEGIN OF ty_from_table_line,                   ` )
                                                           ( `    comp1 TYPE string,                           ` )
                                                           ( `  END OF ty_from_table_line.                     ` )
                                                           ( `TYPES:                                           ` )
                                                           ( `  BEGIN OF ty_to_table_line,                     ` )
                                                           ( `    comp2 TYPE string,                           ` )
                                                           ( `    comp3 TYPE string,                           ` )
                                                           ( `  END OF ty_to_table_line.                       ` )
                                                           ( `DATA from_table TYPE TABLE OF ty_from_table_line.` )
                                                           ( `DATA to_table   TYPE TABLE OF ty_to_table_line.  ` )
                                                           ( `to_table = from_table.                           ` ) ) )
                                        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = from_table
                               IMPORTING to   = to_table ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        new_variable = cl_abap_unit_assert=>assert_true(
                           act = xsdbool( error->textid = zcx_convertible=>objects_tables_not_compatible )
                           msg = 'OBJECTS_TABLES_NOT_COMPATIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD dref_downcast.
    DATA ref_to_data      TYPE REF TO data.
    DATA ref_to_abap_bool TYPE REF TO abap_bool.

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump OBJECTS_NOT_COMPATIBLE (not catchable).
      move( EXPORTING from = ref_to_data
            IMPORTING to   = ref_to_abap_bool ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                     ` )
                                                                   ( `DATA ref_to_data      TYPE REF TO data.     ` )
                                                                   ( `DATA ref_to_abap_bool TYPE REF TO abap_bool.` )
                                                                   ( `ref_to_abap_bool = ref_to_data.             ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_data
                               IMPORTING to   = ref_to_abap_bool ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>objects_not_compatible )
                                          msg = 'OBJECTS_NOT_COMPATIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD move.
    to = from.
  ENDMETHOD.

  METHOD oref_downcast_clas_to_clas.
    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump MOVE_OREF_NOT_CONVERTIBLE (not catchable).
      move( EXPORTING from = ref_to_object
            IMPORTING to   = ref_to_cl_ixml ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.                                 ` )
                                                                   ( `DATA ref_to_object  TYPE REF TO object. ` )
                                                                   ( `DATA ref_to_cl_ixml TYPE REF TO cl_ixml.` )
                                                                   ( `ref_to_cl_ixml = ref_to_object.         ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_object
                               IMPORTING to   = ref_to_cl_ixml ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_oref_not_convertible )
                                          msg = 'MOVE_OREF_NOT_CONVERTIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD oref_downcast_clas_to_intf.
    DATA ref_to_class_wo_intf TYPE REF TO lth_without_intf.
    DATA ref_to_intf          TYPE REF TO lih_any.

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump MOVE_INTERFACE_NOT_SUPPORTED (not catchable).
      move( EXPORTING from = ref_to_class_wo_intf
            IMPORTING to   = ref_to_intf ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check(
                  VALUE #( ( `REPORT.                                                ` )
                           ( `INTERFACE lih_any.                                     ` )
                           ( `ENDINTERFACE.                                          ` )
                           ( `CLASS lth_without_intf DEFINITION.                     ` )
                           ( `  PUBLIC SECTION.                                      ` )
                           ( `ENDCLASS.                                              ` )
                           ( `DATA ref_to_intf TYPE REF TO lih_any.                  ` )
                           ( `DATA ref_to_class_wo_intf TYPE REF TO lth_without_intf.` )
                           ( `ref_to_intf = ref_to_class_wo_intf.                    ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_class_wo_intf
                               IMPORTING to   = ref_to_intf ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_interface_not_supported )
                                          msg = 'MOVE_INTERFACE_NOT_SUPPORTED is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD oref_downcast_intf_to_clas.
    DATA ref_to_intf               TYPE REF TO lih_any.
    DATA ref_to_class_without_intf TYPE REF TO lth_without_intf.

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump MOVE_IREF_TO_OREF (not catchable).
      move( EXPORTING from = ref_to_intf
            IMPORTING to   = ref_to_class_without_intf ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check(
                  VALUE #( ( `REPORT.                                                     ` )
                           ( `INTERFACE lih_any.                                          ` )
                           ( `ENDINTERFACE.                                               ` )
                           ( `CLASS lth_without_intf DEFINITION.                          ` )
                           ( `  PUBLIC SECTION.                                           ` )
                           ( `ENDCLASS.                                                   ` )
                           ( `DATA ref_to_intf               TYPE REF TO lih_any.         ` )
                           ( `DATA ref_to_class_without_intf TYPE REF TO lth_without_intf.` )
                           ( `ref_to_class_without_intf = ref_to_intf.                    ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_intf
                               IMPORTING to   = ref_to_class_without_intf ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_iref_to_oref )
                                          msg = 'MOVE_IREF_TO_OREF is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD oref_downcast_intf_to_intf.
    DATA ref_to_if_ixml_node    TYPE REF TO if_ixml_node.
    DATA ref_to_if_ixml_element TYPE REF TO if_ixml_element.

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump MOVE_IREF_NOT_CONVERTIBLE (not catchable).
      move( EXPORTING from = ref_to_if_ixml_node
            IMPORTING to   = ref_to_if_ixml_element ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check(
                  VALUE #( ( `REPORT.                                                 ` )
                           ( `DATA ref_to_if_ixml_node    TYPE REF TO if_ixml_node.   ` )
                           ( `DATA ref_to_if_ixml_element TYPE REF TO if_ixml_element.` )
                           ( `ref_to_if_ixml_element = ref_to_if_ixml_node.           ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = ref_to_if_ixml_node
                               IMPORTING to   = ref_to_if_ixml_element ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_iref_not_convertible )
                                          msg = 'MOVE_IREF_NOT_CONVERTIBLE is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD t_to_d.
    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump MOVE_NOT_SUPPORTED (not catchable).
      move( EXPORTING from = d
            IMPORTING to   = t ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
        act = lth_abap_code=>get_message_of_syntax_check( VALUE #( ( `REPORT.       ` )
                                                                   ( `DATA d TYPE d.` )
                                                                   ( `DATA t TYPE t.` )
                                                                   ( `t = d.        ` ) ) )
        exp = VALUE lth_abap_code=>ty_message_of_syntax_check( keyword   = 'MESSAGE'
                                                               msgnumber = 'GXD' ) ).
    TRY.
        zcl_convertible=>move( EXPORTING from = t
                               IMPORTING to   = d ).
        cl_abap_unit_assert=>fail( msg = 'Does not fail but exception is expected (zcx_convertible)' ).
      CATCH zcx_convertible INTO DATA(error).
        cl_abap_unit_assert=>assert_true( act = xsdbool( error->textid = zcx_convertible=>move_not_supported )
                                          msg = 'MOVE_NOT_SUPPORTED is expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test.
    DATA:
      BEGIN OF structure_1,
        ref_to_data TYPE REF TO data,
        d           TYPE d,
      END OF structure_1.
    DATA:
      BEGIN OF structure_2,
        ref_to_abap_bool TYPE REF TO abap_bool,
        t                TYPE t,
      END OF structure_2.

    IF 0 = 1.
      " The execution of the dynamic version of this code would lead to short dump OBJECTS_NOT_COMPATIBLE (not catchable).
      move( EXPORTING from = structure_1
            IMPORTING to   = structure_2 ).
    ENDIF.
  ENDMETHOD.
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


CLASS lth_abap_code IMPLEMENTATION.
  METHOD get_message_of_syntax_check.
    DATA(sc_result) = syntax_check( abap_source_code ).
    result = VALUE #( keyword   = sc_result-mid-keyword
                      msgnumber = sc_result-mid-msgnumber ).
  ENDMETHOD.

  METHOD syntax_check.
    DATA(synt) = VALUE ty_syntax_check( dir = VALUE #( name    = '$$DUMMY'
                                                       subc    = '1'
                                                       fixpt   = 'X'
                                                       uccheck = 'X' ) ).
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
