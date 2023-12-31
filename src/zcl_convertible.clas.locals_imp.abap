*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_convertible_complex DEFINITION DEFERRED.
CLASS lcl_convertible_elem DEFINITION DEFERRED.
CLASS lcl_convertible_ref DEFINITION DEFERRED.
CLASS lcl_convertible_struct DEFINITION DEFERRED.
CLASS lcl_convertible_table DEFINITION DEFERRED.

CLASS zcl_convertible DEFINITION
LOCAL FRIENDS
lcl_convertible_complex
lcl_convertible_elem
lcl_convertible_ref
lcl_convertible_struct
lcl_convertible_table.


"! Case source type is an elementary type
CLASS lcl_convertible_elem DEFINITION
  INHERITING FROM zcl_convertible
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS check_convertible_to
        REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


"! Case source type is a reference type
CLASS lcl_convertible_ref DEFINITION
  INHERITING FROM zcl_convertible
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS check_convertible_to
        REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_convertible_complex DEFINITION
  INHERITING FROM zcl_convertible
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


"! Case source type is a structured type
CLASS lcl_convertible_struct DEFINITION
  INHERITING FROM lcl_convertible_complex
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS check_convertible_to
        REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


"! Case source type is a table type
CLASS lcl_convertible_table DEFINITION
  INHERITING FROM lcl_convertible_complex
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS check_convertible_to
        REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_convertible_complex IMPLEMENTATION.
ENDCLASS.


CLASS lcl_convertible_elem IMPLEMENTATION.
  METHOD check_convertible_to.
    IF    ( source_rtti->type_kind = 'D' AND target_rtti->type_kind = 'T' )
       OR ( source_rtti->type_kind = 'T' AND target_rtti->type_kind = 'D' ).
      RAISE EXCEPTION TYPE zcx_convertible
        EXPORTING textid = zcx_convertible=>move_not_supported.
    ENDIF.
    IF target_rtti->kind <> target_rtti->kind_elem.
      RAISE EXCEPTION TYPE zcx_convertible
        EXPORTING textid = zcx_convertible=>objects_move_not_supported.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_convertible_ref IMPLEMENTATION.
  METHOD check_convertible_to.
    " ASSIGNMENT RULES FOR REFERENCE VARIABLES
    "
    " ABAP - Keyword Documentation →  ABAP - Programming Language →  Processing Internal Data →  Assignments →  Assigning References →  Assigning Reference Variables
    "
    " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_references.htm
    "
    "   - The content of a reference variable can only be assigned to another reference variable and the following rules apply:
    "
    "       - data references can only be assigned to data reference variables
    "       - object references can only be assigned to object reference variables
    "
    "   - [...]
    "
    "   - Assignments between reference variables and data objects that are not reference variables lead to a syntax error or the runtime error OBJECTS_MOVE_NOT_SUPPORTED.

    TYPES ty_interfaces_rtti TYPE STANDARD TABLE OF REF TO cl_abap_intfdescr WITH EMPTY KEY.

    DATA from_class_rtti TYPE REF TO cl_abap_classdescr.

    IF source_rtti->type_kind <> target_rtti->type_kind.
      " Data reference <> Object reference
      RAISE EXCEPTION TYPE zcx_convertible
        EXPORTING textid = zcx_convertible=>objects_not_compatible.
    ELSEIF target_rtti->kind <> target_rtti->kind_ref.
      RAISE EXCEPTION TYPE zcx_convertible
        EXPORTING textid = zcx_convertible=>objects_move_not_supported.
    ELSE.

      " If it's compatible, then it's convertible.
      IF source_rtti->absolute_name = target_rtti->absolute_name.
        RETURN.
      ENDIF.

      " ASSIGNMENTS BETWEEN DATA REFERENCE VARIABLES
      "
      " ABAP - Keyword Documentation →  ABAP - Programming Language →  Processing Internal Data →  Assignments →
      "   Assigning References →  Assigning Reference Variables →  Assignment Rules for Reference Variables
      "
      " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_references_data.htm
      "
      "   - An upcast in data references is possible in the following cases:
      "
      "       - The static types of the source variable and the target variable match according to the following rules:
      "
      "       - Both are elementary data types with identical technical type properties, namely the built-in ABAP type,
      "         length, and number of decimal places. It is not important how the static types were defined.
      "
      "       - Both have an identical structured type. In the case of structured types, identical technical type
      "         properties are not sufficient, but the same structured type must have been used to define the static types.
      "
      "       - Both are table types with matching technical type properties, that is, line types, table category, and
      "         table key. In the case of non-structured line types, identical technical properties of the line type
      "         are sufficient. In the case of structured line types, both definitions must have been made with
      "         reference to the same structured type.
      "
      "   - The static type of the source variable is completely typed, and the static type of the target variable is generic.

      DATA(rtti_referenced_by_to_rtti) = CAST cl_abap_refdescr( target_rtti )->get_referenced_type( ).
      DATA(rtti_referenced_by_from_rtti) = CAST cl_abap_refdescr( source_rtti )->get_referenced_type( ).

      IF rtti_referenced_by_from_rtti = rtti_referenced_by_to_rtti.
        RETURN.
      ENDIF.

      IF target_rtti->type_kind = target_rtti->typekind_dref.
        " REF TO DATA
        IF rtti_referenced_by_to_rtti->absolute_name = '\TYPE=DATA'.
          RETURN.
        ENDIF.

        RAISE EXCEPTION TYPE zcx_convertible
          EXPORTING textid = zcx_convertible=>objects_not_compatible.

      ELSEIF target_rtti->type_kind = target_rtti->typekind_oref.
        " ASSIGNMENTS BETWEEN OBJECT REFERENCE VARIABLES
        "
        " ABAP - Keyword Documentation →  ABAP - Programming Language →  Processing Internal Data →  Assignments →
        "   Assigning References →  Assigning Reference Variables →  Assignment Rules for Reference Variables
        "
        " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_references.htm
        "
        "   - An upcast in object references is possible in the following cases:
        "
        "       1) If both static types are classes, the class of the target variable must be the same class or a
        "          superclass of the source variable.
        "
        "       2) If both static types are interfaces, the interface of the target variable must be the same interface
        "          or a component interface of the source variable.
        "
        "       3) If the static type of the target variable is an interface and the static type of the source variable
        "          is a class, the class of the source variable must implement the interface of the target variable.
        "
        "       4) If the static type of the target variable is a class and the static type of the source variable is
        "          an interface, the class of the target variable must be the generic type or the root class object.
        "
        " Otherwise syntax error or short dump MOVE_INTERFACE_NOT_SUPPORTED
        DATA(from_class_to_class) = |{ cl_abap_typedescr=>kind_class }{ cl_abap_typedescr=>kind_class }|.
        DATA(from_intf_to_intf) = |{ cl_abap_typedescr=>kind_intf }{ cl_abap_typedescr=>kind_intf }|.
        DATA(from_class_to_intf) = |{ cl_abap_typedescr=>kind_class }{ cl_abap_typedescr=>kind_intf }|.
        DATA(from_intf_to_class) = |{ cl_abap_typedescr=>kind_intf }{ cl_abap_typedescr=>kind_class }|.

        CASE |{ rtti_referenced_by_from_rtti->kind }{ rtti_referenced_by_to_rtti->kind }|.
          WHEN from_class_to_class.
            " Performance: no need to check the super-class in case the target is REF TO OBJECT,
            " because OBJECT is the super-class of all classes.
            IF rtti_referenced_by_to_rtti->absolute_name = '\CLASS=OBJECT'.
              RETURN.
            ENDIF.
            " Text from above rule:
            "   1) If both static types are classes, the class of the target variable must be the same class or a
            "      superclass of the source variable.
            from_class_rtti = CAST cl_abap_classdescr( rtti_referenced_by_from_rtti ).
            DO.
              IF from_class_rtti = rtti_referenced_by_to_rtti.
                RETURN.
              ENDIF.
              from_class_rtti->get_super_class_type( RECEIVING  p_descr_ref           = DATA(from_super_class_rtti)
                                                     EXCEPTIONS super_class_not_found = 1
                                                                OTHERS                = 2 ).
              IF sy-subrc <> 0.
                " No super class
                EXIT.
              ENDIF.
              from_class_rtti = from_super_class_rtti.
            ENDDO.

            RAISE EXCEPTION TYPE zcx_convertible
              EXPORTING textid = zcx_convertible=>move_oref_not_convertible.

          WHEN from_intf_to_intf.
            " Text from above rule:
            "   2) If both static types are interfaces, the interface of the target variable must be the same interface
            "      or a component interface of the source variable.
            DATA(from_interface_rtti) = CAST cl_abap_intfdescr( rtti_referenced_by_from_rtti ).
            DATA(from_interfaces_rtti) = VALUE ty_interfaces_rtti( ( from_interface_rtti ) ).
            LOOP AT from_interfaces_rtti INTO from_interface_rtti.
              IF from_interface_rtti = rtti_referenced_by_to_rtti.
                RETURN.
              ENDIF.
              LOOP AT from_interface_rtti->interfaces REFERENCE INTO DATA(from_component_interface).
                from_interface_rtti->get_interface_type(
                  EXPORTING  p_name              = from_component_interface->name
                  RECEIVING  p_descr_ref         = DATA(from_component_interface_rtti)
                  EXCEPTIONS interface_not_found = 1
                             OTHERS              = 2 ).
                IF sy-subrc <> 0.
                  RAISE EXCEPTION TYPE zcx_convertible_no_check.
                ENDIF.
                INSERT from_component_interface_rtti INTO TABLE from_interfaces_rtti.
              ENDLOOP.
            ENDLOOP.

            RAISE EXCEPTION TYPE zcx_convertible
              EXPORTING textid = zcx_convertible=>move_iref_not_convertible.

          WHEN from_class_to_intf.
            " Text from above rule:
            "   3) If the static type of the target variable is an interface and the static type of the source variable
            "      is a class, the class of the source variable must implement the interface of the target variable.
            from_class_rtti = CAST cl_abap_classdescr( rtti_referenced_by_from_rtti ).
            DATA(to_interface_rtti) = CAST cl_abap_intfdescr( rtti_referenced_by_to_rtti ).
            IF line_exists( from_class_rtti->interfaces[ name = to_interface_rtti->get_relative_name( ) ] ).
              RETURN.
            ENDIF.

            RAISE EXCEPTION TYPE zcx_convertible
              EXPORTING textid = zcx_convertible=>move_interface_not_supported.

          WHEN from_intf_to_class.
            " Text from above rule:
            "   4) If the static type of the target variable is a class and the static type of the source variable is
            "      an interface, the class of the target variable must be the generic type or the root class object.
            IF rtti_referenced_by_to_rtti->absolute_name = '\CLASS=OBJECT'.
              RETURN.
            ENDIF.

            RAISE EXCEPTION TYPE zcx_convertible
              EXPORTING textid = zcx_convertible=>move_iref_to_oref.

        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_convertible_struct IMPLEMENTATION.
  METHOD check_convertible_to.
    DATA target_struct_rtti TYPE REF TO cl_abap_structdescr.

    IF target_rtti->kind <> target_rtti->kind_struct.
      RAISE EXCEPTION TYPE zcx_convertible
        EXPORTING textid = zcx_convertible=>objects_move_not_supported.
    ENDIF.

    CASE source_rtti->type_kind.
      WHEN source_rtti->typekind_struct2.
        "===================
        " deep structure
        "===================
        "
        " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_struc.htm
        "
        "   - Deep structures can only be assigned to each other if they are compatible. Full compatibility is not
        "     required for the following deep components, and the following applies instead:
        "
        "       - For components that have a reference type, upcasts but not downcasts are allowed.
        "
        "       - For table-like components, it is sufficient if the line type is compatible. This means that full
        "         compatibility is not required, which also includes table categories and table keys.

        IF target_rtti->type_kind <> source_rtti->typekind_struct2.
          RAISE EXCEPTION TYPE zcx_convertible
            EXPORTING textid = zcx_convertible=>objects_not_compatible.
        ENDIF.

        target_struct_rtti = CAST cl_abap_structdescr( target_rtti ).

        IF lines( source_struct_rtti->components ) <> lines( target_struct_rtti->components ).
          RAISE EXCEPTION TYPE zcx_convertible
            EXPORTING textid = zcx_convertible=>objects_not_compatible.
        ENDIF.

        DATA(target_components) = target_struct_rtti->get_components( ).

        LOOP AT source_struct_rtti->get_components( ) REFERENCE INTO DATA(source_component).
          DATA(target_component) = REF #( target_components[ sy-tabix ] ).

          " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_struc.htm
          "
          "   - Deep structures can only be assigned to each other if they are compatible. Full compatibility is not
          "     required for the following deep components, and the following applies instead:
          "
          "       - For components that have a reference type, upcasts but not downcasts are allowed.
          "
          "       - For table-like components, it is sufficient if the line type is compatible. This means that full
          "         compatibility is not required, which also includes table categories and table keys.

          DATA(source_dobj_checker) = create_by_source_rtti( source_component->type ).
          source_dobj_checker->check_convertible_to( target_component->type ).

        ENDLOOP.

      WHEN source_rtti->typekind_struct1.
        "===================
        " flat structure
        "===================
        " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_struc.htm
        "   - For flat structures, there are conversion rules for the following assignments between incompatible data objects:
        "       - Conversion between flat structures
        "       - Conversion between flat structures and single fields
        IF target_rtti->type_kind = source_rtti->typekind_struct1.
        ELSEIF target_rtti->kind = source_rtti->kind_elem.
        ELSE.
          RAISE EXCEPTION TYPE zcx_convertible
            EXPORTING textid = zcx_convertible=>objects_move_not_supported.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_convertible_table IMPLEMENTATION.
  METHOD check_convertible_to.
    " CONVERSION RULES FOR INTERNAL TABLES
    "
    " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_itab.htm
    "
    " ABAP - Keyword Documentation →  ABAP - Programming Language →  Processing Internal Data →  Assignments →  Assignment and Conversion Rules
    "
    "   - Internal tables can only be assigned to internal tables. Whether or not assignment is possible depends
    "     exclusively on the line type, and it is independent of table type, table key, and number of lines.
    "     Internal tables can be assigned to each other if their line types are compatible or convertible.
    "
    "   ...
    "
    "   - Assignments between internal tables and data objects that are not internal tables lead to a syntax error
    "     or the runtime error OBJECTS_MOVE_NOT_SUPPORTED.

    IF target_rtti->kind <> target_rtti->kind_table.
      RAISE EXCEPTION TYPE zcx_convertible
        EXPORTING textid = zcx_convertible=>objects_move_not_supported.
    ENDIF.

    DATA(target_table_rtti) = CAST cl_abap_tabledescr( target_rtti ).
    DATA(target_table_line_rtti) = target_table_rtti->get_table_line_type( ).
    DATA(source_table_line_rtti) = source_table_rtti->get_table_line_type( ).
    IF NOT target_table_line_rtti->applies_to_data( source_table_line_rtti ).
      TRY.
          zcl_convertible=>create_by_source_rtti( source_table_line_rtti )->check_convertible_to( target_table_line_rtti ).
        CATCH zcx_convertible INTO DATA(error).
          RAISE EXCEPTION TYPE zcx_convertible
            EXPORTING textid   = error->objects_tables_not_compatible
                      previous = error.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
