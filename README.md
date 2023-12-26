# abap-convertible

The ABAP class `zcl_convertible` checks whether a data object is [convertible](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconvertible_glosry.htm) to another. It can be used by tools which create data objects dynamically (`CREATE DATA`) and need to check the convertibility at runtime before assigning a value to a variable (to avoid short dumps in case the source data object is not convertible into the target data object).

## About the convertibility and uncatchable runtime errors

In ABAP, the convertibility is implicitly checked when assigning a value to a variable, in case the target data object is not [compatible](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencompatible_glosry.htm) with the source data object.

The convertibility is checked by the ABAP compiler (syntax error if not convertible) only if the data types of the source and target data objects are known at compile time.

Otherwise, the convertibility is checked at runtime and may lead to uncatchable runtime errors like `MOVE_NOT_SUPPORTED`, `MOVE_OREF_NOT_CONVERTIBLE`, `OBJECTS_MOVE_NOT_SUPPORTED` or `OBJECTS_NOT_COMPATIBLE`.

## Development status

`zcl_convertible` currently supports:
- Elementary variables and components
- Reference variables and components
- Deep structures
- Internal tables and table components with elementary and deep line types

It currently doesn't support:
- Flat structures and flat structured components
- Boxed components
- Enumerated types
- Meshes

Pull requests are welcome to fix any bug or add any missing feature.

## Demonstration

Create a program with the code below to test the tool. Examples of results:
- From `DATS` to `TIMS` results in `MOVE_NOT_SUPPORTED`
- From `REF_TO_DATA` to `V3440DREF3` (type ref to char 8) results in `OBJECTS_NOT_COMPATIBLE`
- From `REF_TO_DATA` to `SALV_S_AGGREGATION-R_AGGREGATION` results in `OBJECTS_MOVE_NOT_SUPPORTED`
- From `REF_TO_DATA` to `FLAG` results in `OBJECTS_MOVE_NOT_SUPPORTED`
- From `STECH_FLDS-OBJREF` to `SALV_S_AGGREGATION-R_AGGREGATION` results in `MOVE_OREF_NOT_CONVERTIBLE`
- etc.

Currently-unsupported checks (the result indicates it is convertible although it may be not)
- From `BAPIRET1` to `BAPIRET2`
- etc.

```abap
REPORT.
PARAMETERS ddicfrom TYPE string DEFAULT 'DATS'.
PARAMETERS ddicto TYPE string DEFAULT 'TIMS'.
START-OF-SELECTION.
  TRY.
      DATA dref_from TYPE REF TO data.
      DATA dref_to   TYPE REF TO data.
      CREATE DATA dref_from TYPE (ddicfrom).
      CREATE DATA dref_to TYPE (ddicto).

      zcl_convertible=>create_by_source_rtti(
          CAST #( cl_abap_typedescr=>describe_by_data_ref( dref_from ) )
          )->check_convertible_to(
          CAST #( cl_abap_typedescr=>describe_by_data_ref( dref_to ) ) ).

      MESSAGE |{ ddicfrom } is convertible to { ddicto }| TYPE 'S'.
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
```

## Usage

Example program where an unknown-typed source is to be transferred to an unknown-typed target:
```abap
...
TRY.
    zcl_convertible=>move( EXPORTING from = <from_anything> IMPORTING to = <to_anything> ).
  CATCH zcx_convertible INTO DATA(error_not_convertible).
    RAISE EXCEPTION error_not_convertible.
ENDTRY.

TRY.
    <to_anything> = <from_anything>.
  CATCH cx_sy_conversion_error.
    RAISE EXCEPTION conversion_error.
ENDTRY.
```
