# abap-convertible

The ABAP class `zcl_convertible` checks whether a data object is [convertible](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconvertible_glosry.htm) to another. In ABAP, the convertibility is implicitly checked when assigning a value to a variable, in case the target data object is not [compatible](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencompatible_glosry.htm) with the source data object.

## Why this class

The convertibility is checked by the ABAP compiler (syntax error if not convertible) only if the data types of the source and target data objects are known at compile time.
Otherwise, the convertibility is checked at runtime and may lead to uncatchable runtime errors like `MOVE_NOT_SUPPORTED`, `MOVE_OREF_NOT_CONVERTIBLE`, `OBJECTS_MOVE_NOT_SUPPORTED` or `OBJECTS_NOT_COMPATIBLE`.

Some tools implying dynamic data creation (possibly using RTTS runtime type creation) may need to check the convertibility at runtime to avoid short dumps.

## Development status

The tool `zcl_convertible` was quickly developed and contains probably many errors and flaws, so please post any issue and any pull request.

## Demonstration

Create a program with the code below to test the tool. Examples of results:
- From `DATS` to `TIMS` indicates `MOVE_NOT_SUPPORTED`
- From `REF_TO_DATA` to `V3440DREF3` (type ref to char 8) indicates `MOVE_NOT_SUPPORTED`
- From `BAPIRET1` to `BAPIRET2` indicates it is convertible
- TO DO: `OBJECTS_MOVE_NOT_SUPPORTED`
- TO DO: `OBJECTS_NOT_COMPATIBLE`

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

      zcl_convertible=>create_by_source_rtti( CAST #( cl_abap_typedescr=>describe_by_data_ref( dref_from ) ) )->check_convertible_to(
          CAST #( cl_abap_typedescr=>describe_by_data_ref( dref_to ) ) ).

      MESSAGE |{ ddicfrom } is convertible to { ddicto }| TYPE 'S'.
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
```

## Usage

```abap
TRY.
    zcl_convertible=>move( EXPORTING from = <from_table> IMPORTING to = <to_table> ).
  CATCH zcx_convertible INTO DATA(error_not_convertible).
    RAISE EXCEPTION error_not_convertible.
ENDTRY.

TRY.
    <to_table> = <from_table>.
  CATCH cx_sy_conversion_error.
    RAISE EXCEPTION conversion_error.
ENDTRY.
```
