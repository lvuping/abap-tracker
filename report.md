ZTRACK_FIELDS (관리 테이블) → TABNAME, FIELDNAME, (옵션으로 키필드명들)

ZCHK_RESULT (로그 테이블) → TABNAME, FIELDNAME, KEY_VALUE, INVALID_VALUE, CHECK_DATE

한 테이블에 여러 필드를 관리 가능

Display(ALV)는 네가 따로 하니까, 나는 ZCHK_RESULT 에 저장하는 로직까지만 구현

아래는 저장만 하는 ABAP 코드 로직이야.

*---------------------------------------------------------------------*
* 프로그램 : ZCHK_USERDATA_FORMAT
* 목적     : 특정 테이블/필드의 데이터 포맷 검증 → 로그 테이블(ZCHK_RESULT)에 저장
*---------------------------------------------------------------------*

REPORT zchk_userdata_format.

TABLES: ztrack_fields.  " 추적대상 관리 테이블

TYPES: BEGIN OF ty_result,
         tabname       TYPE tabname,
         fieldname     TYPE fieldname,
         key_value     TYPE string,
         invalid_value TYPE string,
         check_date    TYPE datum,
       END OF ty_result.

DATA: lt_targets TYPE TABLE OF ztrack_fields,
      ls_target TYPE ztrack_fields,
      lt_result TYPE TABLE OF ty_result,
      ls_result TYPE ty_result.

DATA: lv_sql    TYPE string,
      lv_field  TYPE fieldname,
      lv_tab    TYPE tabname.

PARAMETERS: p_date TYPE datum OBLIGATORY DEFAULT '20250901'.

*---------------------------------------------------------------------*
* 1. 추적 대상 테이블/필드 읽기
*---------------------------------------------------------------------*
SELECT * INTO TABLE lt_targets FROM ztrack_fields.

LOOP AT lt_targets INTO ls_target.
  lv_tab   = ls_target-tabname.
  lv_field = ls_target-fieldname.

  " 동적 SQL 작성: Key + 대상 필드 함께 조회
  CLEAR lv_sql.
  CONCATENATE
    `SELECT * FROM` lv_tab
    `WHERE erdat >= @p_date`
    INTO lv_sql SEPARATED BY space.

  " Internal Table에 결과 담기
  DATA(lt_any) = VALUE TABLE OF string( ).
  EXEC SQL PERFORMING fetch_row.
    EXECUTE IMMEDIATE :lv_sql
  ENDEXEC.

  PERFORM check_values USING lv_tab lv_field.

ENDLOOP.

*---------------------------------------------------------------------*
* Callback: FETCH_ROW (EXEC SQL)
*---------------------------------------------------------------------*
FORM fetch_row.
  " 이 부분은 Open SQL로 다시 풀어쓰는 걸 권장
ENDFORM.

*---------------------------------------------------------------------*
* 검증 로직
*---------------------------------------------------------------------*
FORM check_values USING p_tabname p_fieldname.
  FIELD-SYMBOLS: <fs_line> TYPE any,
                 <fs_value> TYPE any.

  LOOP AT (p_tabname) ASSIGNING <fs_line>.
    ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_line> TO <fs_value>.
    IF sy-subrc = 0 AND <fs_value> IS NOT INITIAL.

      DATA(lv_value) = <fs_value>.
      " 숫자 8~10자리만 허용
      IF lv_value CP '[0-9]{8,10}'.
        CONTINUE.
      ELSE.
        CLEAR ls_result.
        ls_result-tabname       = p_tabname.
        ls_result-fieldname     = p_fieldname.
        " PK 필드가 여러 개일 수도 있으므로 우선 string으로만 저장
        ls_result-key_value     = |{ <fs_line> }|.
        ls_result-invalid_value = lv_value.
        ls_result-check_date    = sy-datum.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
* 5. 로그 테이블 저장
*---------------------------------------------------------------------*
IF lt_result IS NOT INITIAL.
  INSERT zchk_result FROM TABLE lt_result.
  COMMIT WORK.
ENDIF.


🔑 여기서 포인트:

Key 필드 관리

네가 ZTRACK_FIELDS 에 "어떤 필드가 키"인지도 넣어주면 ls_result-key_value 에 그 값을 저장할 수 있음.

지금 예제는 단순히 구조 전체를 string 으로 변환해서 넣어둠 (|{ <fs_line> }|).

동적 SELECT

EXEC SQL 보다는 Open SQL + Dynamic Assign 방식이 안정적이야.

예:

SELECT * FROM (lv_tab) INTO TABLE @DATA(lt_data)
  WHERE erdat >= @p_date.


이렇게 하면 <fs_value> 로 대상 필드만 뽑아낼 수 있음.

Display

너는 나중에 SELECT * FROM zchk_result 해서 ALV 뿌리면 됨.