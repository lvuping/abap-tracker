* 복합 스코프 경계 및 엣지 케이스 테스트
DATA: lv_user TYPE sy-uname,
      lv_temp TYPE string,
      ls_document TYPE zdocuments,
      lv_flag TYPE char1.

*----------------------------------------------------------------------*
* 테스트 케이스 1: IF 조건문 내에서 PERFORM 호출
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lv_user = sy-uname.
  lv_temp = lv_user.
  
  IF lv_temp IS NOT INITIAL.
    PERFORM process_user_data USING lv_temp.
  ENDIF.

*----------------------------------------------------------------------*
* 테스트 케이스 2: LOOP 내에서 스코프 경계
*----------------------------------------------------------------------*
DATA: lt_users TYPE TABLE OF sy-uname,
      wa_user TYPE sy-uname.

lv_user = sy-uname.
APPEND lv_user TO lt_users.

LOOP AT lt_users INTO wa_user.
  IF wa_user = sy-uname.
    PERFORM check_authorization USING wa_user.
  ENDIF.
ENDLOOP.

*----------------------------------------------------------------------*
* 테스트 케이스 3: 구조체 필드 전파 후 PERFORM
*----------------------------------------------------------------------*
DATA: ls_header TYPE zhdr_document.

ls_header-created_by = sy-uname.
ls_header-doc_type = 'TEST'.
ls_header-status = 'A'.

PERFORM save_document USING ls_header.

*----------------------------------------------------------------------*
* 테스트 케이스 4: 중첩된 조건문에서 INCLUDE
*----------------------------------------------------------------------*
lv_user = sy-uname.

IF lv_user IS NOT INITIAL.
  IF lv_user <> 'SYSTEM'.
    INCLUDE zprog_user_validation.
  ENDIF.
ENDIF.

*----------------------------------------------------------------------*
* 테스트 케이스 5: CASE 문 내에서 SUBMIT
*----------------------------------------------------------------------*
lv_user = sy-uname.

CASE lv_user+0(1).
  WHEN 'A' OR 'B'.
    SUBMIT zprog_admin WITH p_user = lv_user.
  WHEN 'U'.
    lv_flag = 'X'.
ENDCASE.

*----------------------------------------------------------------------*
* 테스트 케이스 6: 동적 PERFORM 호출
*----------------------------------------------------------------------*
DATA: lv_subroutine TYPE string.

lv_user = sy-uname.
lv_subroutine = 'PROCESS_USER_' && lv_user+0(1).

PERFORM (lv_subroutine) USING lv_user.

*----------------------------------------------------------------------*
* 테스트 케이스 7: TRY-CATCH 내에서 스코프 경계
*----------------------------------------------------------------------*
lv_user = sy-uname.

TRY.
    lv_temp = lv_user.
    PERFORM risky_operation USING lv_temp.
  CATCH cx_sy_dyn_call_error.
    MESSAGE 'Error in dynamic call' TYPE 'E'.
ENDTRY.

*----------------------------------------------------------------------*
* 테스트 케이스 8: FUNCTION 호출 후 FORM 정의 (경계 충돌)
*----------------------------------------------------------------------*
lv_user = sy-uname.

CALL FUNCTION 'Z_VALIDATE_USER'
  EXPORTING
    iv_user = lv_user.

FORM process_user_data USING p_user TYPE any.
  " 새로운 스코프 - 여기서는 추적 종료되어야 함
  UPDATE zusers SET last_access = sy-datum WHERE userid = p_user.
ENDFORM.

*----------------------------------------------------------------------*
* 테스트 케이스 9: METHOD 호출 후 다른 METHOD 정의
*----------------------------------------------------------------------*
DATA: lo_processor TYPE REF TO zcl_user_processor.

lv_user = sy-uname.
CREATE OBJECT lo_processor.
lo_processor->process_user( lv_user ).

METHOD validate_user.
  " 새로운 메소드 스코프
  SELECT SINGLE * FROM zusers WHERE userid = iv_user INTO es_user.
ENDMETHOD.

*----------------------------------------------------------------------*
* 테스트 케이스 10: 복잡한 구조체 중첩 후 스코프 경계
*----------------------------------------------------------------------*
DATA: ls_complex TYPE zcomplex_structure.

ls_complex-header-created_by = sy-uname.
ls_complex-header-doc_id = '12345'.
ls_complex-items-item_id = '001'.
ls_complex-items-created_by = sy-uname.

PERFORM save_complex_data USING ls_complex.

END-OF-SELECTION.
