*&---------------------------------------------------------------------*
*& Report ZMEGA_COMPLEX_SYUNAME_TEST
*&---------------------------------------------------------------------*
*& 메가 복잡한 SY-UNAME 추적 테스트 시나리오
*& 실제 SAP 환경에서 발생할 수 있는 모든 복잡한 패턴들을 포함
*&---------------------------------------------------------------------*

REPORT zmega_complex_syuname_test.

" 다양한 데이터 타입과 구조체 정의
TYPES: BEGIN OF ty_user_info,
         userid     TYPE sy-uname,
         full_name  TYPE string,
         department TYPE string,
         role       TYPE string,
         created_by TYPE sy-uname,
         created_at TYPE sy-datum,
         changed_by TYPE sy-uname,
         changed_at TYPE sy-datum,
       END OF ty_user_info.

TYPES: BEGIN OF ty_document,
         doc_id     TYPE string,
         doc_type   TYPE string,
         status     TYPE char1,
         created_by TYPE sy-uname,
         changed_by TYPE sy-uname,
         approved_by TYPE sy-uname,
       END OF ty_document.

TYPES: BEGIN OF ty_workflow,
         wf_id      TYPE string,
         initiator  TYPE sy-uname,
         current_user TYPE sy-uname,
         approver   TYPE sy-uname,
         status     TYPE char1,
       END OF ty_workflow.

" 전역 변수들
DATA: lv_current_user    TYPE sy-uname,
      lv_processing_user TYPE sy-uname,
      lv_audit_user      TYPE sy-uname,
      lv_temp_user       TYPE sy-uname,
      lv_backup_user     TYPE sy-uname,
      lv_workflow_user   TYPE sy-uname,
      lv_approval_user   TYPE sy-uname,
      lv_final_user      TYPE sy-uname,
      lv_test_variable   TYPE string.

DATA: ls_user_info   TYPE ty_user_info,
      ls_document    TYPE ty_document,
      ls_workflow    TYPE ty_workflow,
      lt_users       TYPE TABLE OF ty_user_info,
      lt_documents   TYPE TABLE OF ty_document,
      lt_temp_users  TYPE TABLE OF sy-uname.

DATA: lv_counter       TYPE i,
      lv_max_attempts  TYPE i VALUE 5,
      lv_success_flag  TYPE char1,
      lv_error_flag    TYPE char1.

*----------------------------------------------------------------------*
* 테스트 케이스 1: 매우 긴 변수 전파 체인 (10단계 이상)
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " 1단계: SY-UNAME 시작
  lv_current_user = sy-uname.
  
  " 2단계: 첫 번째 전파
  lv_processing_user = lv_current_user.
  
  " 3단계: 조건부 전파
  IF lv_processing_user IS NOT INITIAL.
    lv_audit_user = lv_processing_user.
  ENDIF.
  
  " 4단계: 구조체에 할당
  ls_user_info-userid = lv_audit_user.
  ls_user_info-created_by = lv_audit_user.
  
  " 5단계: 구조체에서 다시 추출
  lv_temp_user = ls_user_info-created_by.
  
  " 6단계: 백업 변수에 저장
  lv_backup_user = lv_temp_user.
  
  " 7단계: LOOP를 통한 처리
  APPEND lv_backup_user TO lt_temp_users.
  LOOP AT lt_temp_users INTO DATA(wa_temp_user).
    " 8단계: LOOP 내에서 재할당
    lv_workflow_user = wa_temp_user.
    " 9단계: 워크플로우 구조체에 할당
    ls_workflow-initiator = lv_workflow_user.
    " 10단계: 최종 승인자 설정
    lv_approval_user = ls_workflow-initiator.
    " 11단계: 최종 사용자 변수
    lv_final_user = lv_approval_user.
    
    " 12단계: 복잡한 조건문 내에서 PERFORM 호출
    IF lv_final_user IS NOT INITIAL AND lv_final_user <> 'SYSTEM'.
      CASE lv_final_user+0(1).
        WHEN 'A' OR 'B' OR 'C'.
          " 스코프 경계: PERFORM 호출
          PERFORM process_user_approval USING lv_final_user.
        WHEN OTHERS.
          lv_error_flag = 'X'.
      ENDCASE.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
* 테스트 케이스 2: 실패 케이스 - 주석 라인
*----------------------------------------------------------------------*
* 이 라인은 주석이므로 SY-UNAME이 없음

*----------------------------------------------------------------------*
* 테스트 케이스 3: 실패 케이스 - 빈 라인
*----------------------------------------------------------------------*

" 빈 라인 위에 있음

*----------------------------------------------------------------------*
* 테스트 케이스 4: 중첩된 조건문에서 INCLUDE
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
IF lv_current_user IS NOT INITIAL.
  lv_processing_user = lv_current_user.
  IF lv_processing_user+0(1) = 'A'.
    IF sy-datum >= '20240101'.
      " 여러 단계 중첩 후 INCLUDE
      INCLUDE zmega_user_validation.
    ENDIF.
  ENDIF.
ENDIF.

*----------------------------------------------------------------------*
* 테스트 케이스 5: LOOP 내에서 복잡한 구조체 처리 후 스코프 경계
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
ls_user_info-userid = lv_current_user.
ls_user_info-created_by = lv_current_user.
APPEND ls_user_info TO lt_users.

LOOP AT lt_users INTO DATA(wa_user_info).
  " 구조체 필드 재할당
  lv_processing_user = wa_user_info-created_by.
  ls_document-created_by = lv_processing_user.
  
  " 복잡한 비즈니스 로직
  IF wa_user_info-userid IS NOT INITIAL.
    lv_counter = lv_counter + 1.
    IF lv_counter <= lv_max_attempts.
      " 동적 서브루틴 호출
      CONCATENATE 'PROCESS_' wa_user_info-userid+0(3) INTO lv_test_variable.
      PERFORM (lv_test_variable) USING ls_document.
    ENDIF.
  ENDIF.
ENDLOOP.

*----------------------------------------------------------------------*
* 테스트 케이스 6: 실패 케이스 - sy-uname이 문자열 내에만 있음
*----------------------------------------------------------------------*
lv_test_variable = 'This contains sy-uname in a string but not as variable'.

*----------------------------------------------------------------------*
* 테스트 케이스 7: TRY-CATCH 블록에서 복잡한 처리
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

TRY.
    " TRY 블록 내에서 복잡한 처리
    ls_user_info-userid = lv_processing_user.
    lv_temp_user = ls_user_info-userid.
    
    " 여러 단계 후 예외 발생 가능한 작업
    IF lv_temp_user IS NOT INITIAL.
      lv_audit_user = lv_temp_user.
    ENDIF.
    
  CATCH cx_sy_dyn_call_error.
    lv_error_flag = 'X'.
    MESSAGE 'Dynamic call error' TYPE 'E'.
ENDTRY.

*----------------------------------------------------------------------*
* 테스트 케이스 8: SUBMIT을 통한 다른 프로그램 호출
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

" 복잡한 파라미터 준비
ls_workflow-initiator = lv_processing_user.
ls_workflow-current_user = lv_processing_user.

" 조건부 SUBMIT
IF ls_workflow-initiator IS NOT INITIAL.
  " 스코프 경계: SUBMIT 호출
  SUBMIT zmega_workflow_processor 
    WITH p_user = ls_workflow-initiator
    WITH p_wfid = ls_workflow-wf_id
    AND RETURN.
ENDIF.

*----------------------------------------------------------------------*
* 테스트 케이스 9: 객체 지향 패턴에서 메소드 호출
*----------------------------------------------------------------------*
DATA: lo_user_processor TYPE REF TO zcl_user_processor,
      lo_workflow_mgr   TYPE REF TO zcl_workflow_manager.

lv_current_user = sy-uname.
ls_user_info-userid = lv_current_user.
ls_user_info-created_by = lv_current_user.

" 객체 생성과 메소드 호출
CREATE OBJECT lo_user_processor.
CREATE OBJECT lo_workflow_mgr.

" 복잡한 메소드 체이닝
TRY.
    " 스코프 경계: 객체 메소드 호출
    lo_user_processor->validate_user_permissions( 
      iv_userid = ls_user_info-userid
      iv_created_by = ls_user_info-created_by ).
      
  CATCH cx_access_denied.
    lv_error_flag = 'X'.
ENDTRY.

*----------------------------------------------------------------------*
* 테스트 케이스 10: RFC 호출로 이어지는 복잡한 체인
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.
lv_audit_user = lv_processing_user.

" 복잡한 구조체 준비
ls_document-created_by = lv_audit_user.
ls_document-doc_id = |DOC_{ sy-datum }_{ sy-uzeit }|.
ls_document-status = 'P'.

" 여러 단계를 거친 후 RFC 호출
ls_workflow-initiator = ls_document-created_by.
lv_workflow_user = ls_workflow-initiator.

" RFC를 통한 원격 시스템 호출
CALL FUNCTION 'Z_RFC_MEGA_DOCUMENT_PROCESS'
  DESTINATION 'RFC_DEST'
  EXPORTING
    iv_document_id    = ls_document-doc_id
    iv_initiator      = lv_workflow_user
    iv_approval_level = '3'
  EXCEPTIONS
    system_failure    = 1
    communication_failure = 2
    OTHERS           = 3.

*----------------------------------------------------------------------*
* 테스트 케이스 11: 데이터베이스 INSERT로 이어지는 복잡한 체인
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
ls_user_info-userid = lv_current_user.
ls_user_info-created_by = lv_current_user.
ls_user_info-changed_by = lv_current_user.

" 복잡한 비즈니스 로직 후 데이터베이스 작업
IF ls_user_info-userid IS NOT INITIAL.
  ls_user_info-full_name = |User { ls_user_info-userid }|.
  ls_user_info-department = 'IT'.
  ls_user_info-role = 'ADMIN'.
  
  " 구조체 전체를 사용한 INSERT
  INSERT zusers FROM ls_user_info.
  
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDIF.

*----------------------------------------------------------------------*
* 테스트 케이스 12: FORM 정의 시작 (스코프 경계)
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

" FORM 정의 시작 - 여기서 스코프 경계
FORM process_mega_complex_workflow USING p_user TYPE sy-uname
                                          p_action TYPE string.
  " 새로운 스코프 시작
  DATA: lv_local_user TYPE sy-uname.
  lv_local_user = p_user.
  
  " 복잡한 로컬 처리
  UPDATE zdocuments SET changed_by = lv_local_user
    WHERE doc_id = 'TEST'
    AND status = 'A'.
    
ENDFORM.

*----------------------------------------------------------------------*
* 테스트 케이스 13: FUNCTION 정의 시작 (스코프 경계)
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
ls_document-created_by = lv_current_user.

" FUNCTION 정의 시작
FUNCTION z_mega_complex_function.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    IV_USER TYPE SY-UNAME
*"----------------------------------------------------------------------
  " 새로운 함수 스코프
  DATA: lv_func_user TYPE sy-uname.
  lv_func_user = iv_user.
  
ENDFUNCTION.

*----------------------------------------------------------------------*
* 테스트 케이스 14: 동적 CALL FUNCTION
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

" 동적 함수명 생성
CONCATENATE 'Z_DYNAMIC_FUNC_' lv_processing_user+0(2) INTO lv_test_variable.

" 동적 함수 호출
CALL FUNCTION lv_test_variable
  EXPORTING
    iv_user_id = lv_processing_user
  EXCEPTIONS
    function_not_found = 1
    OTHERS = 2.

*----------------------------------------------------------------------*
* 테스트 케이스 15: 동적 METHOD 호출
*----------------------------------------------------------------------*
DATA: lv_method_name TYPE string,
      lo_dynamic_obj TYPE REF TO object.

lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

" 동적 메소드명 생성
lv_method_name = 'PROCESS_USER_' && lv_processing_user+0(1).

" 동적 메소드 호출
CALL METHOD lo_dynamic_obj->(lv_method_name)
  EXPORTING
    iv_userid = lv_processing_user.

*----------------------------------------------------------------------*
* 테스트 케이스 16: CLASS 정의 시작 (스코프 경계)
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
ls_user_info-userid = lv_current_user.

" CLASS 정의 시작
CLASS zcl_mega_complex_processor DEFINITION.
  PUBLIC SECTION.
    METHODS: process_user IMPORTING iv_user TYPE sy-uname.
  PRIVATE SECTION.
    DATA: mv_current_user TYPE sy-uname.
ENDCLASS.

*----------------------------------------------------------------------*
* 테스트 케이스 17: UPDATE SET으로 이어지는 복잡한 체인
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.
lv_audit_user = lv_processing_user.

" 복잡한 구조체 처리
ls_document-created_by = lv_audit_user.
ls_document-changed_by = lv_audit_user.
ls_document-approved_by = lv_audit_user.

" 여러 필드를 한번에 UPDATE
UPDATE zdocuments SET 
  changed_by = ls_document-changed_by,
  approved_by = ls_document-approved_by,
  last_change = sy-datum
WHERE doc_id = 'MEGA001'
AND status = 'P'.

*----------------------------------------------------------------------*
* 테스트 케이스 18: 실패 케이스 - 타입 정의 라인
*----------------------------------------------------------------------*
DATA: lv_dummy_user TYPE sy-uname.
" 이 라인은 단순 변수 선언

*----------------------------------------------------------------------*
* 테스트 케이스 19: MODIFY TABLE로 이어지는 복잡한 체인
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
ls_user_info-userid = lv_current_user.
ls_user_info-created_by = lv_current_user.
ls_user_info-changed_by = lv_current_user.

" 내부 테이블에 추가
CLEAR lt_users.
APPEND ls_user_info TO lt_users.

" 복잡한 LOOP 처리
LOOP AT lt_users INTO DATA(wa_user_modify).
  wa_user_modify-changed_at = sy-datum.
  wa_user_modify-changed_by = lv_current_user.
  MODIFY lt_users FROM wa_user_modify.
ENDLOOP.

" 데이터베이스 일괄 수정
MODIFY zusers FROM TABLE lt_users.

*----------------------------------------------------------------------*
* 테스트 케이스 20: 중첩된 예외 처리에서 복잡한 흐름
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

TRY.
    " 첫 번째 TRY 블록
    ls_workflow-initiator = lv_processing_user.
    
    TRY.
        " 중첩된 TRY 블록
        lv_audit_user = ls_workflow-initiator.
        
        " 예외 발생 가능한 작업
        IF lv_audit_user IS INITIAL.
          RAISE EXCEPTION TYPE cx_sy_dyn_call_error.
        ENDIF.
        
      CATCH cx_sy_dyn_call_error.
        " 내부 예외 처리
        lv_error_flag = 'X'.
    ENDTRY.
    
  CATCH cx_root.
    " 외부 예외 처리
    MESSAGE 'Outer exception caught' TYPE 'I'.
ENDTRY.

*----------------------------------------------------------------------*
* 테스트 케이스 21: BDC 패턴을 통한 트랜잭션 호출
*----------------------------------------------------------------------*
DATA: lt_bdc_data TYPE TABLE OF bdcdata.

lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

" BDC 데이터 준비
CLEAR lt_bdc_data.
APPEND VALUE #( program = 'SAPMF05A' dynpro = '0100' dynbegin = 'X' ) TO lt_bdc_data.
APPEND VALUE #( fnam = 'RF05A-NEWKO' fval = lv_processing_user ) TO lt_bdc_data.

" BDC를 통한 트랜잭션 실행
CALL TRANSACTION 'FB01'
  USING lt_bdc_data
  MODE 'N'
  UPDATE 'S'.

*----------------------------------------------------------------------*
* 테스트 케이스 22: 워크플로우 시작과 복잡한 파라미터 전달
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
ls_workflow-initiator = lv_current_user.
ls_workflow-current_user = lv_current_user.

" 복잡한 워크플로우 파라미터 준비
DATA: lv_workflow_id TYPE string,
      lv_task_id     TYPE string.

lv_workflow_id = |WF_{ sy-datum }_{ ls_workflow-initiator }|.
lv_task_id = |TASK_{ ls_workflow-current_user }_001|.

" 워크플로우 RFC 호출
CALL FUNCTION 'Z_RFC_START_MEGA_WORKFLOW'
  EXPORTING
    iv_workflow_id    = lv_workflow_id
    iv_initiator      = ls_workflow-initiator
    iv_current_user   = ls_workflow-current_user
    iv_task_id        = lv_task_id
  EXCEPTIONS
    workflow_error    = 1
    authorization_error = 2
    OTHERS           = 3.

*----------------------------------------------------------------------*
* 테스트 케이스 23: 실패 케이스 - SELECT 문에서 INTO 절만
*----------------------------------------------------------------------*
SELECT SINGLE userid FROM zusers INTO lv_temp_user WHERE created_by = 'SYSTEM'.
" sy-uname이 없는 일반적인 SELECT 문

*----------------------------------------------------------------------*
* 테스트 케이스 24: 복잡한 CASE문에서 다중 스코프 경계
*----------------------------------------------------------------------*
lv_current_user = sy-uname.
lv_processing_user = lv_current_user.

CASE lv_processing_user+0(1).
  WHEN 'A'.
    " 관리자 권한
    ls_user_info-role = 'ADMIN'.
    ls_user_info-created_by = lv_processing_user.
    PERFORM admin_processing USING ls_user_info.
    
  WHEN 'U'.
    " 일반 사용자
    ls_user_info-role = 'USER'.
    ls_user_info-created_by = lv_processing_user.
    INCLUDE zmega_user_processing.
    
  WHEN 'S'.
    " 시스템 계정
    SUBMIT zmega_system_maintenance WITH p_user = lv_processing_user.
    
  WHEN OTHERS.
    " 기타 경우
    lv_error_flag = 'X'.
ENDCASE.

*----------------------------------------------------------------------*
* 테스트 케이스 25: 최종 복합 시나리오 - 모든 패턴 조합
*----------------------------------------------------------------------*
lv_current_user = sy-uname.

" 1단계: 초기 설정
lv_processing_user = lv_current_user.
ls_user_info-userid = lv_processing_user.

" 2단계: 조건문과 반복문
IF lv_processing_user IS NOT INITIAL.
  LOOP AT lt_temp_users INTO DATA(wa_final_user).
    " 3단계: 구조체 처리
    ls_document-created_by = wa_final_user.
    ls_workflow-initiator = wa_final_user.
    
    " 4단계: TRY-CATCH
    TRY.
        " 5단계: 동적 호출 준비
        CONCATENATE 'FINAL_PROCESS_' wa_final_user+0(2) INTO lv_test_variable.
        
        " 6단계: 복잡한 객체 지향 호출
        CREATE OBJECT lo_user_processor.
        lo_user_processor->execute_complex_workflow(
          iv_user = ls_workflow-initiator
          iv_document_id = ls_document-doc_id
        ).
        
      CATCH cx_sy_dyn_call_error.
        " 예외 상황에서도 처리 계속
        lv_error_flag = 'X'.
    ENDTRY.
  ENDLOOP.
ENDIF.

" 7단계: 최종 데이터베이스 작업 또는 스코프 경계
IF lv_error_flag IS INITIAL.
  " 성공 시 데이터베이스 작업
  INSERT zdocuments FROM ls_document.
ELSE.
  " 실패 시 에러 처리 서브루틴 호출
  PERFORM handle_mega_error USING lv_current_user lv_error_flag.
ENDIF.

END-OF-SELECTION.
