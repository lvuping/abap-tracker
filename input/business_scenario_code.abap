*&---------------------------------------------------------------------*
*& Report Z_BUSINESS_SCENARIO_SY_UNAME_TRACKING
*&---------------------------------------------------------------------*
*& 실제 비즈니스 시나리오에서 SY-UNAME 사용 추적
*& - 데이터베이스 작업 (UPDATE, INSERT, MODIFY, DELETE)
*& - RFC 파라미터 사용
*& - 감사 필드 (ERDAT, AENAM 등) 사용
*&---------------------------------------------------------------------*
REPORT z_business_scenario_sy_uname_tracking.

*----------------------------------------------------------------------*
* TYPES & DATA Declarations
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_document,
         doc_id     TYPE n LENGTH 10,
         doc_type   TYPE c LENGTH 4,
         created_by TYPE sy-uname,
         created_at TYPE sy-datum,
         changed_by TYPE sy-uname,
         changed_at TYPE sy-datum,
         status     TYPE c LENGTH 1,
       END OF ty_document.

TYPES: BEGIN OF ty_audit_log,
         log_id     TYPE n LENGTH 12,
         object_id  TYPE c LENGTH 20,
         action     TYPE c LENGTH 10,
         user_name  TYPE sy-uname,
         timestamp  TYPE timestampl,
         details    TYPE string,
       END OF ty_audit_log.

DATA: lv_current_user TYPE sy-uname,
      lv_processing_user TYPE sy-uname,
      lv_audit_user TYPE sy-uname,
      ls_document TYPE ty_document,
      ls_audit TYPE ty_audit_log,
      lt_documents TYPE TABLE OF ty_document.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
PARAMETERS: p_docid TYPE n LENGTH 10 OBLIGATORY.

*----------------------------------------------------------------------*
* MAIN PROCESSING
*----------------------------------------------------------------------*
START-OF-SELECTION.

  "--------------------------------------------------------------------"
  " 시나리오 1: SY-UNAME을 사용자 변수로 전파
  "--------------------------------------------------------------------"
  lv_current_user = sy-uname.
  
  " 다단계 변수 전파
  lv_processing_user = lv_current_user.
  lv_audit_user = lv_processing_user.

  "--------------------------------------------------------------------"
  " 시나리오 2: 문서 생성 - 감사 필드에 사용자 정보 저장
  "--------------------------------------------------------------------"
  
  " 문서 구조체 준비
  ls_document-doc_id = p_docid.
  ls_document-doc_type = 'QUOT'.
  ls_document-created_by = lv_current_user.     " ← SY-UNAME 추적 중요!
  ls_document-created_at = sy-datum.
  ls_document-changed_by = lv_processing_user.   " ← SY-UNAME 추적 중요!
  ls_document-changed_at = sy-datum.
  ls_document-status = 'A'.

  " 감사 로그 구조체 준비  
  ls_audit-log_id = |{ sy-datum }{ sy-uzeit }|.
  ls_audit-object_id = |DOC_{ p_docid }|.
  ls_audit-action = 'CREATE'.
  ls_audit-user_name = lv_audit_user.           " ← SY-UNAME 추적 중요!
  GET TIME STAMP FIELD ls_audit-timestamp.
  ls_audit-details = |Document created by { lv_current_user }|.

  "--------------------------------------------------------------------"
  " 시나리오 3: 데이터베이스 INSERT 작업
  "--------------------------------------------------------------------"
  
  " 테이블에 문서 레코드 삽입
  INSERT zdocuments FROM ls_document.
  
  IF sy-subrc = 0.
    " 감사 로그 테이블에 로그 삽입
    INSERT zaudit_log FROM ls_audit.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  "--------------------------------------------------------------------"
  " 시나리오 4: 기존 문서 업데이트 - UPDATE 작업
  "--------------------------------------------------------------------"
  
  " 문서 상태 변경
  UPDATE zdocuments 
    SET status = 'P',
        changed_by = lv_processing_user,        " ← SY-UNAME 추적 중요!
        changed_at = sy-datum
    WHERE doc_id = p_docid.

  IF sy-subrc = 0.
    " 변경 감사 로그 생성
    ls_audit-action = 'UPDATE'.
    ls_audit-details = |Document { p_docid } updated by { lv_processing_user }|.
    GET TIME STAMP FIELD ls_audit-timestamp.
    
    INSERT zaudit_log FROM ls_audit.
  ENDIF.

  "--------------------------------------------------------------------"
  " 시나리오 5: 대량 데이터 처리 - MODIFY 작업
  "--------------------------------------------------------------------"
  
  " 여러 문서 상태 일괄 변경
  SELECT * FROM zdocuments INTO TABLE lt_documents
    WHERE created_by = lv_current_user.         " ← SY-UNAME WHERE 조건 추적!

  LOOP AT lt_documents INTO ls_document.
    ls_document-changed_by = lv_processing_user. " ← SY-UNAME 추적 중요!
    ls_document-changed_at = sy-datum.
    ls_document-status = 'C'.
  ENDLOOP.

  " 대량 업데이트
  MODIFY zdocuments FROM TABLE lt_documents.

  "--------------------------------------------------------------------"
  " 시나리오 6: RFC 호출에서 사용자 정보 전달
  "--------------------------------------------------------------------"
  
  " 외부 시스템에 문서 생성 알림
  CALL FUNCTION 'Z_RFC_NOTIFY_DOCUMENT_CREATION'
    DESTINATION 'EXTERNAL_SYSTEM'
    EXPORTING
      i_document_id    = p_docid
      i_creator        = lv_current_user         " ← SY-UNAME RFC 파라미터 추적!
      i_processor      = lv_processing_user      " ← SY-UNAME RFC 파라미터 추적!
      i_creation_date  = sy-datum
    IMPORTING
      e_external_id    = DATA(lv_external_id)
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.

  IF sy-subrc = 0.
    " 외부 시스템 연동 성공 로그
    ls_audit-action = 'RFC_CALL'.
    ls_audit-details = |External notification sent by { lv_audit_user }|.
    INSERT zaudit_log FROM ls_audit.
  ENDIF.

  "--------------------------------------------------------------------"
  " 시나리오 7: 권한 체크 및 삭제 작업
  "--------------------------------------------------------------------"
  
  " 권한 확인 RFC 호출
  CALL FUNCTION 'Z_RFC_CHECK_DELETE_PERMISSION'
    EXPORTING
      i_user_id     = lv_current_user            " ← SY-UNAME RFC 파라미터 추적!
      i_object_type = 'DOCUMENT'
      i_object_id   = p_docid
    IMPORTING  
      e_authorized  = DATA(lv_authorized).

  IF lv_authorized = 'X'.
    " 삭제 권한이 있는 경우
    DELETE FROM zdocuments 
      WHERE doc_id = p_docid 
        AND created_by = lv_current_user.        " ← SY-UNAME WHERE 조건 추적!
    
    IF sy-subrc = 0.
      " 삭제 감사 로그
      ls_audit-action = 'DELETE'.
      ls_audit-details = |Document { p_docid } deleted by { lv_audit_user }|.
      INSERT zaudit_log FROM ls_audit.
    ENDIF.
  ENDIF.

  "--------------------------------------------------------------------"
  " 시나리오 8: 트랜잭션 호출
  "--------------------------------------------------------------------"
  
  " BDC 데이터 준비
  DATA: BEGIN OF ls_bdc_data,
          fnam TYPE c LENGTH 132,
          fval TYPE c LENGTH 132,
        END OF ls_bdc_data.

  ls_bdc_data-fnam = 'CREATED_BY'.
  ls_bdc_data-fval = lv_current_user.           " ← SY-UNAME BDC 데이터 추적!

  " 표준 트랜잭션 호출
  CALL TRANSACTION 'Z_DOC_MAINTAIN' 
    USING lt_bdc_data 
    MODE 'N'
    UPDATE 'S'.

  "--------------------------------------------------------------------"
  " 시나리오 9: 워크플로우 시작
  "--------------------------------------------------------------------"
  
  " 워크플로우 시작 RFC
  CALL FUNCTION 'Z_RFC_START_APPROVAL_WORKFLOW'
    EXPORTING
      i_document_id     = p_docid
      i_initiator       = lv_current_user        " ← SY-UNAME RFC 파라미터 추적!
      i_approver_level  = '1'
    EXCEPTIONS
      workflow_error    = 1
      OTHERS           = 2.

*&---------------------------------------------------------------------*
*&      Form  ADDITIONAL_AUDIT_TRACKING
*&---------------------------------------------------------------------*
FORM additional_audit_tracking USING iv_user TYPE sy-uname
                                     iv_action TYPE c.

  DATA: ls_custom_audit TYPE ty_audit_log.

  " 추가 감사 추적
  ls_custom_audit-user_name = iv_user.          " ← SY-UNAME 서브루틴 파라미터 추적!
  ls_custom_audit-action = iv_action.
  ls_custom_audit-details = |Additional audit by { iv_user }|.
  
  " 커스텀 감사 테이블에 저장
  INSERT zcustom_audit FROM ls_custom_audit.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_USER_STATISTICS  
*&---------------------------------------------------------------------*
FORM update_user_statistics USING iv_user TYPE sy-uname.

  " 사용자 통계 업데이트
  UPDATE zuser_stats 
    SET last_activity = sy-datum,
        activity_count = activity_count + 1,
        last_action_user = iv_user           " ← SY-UNAME 추적 중요!
    WHERE user_id = iv_user.                " ← SY-UNAME WHERE 조건 추적!

ENDFORM.
