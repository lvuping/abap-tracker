*&---------------------------------------------------------------------*
*& Test Code for UPDATE Statement with SY-UNAME Detection
*&---------------------------------------------------------------------*
*& 이 테스트는 UPDATE 구문에서 SY-UNAME을 감지하는 두 가지 패턴을 테스트합니다:
*& 1. UPDATE 구문에서 sy-uname 직접 사용
*& 2. 구조체에 sy-uname 할당 후 UPDATE에서 구조체 필드 사용
*&---------------------------------------------------------------------*

REPORT z_test_update_syuname.

DATA: ls_ztable TYPE z_my_custom_table.

" === 테스트 케이스 1: UPDATE 구문에서 sy-uname 직접 사용 ===
PERFORM test_direct_syuname_in_update.

" === 테스트 케이스 2: 구조체 필드를 통한 sy-uname 사용 ===
PERFORM test_structure_field_syuname.

" === 테스트 케이스 3: 혼합 사용 케이스 ===
PERFORM test_mixed_update_patterns.

*&---------------------------------------------------------------------*
*& Form test_direct_syuname_in_update
*&---------------------------------------------------------------------*
FORM test_direct_syuname_in_update.
  " 직접 sy-uname을 UPDATE 구문에서 사용하는 케이스
  
  " 단일 필드 UPDATE - sy-uname 직접 사용
  UPDATE y_my_custom_table
  SET  status       = 'ACTIVE',
       amount       = 10000,
       currency     = 'KRW',
       comment      = 'Direct SY-UNAME test',
       changed_by   = sy-uname,
       changed_at   = sy-datlo,
       changed_on   = sy-timlo
  WHERE id = '12345'.

  " 다중 필드 UPDATE - sy-uname 직접 사용
  UPDATE z_document_table
  SET  created_by   = sy-uname,
       created_at   = sy-datum,
       last_updated = sy-datum
  WHERE doc_id = 'DOC001'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_structure_field_syuname
*&---------------------------------------------------------------------*
FORM test_structure_field_syuname.
  " 구조체에 sy-uname을 할당한 후 UPDATE에서 사용하는 케이스
  
  DATA: ls_ztable TYPE z_my_custom_table.

  " ls_ztable 스트럭처에 업데이트할 데이터 값들을 설정합니다.
  " ID는 WHERE 절에 사용될 키 값입니다.
  ls_ztable-id       = '12345'.
  ls_ztable-status   = 'COMP'.
  ls_ztable-amount   = '10000'.
  ls_ztable-currency = 'KRW'.
  ls_ztable-comment  = '프로세스 완료'.

  " sy-uname을 사용하여 변경한 사용자 정보를 필드에 할당합니다.
  ls_ztable-changed_by = sy-uname.
  ls_ztable-changed_at = sy-datlo.  " 변경 날짜 (로컬 시간)
  ls_ztable-changed_on = sy-timlo.  " 변경 시간 (로컬 시간)

  " UPDATE 구문을 사용하여 여러 필드를 한 번에 업데이트합니다.
  UPDATE z_my_custom_table
    SET  status       = ls_ztable-status,
         amount       = ls_ztable-amount,
         currency     = ls_ztable-currency,
         comment      = ls_ztable-comment,
         changed_by   = ls_ztable-changed_by,
         changed_at   = ls_ztable-changed_at,
         changed_on   = ls_ztable-changed_on
    WHERE id = ls_ztable-id.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_mixed_update_patterns
*&---------------------------------------------------------------------*
FORM test_mixed_update_patterns.
  " 여러 패턴을 혼합한 복잡한 UPDATE 케이스
  
  DATA: ls_doc TYPE z_document,
        lv_user TYPE sy-uname.

  " 변수에 sy-uname 할당
  lv_user = sy-uname.
  
  " 구조체 필드에 할당
  ls_doc-created_by = sy-uname.
  ls_doc-modified_by = lv_user.
  ls_doc-created_at = sy-datum.

  " 혼합 패턴 UPDATE: 직접 사용, 변수 사용, 구조체 필드 사용
  UPDATE y_document_master
  SET  created_by    = sy-uname,           " 직접 사용
       modified_by   = lv_user,            " 변수 사용
       approved_by   = ls_doc-created_by,  " 구조체 필드 사용
       status        = 'APPROVED',
       approval_date = sy-datum
  WHERE document_id = 'DOC123'.

  " 추가적인 구조체 기반 UPDATE
  UPDATE z_audit_log
  SET  user_id      = ls_doc-created_by,
       action_date  = ls_doc-created_at,
       action_type  = 'UPDATE'
  WHERE log_id = 'LOG001'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_edge_cases
*&---------------------------------------------------------------------*
FORM test_edge_cases.
  " 엣지 케이스들
  
  DATA: lt_batch TYPE TABLE OF z_batch_data,
        ls_batch LIKE LINE OF lt_batch.

  " LOOP 내에서 UPDATE 사용
  LOOP AT lt_batch INTO ls_batch.
    ls_batch-processed_by = sy-uname.
    ls_batch-processed_at = sy-datum.
    
    UPDATE z_batch_table
    SET  processed_by = ls_batch-processed_by,
         processed_at = ls_batch-processed_at,
         status       = 'PROCESSED'
    WHERE batch_id = ls_batch-batch_id.
  ENDLOOP.

  " WHERE 절에 sy-uname 사용하면서 SET에도 사용
  UPDATE z_user_session
  SET  last_activity = sy-datum,
       session_status = 'ACTIVE',
       updated_by     = sy-uname
  WHERE user_id = sy-uname
    AND session_id = 'CURRENT'.

ENDFORM.
