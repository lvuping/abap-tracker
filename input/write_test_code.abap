REPORT Z_WRITE_PATTERN_TEST.

* ========================================
* WRITE 패턴 테스트 케이스들
* ========================================

DATA: gv_user TYPE sy-uname,
      gv_date TYPE sy-datum,
      gv_time TYPE sy-uzeit,
      gv_temp TYPE string.

* 테스트 케이스 1: 기본 변수 할당 후 단순 WRITE
gv_user = sy-uname.
WRITE gv_user.

* 테스트 케이스 2: 직접 SY-UNAME WRITE
WRITE sy-uname.

* 테스트 케이스 3: 연속 WRITE (콜론 사용) - 여러 줄
WRITE:  sy-datum,
        sy-uname,
        sy-uzeit.

* 테스트 케이스 4: 연속 WRITE (콜론 사용) - 한 줄
WRITE: gv_date, gv_user, gv_time.

* 테스트 케이스 5: 여러 변수 WRITE (콤마 구분)
WRITE sy-datum, sy-uname, sy-uzeit.

* 테스트 케이스 6: 변수 전파 후 WRITE
gv_temp = sy-uname.
gv_user = gv_temp.
WRITE gv_user.

* 테스트 케이스 7: 구조체 필드 할당 후 WRITE
DATA: BEGIN OF gs_doc,
        user TYPE sy-uname,
        date TYPE sy-datum,
      END OF gs_doc.

gs_doc-user = sy-uname.
WRITE gs_doc-user.

* 테스트 케이스 8: MOVE 후 WRITE
MOVE sy-uname TO gv_user.
WRITE gv_user.

* 테스트 케이스 9: 복잡한 여러 줄 WRITE
WRITE:  'User:',
        sy-uname,
        'Date:',
        sy-datum.

* 테스트 케이스 10: CONCATENATE 후 WRITE
CONCATENATE 'User:' sy-uname INTO gv_temp.
WRITE gv_temp.

* 테스트 케이스 11: SPLIT 후 WRITE
SPLIT sy-uname AT '-' INTO gv_user gv_temp.
WRITE gv_user.

* 테스트 케이스 12: PERFORM을 통한 변수 전파 후 WRITE
PERFORM set_user USING sy-uname CHANGING gv_user.
WRITE gv_user.

FORM set_user USING iv_input TYPE sy-uname
              CHANGING cv_output TYPE sy-uname.
  cv_output = iv_input.
ENDFORM.

* 테스트 케이스 13: WHERE 조건과 함께 사용
SELECT SINGLE * FROM usr02
  INTO @DATA(ls_user)
  WHERE bname = @sy-uname.

WRITE ls_user-bname.

* 테스트 케이스 14: UPDATE 문과 WRITE 조합
DATA: ls_document TYPE zdocuments.
ls_document-created_by = sy-uname.
INSERT zdocuments FROM ls_document.
WRITE ls_document-created_by.

* 테스트 케이스 15: RFC 호출과 WRITE 조합
CALL FUNCTION 'Z_RFC_USER_INFO'
  EXPORTING
    iv_user = sy-uname
  IMPORTING
    ev_result = gv_temp.

WRITE gv_temp.
