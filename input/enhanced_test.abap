*&---------------------------------------------------------------------*
*& Enhanced Test Cases for Z/Y Table + RFC Focus
*&---------------------------------------------------------------------*
REPORT Z_ENHANCED_TEST.

" ===== 테스트 케이스 1: 선언만 (힌트 확인) =====
DATA: lv_user TYPE sy-uname.
PARAMETERS: p_user LIKE sy-uname.
SELECT-OPTIONS: s_user FOR sy-uname.

" ===== 테스트 케이스 2: FIELD-SYMBOLS + Z 테이블 =====
FIELD-SYMBOLS: <fs_user> TYPE any.
ASSIGN sy-uname TO <fs_user>.
ls_zdoc-created_by = <fs_user>.
INSERT zdocuments FROM ls_zdoc.

" ===== 테스트 케이스 3: RFC 파라미터 =====
lv_user = sy-uname.
CALL FUNCTION 'Z_USER_RFC'
  EXPORTING
    iv_user = lv_user.

" ===== 테스트 케이스 4: 읽기 전용 사용 (힌트) =====
lv_current = sy-uname.
SELECT * FROM ztable WHERE user_id = lv_current.
READ TABLE lt_users WITH KEY user = lv_current.

" ===== 테스트 케이스 5: DELETE FROM WA (ECC 6.0) =====
wa_zdoc-changed_by = sy-uname.
DELETE zdocuments FROM wa_zdoc.

" ===== 테스트 케이스 6: 일반 테이블 (추적 안됨) =====
ls_normal-user = sy-uname.
INSERT normal_table FROM ls_normal.
