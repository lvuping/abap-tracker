*&---------------------------------------------------------------------*
*& Report ZHARDCODE_TEST
*&---------------------------------------------------------------------*
*& SY-UNAME 하드코딩 패턴 테스트용 ABAP 코드
*&---------------------------------------------------------------------*

REPORT zhardcode_test.

DATA: lv_user TYPE sy-uname,
      lv_authorized TYPE abap_bool,
      lv_result TYPE string.

* 권한 체크 시나리오 (하드코딩 패턴)
IF sy-uname = 'ADMIN'.
  lv_authorized = abap_true.
  WRITE: 'Admin user detected'.
ELSEIF sy-uname EQ 'MANAGER'.
  lv_authorized = abap_true.
  WRITE: 'Manager user detected'.
ELSEIF sy-uname <> 'GUEST'.
  lv_authorized = abap_false.
  WRITE: 'Regular user'.
ELSE.
  lv_authorized = abap_false.
  WRITE: 'Guest user'.
ENDIF.

* CHECK 문 하드코딩
CHECK sy-uname NE 'SYSTEM'.

* 데이터베이스 쿼리에서 하드코딩
SELECT * FROM users
  INTO TABLE @DATA(lt_users)
  WHERE user_id = @sy-uname
    AND sy-uname <> 'GUEST'
    AND status = 'ACTIVE'.

* ASSERT 문 하드코딩 (개발환경에서만)
ASSERT sy-uname = 'DEVELOPER'.

* 일반 비교 하드코딩
lv_result = COND #( WHEN sy-uname = 'TESTUSER' THEN 'Test Mode'
                    WHEN sy-uname EQ 'PRODUSER' THEN 'Production Mode'
                    ELSE 'Unknown Mode' ).

WRITE: / 'Current user:', sy-uname.
WRITE: / 'Authorization:', lv_authorized.
WRITE: / 'Mode:', lv_result.
