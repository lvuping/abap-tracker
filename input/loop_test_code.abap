REPORT Z_LOOP_PATTERN_TEST.

* ========================================
* LOOP 패턴 테스트 케이스들
* ========================================

DATA: t_flight_data TYPE TABLE OF sflight,
      s_flight_data TYPE sflight,
      wa_flight_add TYPE zflight_add,
      it_flight_add TYPE TABLE OF zflight_add,
      gv_user TYPE sy-uname.

* 테스트 케이스 1: 기본 LOOP 패턴 - SY-UNAME 직접 할당
LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = sy-uname.
  wa_flight_add-carrid = s_flight_data-carrid.
  wa_flight_add-connid = s_flight_data-connid.
  APPEND wa_flight_add TO it_flight_add.
  CLEAR wa_flight_add.
ENDLOOP.

INSERT ztable FROM TABLE it_flight_add.

* 테스트 케이스 2: 변수 전파 후 LOOP 내 할당
gv_user = sy-uname.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = gv_user.
  wa_flight_add-carrid = s_flight_data-carrid.
  APPEND wa_flight_add TO it_flight_add.
  CLEAR wa_flight_add.
ENDLOOP.

INSERT zflight_add FROM TABLE it_flight_add.

* 테스트 케이스 3: 구조체 필드에 직접 SY-UNAME 할당
LOOP AT t_flight_data INTO s_flight_data.
  s_flight_data-created_by = sy-uname.
  MODIFY t_flight_data FROM s_flight_data.
ENDLOOP.

UPDATE ztable_flight FROM TABLE t_flight_data.

* 테스트 케이스 4: 중첩 LOOP 패턴
DATA: t_booking TYPE TABLE OF sbook,
      s_booking TYPE sbook,
      wa_audit TYPE zaudit_log.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = sy-uname.
  wa_flight_add-carrid = s_flight_data-carrid.
  
  LOOP AT t_booking INTO s_booking WHERE carrid = s_flight_data-carrid.
    wa_audit-user_id = sy-uname.
    wa_audit-booking_id = s_booking-bookid.
    INSERT zaudit_log FROM wa_audit.
  ENDLOOP.
  
  APPEND wa_flight_add TO it_flight_add.
ENDLOOP.

MODIFY zflight_master FROM TABLE it_flight_add.

* 테스트 케이스 5: LOOP 후 DELETE 작업
LOOP AT it_flight_add INTO wa_flight_add.
  wa_flight_add-status = 'X'.
  MODIFY it_flight_add FROM wa_flight_add.
ENDLOOP.

DELETE FROM zflight_temp WHERE username = sy-uname.

* 테스트 케이스 6: CONCATENATE와 LOOP 조합
DATA: gv_full_name TYPE string.

CONCATENATE 'USER:' sy-uname INTO gv_full_name.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = gv_full_name.
  wa_flight_add-carrid = s_flight_data-carrid.
  APPEND wa_flight_add TO it_flight_add.
ENDLOOP.

INSERT zuser_flight FROM TABLE it_flight_add.

* 테스트 케이스 7: PERFORM과 LOOP 조합
PERFORM set_user_info USING sy-uname CHANGING gv_user.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = gv_user.
  wa_flight_add-carrid = s_flight_data-carrid.
  APPEND wa_flight_add TO it_flight_add.
ENDLOOP.

MODIFY zflight_history FROM TABLE it_flight_add.

FORM set_user_info USING iv_user TYPE sy-uname
                  CHANGING cv_user TYPE sy-uname.
  cv_user = iv_user.
ENDFORM.

* 테스트 케이스 8: RFC 호출과 LOOP 조합
DATA: gv_result TYPE string.

CALL FUNCTION 'Z_GET_USER_INFO'
  EXPORTING
    iv_user = sy-uname
  IMPORTING
    ev_result = gv_result.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = gv_result.
  wa_flight_add-carrid = s_flight_data-carrid.
  APPEND wa_flight_add TO it_flight_add.
ENDLOOP.

* 테스트 케이스 9: WHERE 조건과 LOOP
SELECT * FROM sflight
  INTO TABLE t_flight_data
  WHERE created_by = sy-uname.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = sy-uname.
  wa_flight_add-carrid = s_flight_data-carrid.
  APPEND wa_flight_add TO it_flight_add.
ENDLOOP.

INSERT zflight_filtered FROM TABLE it_flight_add.

* 테스트 케이스 10: SPLIT과 LOOP 조합
DATA: gv_user_part TYPE string.

SPLIT sy-uname AT '-' INTO gv_user gv_user_part.

LOOP AT t_flight_data INTO s_flight_data.
  wa_flight_add-username = gv_user.
  wa_flight_add-user_part = gv_user_part.
  wa_flight_add-carrid = s_flight_data-carrid.
  APPEND wa_flight_add TO it_flight_add.
ENDLOOP.

UPDATE zflight_split FROM TABLE it_flight_add.
