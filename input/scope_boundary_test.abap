* 스코프 경계 테스트 ABAP 코드
DATA: lv_user TYPE sy-uname,
      lv_temp TYPE string.

* 테스트 케이스 1: PERFORM 호출로 스코프 경계
START-OF-SELECTION.
  lv_user = sy-uname.
  lv_temp = lv_user.
  PERFORM update_database USING lv_temp.

* 테스트 케이스 2: INCLUDE로 스코프 경계
DATA: lv_user2 TYPE sy-uname.
lv_user2 = sy-uname.
INCLUDE zprog_include.

* 테스트 케이스 3: SUBMIT으로 스코프 경계
DATA: lv_user3 TYPE sy-uname.
lv_user3 = sy-uname.
SUBMIT zprog_test WITH s_user = lv_user3.

* 테스트 케이스 4: FORM 정의로 스코프 경계
DATA: lv_user4 TYPE sy-uname.
lv_user4 = sy-uname.
FORM update_database USING p_user TYPE any.
  " 여기서는 새로운 스코프
ENDFORM.

* 테스트 케이스 5: FUNCTION 정의로 스코프 경계
DATA: lv_user5 TYPE sy-uname.
lv_user5 = sy-uname.
FUNCTION z_test_function.
  " 여기서는 새로운 스코프
ENDFUNCTION.

* 테스트 케이스 6: METHOD 정의로 스코프 경계
DATA: lv_user6 TYPE sy-uname.
lv_user6 = sy-uname.
METHOD test_method.
  " 여기서는 새로운 스코프
ENDMETHOD.

* 테스트 케이스 7: CLASS 정의로 스코프 경계
DATA: lv_user7 TYPE sy-uname.
lv_user7 = sy-uname.
CLASS cl_test DEFINITION.
  " 여기서는 새로운 스코프
ENDCLASS.
