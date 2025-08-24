*&---------------------------------------------------------------------*
*& Report  Z_RFC_EXAMPLE_WITH_SY_UNAME
*&
*&---------------------------------------------------------------------*
*&
*& Description:
*& 이 프로그램은 현재 로그인된 사용자의 ID (SY-UNAME)를 이용하여
*& RFC를 호출하고, 원격 시스템에서 사용자 정보를 조회하는 예제입니다.
*& 표준 BAPI인 'BAPI_USER_GET_DETAIL'을 사용하며, RFC 호출의
*& 기본 구조, 파라미터 전달, 예외 처리 방법을 보여줍니다.
*&
*&---------------------------------------------------------------------*

REPORT z_rfc_example_with_sy_uname.

*----------------------------------------------------------------------*
* §1. 전역 데이터 선언 (Global Data Declarations)
*----------------------------------------------------------------------*

* RFC 호출에 사용할 파라미터 및 변수를 선언합니다.
DATA:
  " SY-UNAME을 저장할 로컬 변수
  lv_user_id      TYPE sy-uname,

  " RFC Destination을 저장할 변수 (SM59 트랜잭션에서 설정)
  lv_rfc_dest     TYPE rfcdest VALUE 'NONE', " 기본값 'NONE'은 로컬 호출을 의미

  " BAPI_USER_GET_DETAIL BAPI의 구조체 및 테이블 타입 선언
  ls_address      TYPE bapiaddr3,  " 사용자 주소 정보
  ls_logondata    TYPE bapilogond, " 사용자 로그온 정보
  ls_defaults     TYPE bapidefaul, " 사용자 기본값 정보
  ls_company      TYPE bapiuscomp, " 사용자 회사 정보

  " BAPI 호출 결과를 담을 Return 메시지 테이블
  lt_return       TYPE TABLE OF bapiret2,
  ls_return       TYPE bapiret2.

*----------------------------------------------------------------------*
* §2. 선택 화면 정의 (Selection Screen)
*----------------------------------------------------------------------*

* 사용자가 직접 RFC Destination을 입력할 수 있도록 파라미터를 정의합니다.
* 이를 통해 동적으로 다른 시스템을 대상으로 RFC를 테스트할 수 있습니다.
PARAMETERS:
  p_dest TYPE rfcdest OBLIGATORY DEFAULT 'NONE'.

*----------------------------------------------------------------------*
* §3. 프로그램 초기화 (Initialization)
*----------------------------------------------------------------------*

* 프로그램이 시작되기 전에 실행되는 부분입니다.
INITIALIZATION.
  " RFC Destination 파라미터의 기본값을 설정합니다.
  " 실제 운영 환경에서는 개발, 품질, 운영 시스템에 맞는 Destination을
  " 지정해야 합니다. 예: 'DEV_100', 'QAS_200' 등
  lv_rfc_dest = p_dest.


*----------------------------------------------------------------------*
* §4. 메인 로직 시작 (Start of Main Logic)
*----------------------------------------------------------------------*

START-OF-SELECTION.

  " 1. 현재 사용자 ID를 로컬 변수에 할당
  " SY-UNAME 시스템 변수는 현재 프로그램을 실행하는 사용자의 ID를 담고 있습니다.
  " 이 값을 로컬 변수 LV_USER_ID에 복사하여 RFC 호출 시 파라미터로 사용합니다.
  lv_user_id = sy-uname.

  WRITE: / '▶ 프로그램 시작'.
  ULINE.
  WRITE: / '1. 현재 로그인 사용자 ID (SY-UNAME):', lv_user_id.
  WRITE: / '2. RFC 호출 대상 시스템 (Destination):', p_dest.
  SKIP 1.


  " 2. RFC 호출을 통한 사용자 정보 조회
  " CALL FUNCTION 키워드를 사용하여 원격 시스템의 함수를 호출합니다.
  " DESTINATION 파라미터에 지정된 시스템으로 요청이 전달됩니다.
  PERFORM call_rfc_get_user_detail.


  " 3. RFC 호출 결과 처리
  " RFC 호출이 성공적으로 완료되었는지, 혹은 오류가 발생했는지 확인하고
  " 그에 따른 후속 처리를 진행합니다.
  PERFORM process_rfc_results.


*&---------------------------------------------------------------------*
*&      Form  CALL_RFC_GET_USER_DETAIL
*&---------------------------------------------------------------------*
* RFC를 호출하여 사용자 정보를 가져오는 서브루틴
*----------------------------------------------------------------------*
FORM call_rfc_get_user_detail.

  WRITE: / '3. RFC 호출 시작...'.

  " BAPI_USER_GET_DETAIL 함수를 원격으로 호출합니다.
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    DESTINATION p_dest
    EXPORTING
      username    = lv_user_id    " IMPORTING 파라미터: 조회할 사용자 ID
    IMPORTING
      address     = ls_address    " EXPORTING 파라미터: 주소 정보
      logondata   = ls_logondata  " EXPORTING 파라미터: 로그온 정보
      defaults    = ls_defaults   " EXPORTING 파라미터: 기본값 정보
      company     = ls_company    " EXPORTING 파라미터: 회사 정보
    TABLES
      return      = lt_return     " RETURN 파라미터: 처리 결과 메시지
    EXCEPTIONS
      communication_failure = 1   " 통신 오류
      system_failure        = 2   " 시스템 오류
      OTHERS                = 3.  " 기타 오류


  " RFC 호출 자체의 성공/실패 여부를 SY-SUBRC 값으로 확인합니다.
  " SY-SUBRC가 0이 아니면, 통신 문제나 시스템 레벨의 심각한 오류가
  " 발생한 것입니다. (예: Destination을 찾을 수 없거나, 시스템 다운 등)
  IF sy-subrc <> 0.
    " 예외(Exception) 처리
    WRITE: / '----------------------------------------------------------------------'.
    WRITE: / '!! RFC 호출 실패 (SY-SUBRC =', sy-subrc, ') !!'.
    CASE sy-subrc.
      WHEN 1.
        WRITE: / '오류 유형: Communication Failure'.
        WRITE: / '해결 방안: RFC Destination 설정(SM59) 및 네트워크 상태를 점검하세요.'.
      WHEN 2.
        WRITE: / '오류 유형: System Failure'.
        WRITE: / '해결 방안: 대상 시스템의 Dump(ST22)나 시스템 로그(SM21)를 확인하세요.'.
      WHEN OTHERS.
        WRITE: / '오류 유형: 기타 알 수 없는 오류'.
    ENDCASE.
    WRITE: / '----------------------------------------------------------------------'.
    " 프로그램 중단 또는 오류 처리 로직 추가
    STOP.
  ELSE.
    WRITE: / '   RFC 호출 완료. 결과 분석을 시작합니다.'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  PROCESS_RFC_RESULTS
*&---------------------------------------------------------------------*
* RFC 호출 결과를 분석하고 출력하는 서브루틴
*----------------------------------------------------------------------*
FORM process_rfc_results.

  DATA: lv_error_found TYPE c.
  CLEAR: lv_error_found.

  SKIP 1.
  WRITE: / '4. RFC 결과 처리'.
  ULINE.

  " BAPI의 RETURN 테이블을 확인하여 비즈니스 로직 상의 오류가 있는지 확인합니다.
  " RETURN 테이블의 TYPE 필드가 'E'(Error), 'A'(Abort)인 경우 오류로 간주합니다.
  LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
    lv_error_found = 'X'.
    EXIT.
  ENDLOOP.

  " 오류 메시지가 발견된 경우
  IF lv_error_found = 'X'.
    WRITE: / '!! BAPI 처리 중 오류가 발생했습니다.'.
    WRITE: / '----------------------------------------------------------------------'.
    WRITE: / '   오류 메시지 목록:'.
    ULINE AT 3(60).
    " 전체 오류 메시지를 출력합니다.
    LOOP AT lt_return INTO ls_return.
      WRITE: / ls_return-message.
    ENDLOOP.
    ULINE AT 3(60).
    WRITE: / '----------------------------------------------------------------------'.

  " 오류 없이 성공적으로 데이터를 수신한 경우
  ELSE.
    WRITE: / '▶ 사용자 정보 조회 성공'.
    WRITE: / '----------------------------------------------------------------------'.

    " IMPORTING 파라미터로 받은 구조체의 내용을 출력합니다.
    WRITE: / '   [기본 정보]'.
    WRITE: / '   - 성명 (Full Name):', ls_address-fullname.
    WRITE: / '   - 부서 (Department):', ls_address-department.
    WRITE: / '   - 이메일 (E-Mail):', ls_address-e_mail.
    WRITE: / '   - 마지막 로그온 날짜:', ls_logondata-lastgdat.
    WRITE: / '   - 마지막 로그온 시간:', ls_logondata-lastgtime.
    WRITE: / '   - 시작 메뉴 (Start Menu):', ls_defaults-start_menu.
    WRITE: / '   - 출력 장치 (Output Device):', ls_defaults-spoolde.
    WRITE: / '----------------------------------------------------------------------'.

    " 성공 메시지도 출력할 수 있습니다.
    WRITE: / '   [성공 메시지]'.
    LOOP AT lt_return INTO ls_return WHERE type = 'S'. " Success 메시지
      WRITE: / '   -', ls_return-message.
    ENDLOOP.
    WRITE: / '----------------------------------------------------------------------'.
  ENDIF.

  SKIP 2.
  WRITE: / '▶ 프로그램 종료'.
  ULINE.

ENDFORM.