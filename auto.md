Markdown

# SAP ABAP 개발 프로세스 자동화 올인원 가이드

이 문서는 반복적인 SAP ABAP 수정 작업을 반자동화하여 개발 생산성을 극대화하기 위한 전체 구현 가이드입니다. ABAP 유틸리티 및 RFC 생성부터 AutoHotkey 스크립트 작성까지, 모든 단계를 상세하게 설명합니다.

---

## 0. 시작하기 전에: 준비물 확인

본격적인 시작에 앞서, 아래 준비물이 갖추어져 있는지 확인해주세요.

-   **SAP GUI**: 당연히 설치되어 있어야 하며, **GUI Scripting이 활성화**되어 있어야 합니다. (SAP Logon 옵션에서 활성화 가능)
-   **AutoHotkey**: v1 또는 v2가 설치되어 있어야 합니다. 이 가이드는 v1 기준으로 작성되었습니다.
-   **개발 권한**: SAP 시스템에서 ABAP 프로그램, Function Module, 트랜잭션을 생성할 수 있는 개발 권한이 필요합니다.

---

## 1. 자동화 아키텍처 이해: 왜 RFC인가?

단순히 화면을 조작하는 GUI 스크립팅 방식도 있지만, 우리는 더 빠르고 안정적인 **RFC(Remote Function Call)** 방식을 핵심으로 사용합니다.

-   **RFC 직접 호출**: AutoHotkey가 SAP GUI 화면을 거치지 않고, SAP 서버의 함수(로직)를 직접 호출하여 데이터를 주고받는 방식입니다.
-   **GUI 스크립팅**: AutoHotkey가 사람처럼 마우스 클릭, 키보드 입력을 흉내 내어 화면을 조작하는 방식입니다.

| 구분        | RFC 직접 호출                     | GUI 스크립팅 (화면 조작)             |
| :---------- | :-------------------------------- | :----------------------------------- |
| **속도** | **매우 빠름 (100ms 이하)** | 느림 (UI 반응 속도에 따라 수 초 이상) |
| **안정성** | **높음 (화면 구조 변경에 무관)** | 중간 (UI 변경 시 스크립트 수정 필요) |
| **실행 방식** | **백그라운드 실행 가능** | GUI 화면이 반드시 활성화되어야 함    |

이 가이드에서는 두 방식의 장점을 모두 활용합니다. **데이터 조회**는 RFC로 빠르게 처리하고, **화면 이동이나 최종 입력**은 GUI 스크립팅을 사용합니다.

---

## 2. SAP 백엔드 오브젝트 생성 (ABAP)

자동화의 핵심 두뇌 역할을 할 ABAP 오브젝트들을 생성합니다.

### 2-1. 메인 프로그램 조회/이동용 유틸리티 프로그램

-   **역할**: 하위 오브젝트(Include 등)의 메인 프로그램을 찾아 즉시 에디터로 이동시켜 줍니다.

1.  **T-CODE: `SE38`** 실행
2.  `프로그램`에 `Z_FIND_MAIN` 입력 후 `생성` 클릭
3.  `속성` 창에서 `제목`: `Find Main Program Utility`, `유형`: `실행 프로그램` 설정 후 저장
4.  아래 소스코드를 전체 복사하여 붙여넣고 `Ctrl+F3`으로 활성화(Activate)

```abap
*&---------------------------------------------------------------------*
*& Report Z_FIND_MAIN
*&---------------------------------------------------------------------*
REPORT z_find_main.

PARAMETERS: p_objnm TYPE sobj_name OBLIGATORY. "입력 파라미터

DATA: lv_main_prog  TYPE program,
      lt_where_used TYPE TABLE OF rseu_wn,
      lv_object_type TYPE trobjtype.

START-OF-SELECTION.
  DATA(lv_objnm) = p_objnm.
  TRANSLATE lv_objnm TO UPPER CASE.

  " 1. 가장 빠른 방법: Include의 경우 D010SINF 직접 조회
  SELECT SINGLE master
    INTO lv_main_prog
    FROM d010sinf
   WHERE prog = lv_objnm.

  " 2. D010SINF에 없거나 다른 타입일 경우, 표준 FM 사용 (더 안정적)
  IF sy-subrc <> 0.
    SELECT SINGLE objtype INTO lv_object_type FROM tadir WHERE obj_name = lv_objnm.
    IF sy-subrc = 0.
      CALL FUNCTION 'RS_GET_WHERE_USED_LIST'
        EXPORTING
          object_name     = lv_objnm
          object_type     = lv_object_type
        TABLES
          where_used_list = lt_where_used
        EXCEPTIONS OTHERS = 1.

      READ TABLE lt_where_used INTO DATA(ls_where_used) WITH KEY object_type = 'REPT'. "REPT = Main Program
      IF sy-subrc = 0.
        lv_main_prog = ls_where_used-object_name.
      ENDIF.
    ENDIF.
  ENDIF.

  " 3. 찾은 메인 프로그램으로 즉시 이동
  IF lv_main_prog IS NOT INITIAL.
    SET PARAMETER ID 'RID' FIELD 'EDITOR'.
    SET PARAMETER ID 'LIB' FIELD lv_main_prog.
    CALL TRANSACTION 'SE38' AND SKIP FIRST SCREEN.
  ELSE.
    MESSAGE |메인 프로그램을 찾을 수 없습니다: { lv_objnm }| TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  ```
T-CODE: SE93 실행

트랜잭션 코드에 ZFM 입력 후 생성 클릭

Start object 섹션에서 Program and selection screen (report transaction) 선택

Program에 Z_FIND_MAIN 입력 후 저장하여 트랜잭션 코드 생성을 완료합니다.

2-2. 오브젝트 잠금 확인용 RFC
역할: 수정하려는 오브젝트를 다른 사용자가 이미 작업 중인지 미리 확인합니다.

T-CODE: SE37 실행

Function Module에 Z_CHECK_OBJECT_LOCK 입력 후 생성 클릭

Function group 지정 후 저장

Properties 탭에서 Processing Type을 **Remote-Enabled Module**로 선택

Import 탭에 파라미터 추가: I_OBJ_NAME TYPE SOBJ_NAME

Export 탭에 파라미터 추가: E_IS_LOCKED TYPE CHAR1, E_LOCKED_BY TYPE UNAME

Source code 탭에 아래 코드를 붙여넣고 Ctrl+F3으로 활성화

ABAP

FUNCTION Z_CHECK_OBJECT_LOCK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_OBJ_NAME) TYPE  SOBJ_NAME
*"  EXPORTING
*"     VALUE(E_IS_LOCKED) TYPE  CHAR1
*"     VALUE(E_LOCKED_BY) TYPE  UNAME
*"----------------------------------------------------------------------
  DATA: lt_enq  TYPE TABLE OF seqg3,
        lv_garg TYPE string.

  CLEAR: E_IS_LOCKED, E_LOCKED_BY.

  " SAP 잠금 테이블(SM12)의 검색 인수를 만듭니다.
  CONCATENATE sy-mandt i_obj_name '*' INTO lv_garg.
  CONDENSE lv_garg NO-GAPS.

  " 잠금 테이블을 직접 읽는 표준 함수를 호출합니다.
  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      garg              = lv_garg
      guname            = '*'
    TABLES
      enq               = lt_enq
    EXCEPTIONS OTHERS   = 1.

  IF sy-subrc = 0 AND lt_enq IS NOT INITIAL.
    READ TABLE lt_enq INDEX 1 INTO DATA(ls_enq).
    E_IS_LOCKED = 'X'. "잠겨있음 플래그
    E_LOCKED_BY = ls_enq-guname. "잠근 사용자 ID
  ENDIF.
ENDFUNCTION.
2-3. CTS 번호 조회용 RFC
역할: 현재 사용자가 작업 중인 오브젝트가 포함된 CTS 번호를 가져옵니다.

T-CODE: SE37 실행

Function Module에 Z_GET_CTS_FOR_OBJECT 입력 후 생성 클릭

Properties 탭에서 Processing Type을 **Remote-Enabled Module**로 선택

Import 탭에 파라미터 추가: I_OBJ_TYPE TYPE E071-OBJECT, I_OBJ_NAME TYPE E071-OBJ_NAME

Export 탭에 파라미터 추가: E_CTS_NUMBER TYPE E071-TRKORR

Source code 탭에 아래 코드를 붙여넣고 Ctrl+F3으로 활성화

ABAP
```abap
FUNCTION Z_GET_CTS_FOR_OBJECT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_OBJ_TYPE) TYPE  E071-OBJECT
*"     VALUE(I_OBJ_NAME) TYPE  E071-OBJ_NAME
*"  EXPORTING
*"     VALUE(E_CTS_NUMBER) TYPE  E071-TRKORR
*"----------------------------------------------------------------------
  " E071(오브젝트 리스트), E070(헤더) 테이블을 조인하여
  " 현재 사용자가 소유하고, 아직 릴리즈되지 않은('D') CTS 번호를 찾는다.
  SELECT SINGLE T1~TRKORR
    INTO E_CTS_NUMBER
    FROM E071 AS T1
    INNER JOIN E070 AS T2
      ON T1~TRKORR = T2~TRKORR
   WHERE T1~OBJECT   = I_OBJ_TYPE
     AND T1~OBJ_NAME = I_OBJ_NAME
     AND T2~AS4USER  = sy-uname
     AND T2~TRSTATUS = 'D'.  " D = Modifiable (수정 가능)
ENDFUNCTION.
3. AutoHotkey 스크립트 구현
이제 SAP와 통신하여 전체 프로세스를 조립할 AutoHotkey 스크립트를 작성합니다.

3-1. 전체 스크립트 코드
아래 코드를 SAP_Automation.ahk 파일로 저장하고 실행합니다. 단축키는 Ctrl + j 입니다.

AutoHotkey

#SingleInstance, Force
SetTitleMatchMode, 2 ; 창 제목의 일부만 일치해도 찾도록 설정

; ###########################################
; ## SAP ABAP 개발 프로세스 자동화 스크립트 ##
; ###########################################

^j::
    ; -------------------- 1. SAP 연결 및 정보 획득 --------------------
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, 오류, SAP GUI 세션을 찾을 수 없습니다.`nSAP Logon 후 스크립트를 실행해주세요.
        Return
    }

    ; 현재 화면의 오브젝트 이름과 타입 획득 (컨트롤 이름은 Window Spy로 확인 필요)
    Try {
        objName := SAP_Session.findById("wnd[0]/titl/usr/txtGS_OBJECT-OBJECT").text
        objType := SAP_Session.findById("wnd[0]/titl/usr/lblGS_OBJECT-OBJ_TYPE").text
    } Catch e {
        MsgBox, 48, 오류, 오브젝트 이름/타입을 가져올 수 없습니다.`nABAP 에디터 화면이 맞는지 확인해주세요.
        Return
    }
    
    ; -------------------- 2. 오브젝트 잠금 상태 확인 --------------------
    FunctionCtrl := ComObjCreate("SAP.Functions.Unicode") ; Unicode 시스템용
    FunctionCtrl.Connection := SAP_Session.Connection
    CheckLockFunc := FunctionCtrl.Add("Z_CHECK_OBJECT_LOCK")
    CheckLockFunc.Exports("I_OBJ_NAME").Value := objName

    Try {
        If (CheckLockFunc.Call() = True) {
            isLocked := CheckLockFunc.Imports("E_IS_LOCKED").Value
            lockedBy := CheckLockFunc.Imports("E_LOCKED_BY").Value
            If (isLocked = "X") {
                MsgBox, 48, 프로세스 중단, %objName% 오브젝트는 현재 %lockedBy% 님이 수정 중입니다.
                Return
            }
        } else {
            Throw, CheckLockFunc.Exception
        }
    } Catch e {
        MsgBox, 16, 오류, 잠금 상태 확인 중 RFC 호출 실패:`n% e.Message
        Return
    }
    MsgBox, 64, 진행, 오브젝트가 수정 가능한 상태입니다. 프로세스를 시작합니다.

    ; -------------------- 3. 메인 프로그램으로 이동 --------------------
    SAP_Session.findById("wnd[0]").maximize()
    SAP_Session.findById("wnd[0]/tbar[0]/okcd").text := "/nZFM"
    SAP_Session.findById("wnd[0]").sendVKey(0) ; Enter
    Sleep, 500
    SAP_Session.findById("wnd[0]/usr/ctxtP_OBJNM").text := objName
    SAP_Session.findById("wnd[0]").sendVKey(8) ; F8
    Sleep, 1000

    ; -------------------- 4. CTS 번호 조회 --------------------
    GetCTSFunc := FunctionCtrl.Add("Z_GET_CTS_FOR_OBJECT")
    GetCTSFunc.Exports("I_OBJ_TYPE").Value := objType
    GetCTSFunc.Exports("I_OBJ_NAME").Value := objName
    
    ctsNumber := ""
    Try {
        If (GetCTSFunc.Call() = True) {
            ctsNumber := GetCTSFunc.Imports("E_CTS_NUMBER").Value
        } else {
            Throw, GetCTSFunc.Exception
        }
    } Catch e {
        MsgBox, 16, 오류, CTS 조회 중 RFC 호출 실패:`n% e.Message
    }
    
    If (ctsNumber = "") {
        MsgBox, 48, 경고, 이 오브젝트에 대한 CTS를 찾을 수 없습니다.
    }

    ; -------------------- 5. 변경 이력 생성 및 추가 --------------------
    InputBox, changeNote, 변경 이력 내용, 수정 내용을 입력하세요.,, 350, 150
    If (ErrorLevel) {
        MsgBox, 프로세스가 사용자에 의해 취소되었습니다.
        Return
    }
    
    ; 새 변경 이력 키와 라인 생성 (여기서는 U6으로 하드코딩, 동적 채번 로직으로 개선 가능)
    newKey := "U6" 
    changeHistoryLine := "*& " . newKey . "  " . A_UserName . "  " . A_YYYY . "." . A_MM . "." . A_DD . "  " . changeNote . " (" . ctsNumber . ")"
    
    Clipboard_Backup := ClipboardAll
    Clipboard := changeHistoryLine
    
    ; 메인 프로그램 주석 마지막에 붙여넣기
    mainEditor := SAP_Session.findById("wnd[0]/usr/cntlEDITOR/shellcont/shell")
    mainEditor.setFocus()
    SendInput, ^{End}{Enter}^v
    
    Clipboard := Clipboard_Backup

    MsgBox, 64, 완료, 메인 프로그램에 변경 이력이 추가되었습니다.`n이제 원래 오브젝트로 돌아가 수정 작업을 진행하고`n라인 끝에 '*& %newKey%' 주석을 추가해주세요.
Return

; ############### SAP GUI 세션 연결 함수 ###############
Connect_To_SAP_Session() {
    Try {
        SapGuiAuto := ComObjGet("SAPGUI")
        application := SapGuiAuto.GetScriptingEngine
        connection := application.Children(0)
        session := connection.Children(0)
        Return session
    } Catch e {
        Return ""
    }
}
3-2. 스크립트 주요 코드 해설
Connect_To_SAP_Session(): 실행 중인 SAP GUI의 첫 번째 세션에 자동으로 연결하는 함수입니다.

ComObjCreate("SAP.Functions.Unicode"): SAP RFC를 호출하기 위한 COM 객체를 생성합니다. 대부분의 최신 SAP 시스템은 Unicode이므로 .Unicode를 붙이는 것이 안전합니다.

FunctionCtrl.Add("..."): ABAP에서 만든 RFC Function Module을 지정합니다.

.Exports("...").Value: RFC의 Import 파라미터에 값을 전달합니다.

.Imports("...").Value: RFC의 Export 파라미터로부터 결과 값을 받아옵니다.

Try...Catch: RFC 호출 등 COM 객체와 통신 시 발생할 수 있는 에러를 잡아내어 스크립트가 비정상 종료되는 것을 방지합니다.

4. 추가 팁 및 고급 주제
4-1. AutoHotkey Window Spy 활용법
스크립트에서 SAP_Session.findById(...) 내부의 컨트롤 ID나 ControlSetText의 컨트롤 이름(ClassNN)을 찾을 때 Window Spy는 필수입니다.

AutoHotkey 설치 폴더에서 AU3_Spy.exe를 실행합니다.

정보를 얻고 싶은 SAP 창의 컨트롤 위로 마우스를 가져갑니다.

Now Under Mouse Cursor 섹션의 ClassNN 값을 확인하여 ControlSend, ControlSetText 등에 활용합니다.

4-2. 동적 변경 이력 채번 로직 (개선 아이디어)
현재 스크립트는 U6을 하드코딩하고 있습니다. 실제 사용을 위해서는 메인 프로그램의 소스코드를 읽어 마지막 번호를 찾아 +1 하는 로직이 필요합니다.

AutoHotkey

; ... (메인 프로그램으로 이동 후) ...

; 1. 전체 텍스트를 클립보드로 복사
SendInput, ^a^c
Sleep, 200
fullText := Clipboard

; 2. 정규식으로 마지막 U넘버 찾기
if RegExMatch(fullText, "s)\*& U(\d+)", lastMatch) {
    newNum := lastMatch1 + 1
    newKey := "U" . newNum
} else {
    newKey := "U1" ; 첫 수정일 경우
}

; ... 이후 newKey 변수를 사용 ...
4-3. 동시 수정 및 CTS 관리
첨부 파일에서 언급된 것처럼, 복잡한 CTS 관리가 필요할 수 있습니다.

신규 CTS 생성: TR_REQUEST_INSERT BAPI/RFC를 호출하여 스크립트로 새 CTS를 만들 수 있습니다.

오브젝트 이동: TR_OBJECTS_CHANGE BAPI/RFC를 사용하여 특정 오브젝트를 다른 CTS로 옮기는 작업도 자동화 가능합니다. 이는 고급 자동화 영역에 해당합니다.