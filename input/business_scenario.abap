
* 사용자 관리 프로그램
REPORT zuser_management.

* 선택 화면 정의
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_user TYPE sy-uname DEFAULT sy-uname,
            p_date TYPE sy-datum DEFAULT sy-datum,
            p_lang LIKE sy-langu DEFAULT sy-langu.
SELECTION-SCREEN END OF BLOCK b1.

* 선택 옵션 정의
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_user FOR sy-uname,
                s_date FOR sy-datum,
                s_lang FOR sy-langu.
SELECTION-SCREEN END OF BLOCK b2.
    