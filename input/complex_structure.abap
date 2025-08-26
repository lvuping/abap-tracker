
* 복잡한 구조체를 사용한 PARAMETERS
PARAMETERS: p_header TYPE zheader,
            p_user TYPE sy-uname,
            p_date TYPE sy-datum,
            p_detail TYPE zdetail.

* 복잡한 구조체를 사용한 SELECT-OPTIONS
SELECT-OPTIONS: s_header FOR zheader,
                s_user FOR sy-uname,
                s_date FOR sy-datum,
                s_detail FOR zdetail.
    