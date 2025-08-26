
* 여러 줄로 작성된 SELECT-OPTIONS
SELECT-OPTIONS: s_user FOR sy-uname,
                s_date FOR sy-datum,
                s_lang FOR sy-langu.

* 여러 줄로 작성된 PARAMETERS
PARAMETERS: p_user TYPE sy-uname,
            p_date TYPE sy-datum,
            p_lang LIKE sy-langu.
    