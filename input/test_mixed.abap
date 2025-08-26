
* 타겟 도메인과 일반 필드가 섞인 경우
PARAMETERS: p_user TYPE sy-uname,
            p_field TYPE c LENGTH 20,
            p_date TYPE sy-datum.

SELECT-OPTIONS: s_user FOR sy-uname,
                s_field FOR table-field,
                s_date FOR sy-datum.
    