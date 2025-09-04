
* DEFAULT 키워드를 사용한 SELECT-OPTIONS
SELECT-OPTIONS s_uname FOR bkpf-usnam DEFAULT sy-uname.

* DEFAULT 키워드를 사용한 PARAMETERS
PARAMETERS p_carrid TYPE s_carr_id DEFAULT 'LH'.
PARAMETERS p_city TYPE s_from_cit DEFAULT 'FRANKFURT' LOWER CASE.
PARAMETERS p_name TYPE sy-uname DEFAULT sy-uname.
    