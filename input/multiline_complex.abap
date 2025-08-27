
* 여러 줄 복합 구문
SELECT-OPTIONS: s_user FOR ztable-user, " 기본값 sy-uname 고려
                s_date FOR ztable-date, " 기본값 sy-datum 고려
                s_langu FOR ztable-lang DEFAULT sy-langu.
            