import re

# MOVE a TO b.
MOVE_PATTERN = re.compile(
    r"^\s*MOVE\s+(?P<source>[\w\d\-\>\[\]]+)\s+TO\s+(?P<target>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# target = source.
ASSIGN_PATTERN = re.compile(
    r"^\s*(?P<target>[\w\d\-\>\[\]]+)\s*=\s*(?P<source>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# a = b = c. 연속 할당 패턴
CHAIN_ASSIGN_PATTERN = re.compile(
    r"^\s*(?P<targets>(?:[\w\d\-\>\[\]]+\s*=\s*)+)(?P<source>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# CALL FUNCTION 'Z_RFC_NAME' ...
RFC_CALL_PATTERN = re.compile(
    r"^\s*CALL\s+FUNCTION\s+'(?P<rfc_name>[\w\d_]+)'\s*(?P<params>.*)",
    re.IGNORECASE | re.DOTALL,  # 여러 줄에 걸친 파라미터 처리
)

# iv_param = lv_var
RFC_PARAM_PATTERN = re.compile(
    r"(?P<param_name>[\w\d_]+)\s*=\s*(?P<param_value>[\w\d\-\>\[\]]+)", re.IGNORECASE
)

# PERFORM subroutine USING var1 var2 CHANGING var3
PERFORM_PATTERN = re.compile(
    r"^\s*PERFORM\s+(?P<subroutine>[\w\d_]+)\s*(?P<params>.*)",
    re.IGNORECASE | re.DOTALL,
)

# USING/CHANGING 파라미터 패턴
USING_PARAM_PATTERN = re.compile(
    r"USING\s+(?P<using_params>.*?)(?=CHANGING|$)", re.IGNORECASE | re.DOTALL
)

CHANGING_PARAM_PATTERN = re.compile(
    r"CHANGING\s+(?P<changing_params>.*?)(?=USING|$)", re.IGNORECASE | re.DOTALL
)

# 구조체 필드 할당: gs_structure-field = variable.
STRUCTURE_ASSIGN_PATTERN = re.compile(
    r"^\s*(?P<target>[\w\d_]+\-[\w\d_]+)\s*=\s*(?P<source>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# WHERE 조건절에서 변수 사용: WHERE field = @variable
WHERE_CONDITION_PATTERN = re.compile(
    r"WHERE\s+[\w\d_]+\s*=\s*@?(?P<variable>[\w\d\-\>\[\]]+)", re.IGNORECASE
)

# FORM 정의에서 파라미터 패턴
FORM_PARAM_PATTERN = re.compile(
    r"FORM\s+[\w\d_]+\s+(?P<params>.*)", re.IGNORECASE | re.DOTALL
)

# FORM 파라미터에서 변수 추출
FORM_USING_PATTERN = re.compile(
    r"USING\s+(?:VALUE\()?(?P<param_name>[\w\d_]+)\)?(?:\s+TYPE\s+[\w\d_]+)?",
    re.IGNORECASE,
)

FORM_CHANGING_PATTERN = re.compile(
    r"CHANGING\s+(?P<param_name>[\w\d_]+)(?:\s+TYPE\s+[\w\d_]+)?", re.IGNORECASE
)

# APPEND, MODIFY 등 내부 테이블 조작
APPEND_PATTERN = re.compile(
    r"^\s*APPEND\s+(?P<source>[\w\d\-\>\[\]]+)\s+TO\s+(?P<target>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# MOVE-CORRESPONDING 패턴
MOVE_CORRESPONDING_PATTERN = re.compile(
    r"^\s*MOVE[-_]CORRESPONDING\s+(?P<source>[\w\d\-\>\[\]]+)\s+TO\s+(?P<target>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# CONCATENATE 패턴
CONCATENATE_PATTERN = re.compile(
    r"^\s*CONCATENATE\s+(?P<sources>.*?)\s+INTO\s+(?P<target>[\w\d\-\>\[\]]+)(?:\s+SEPARATED\s+BY\s+.*?)?\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# CONDENSE 패턴 (문자열 압축)
CONDENSE_PATTERN = re.compile(
    r"^\s*CONDENSE\s+(?P<target>[\w\d\-\>\[\]]+)(?:\s+NO-GAPS)?\s*\.",
    re.IGNORECASE,
)

# TRANSLATE 패턴 (대소문자 변환)
TRANSLATE_PATTERN = re.compile(
    r"^\s*TRANSLATE\s+(?P<target>[\w\d\-\>\[\]]+)\s+(?:TO\s+(?:UPPER|LOWER)\s+CASE|USING\s+.*?)\s*\.",
    re.IGNORECASE,
)

# REPLACE 패턴 (문자열 치환)
REPLACE_PATTERN = re.compile(
    r"^\s*REPLACE\s+.*?\s+IN\s+(?P<target>[\w\d\-\>\[\]]+)\s+WITH\s+(?P<source>[\w\d\-\>\[\]'\"]+)(?:\s+.*?)?\s*\.",
    re.IGNORECASE,
)

# CLEAR 패턴 (변수 초기화)
CLEAR_PATTERN = re.compile(
    r"^\s*CLEAR\s*:\s*(?P<variables>.*?)\s*\.",
    re.IGNORECASE,
)

# COMPUTE 패턴 (계산식 할당)
COMPUTE_PATTERN = re.compile(
    r"^\s*(?:COMPUTE\s+)?(?P<target>[\w\d\-\>\[\]]+)\s*=\s*(?P<expression>.*?)\s*\.",
    re.IGNORECASE,
)

# SPLIT 패턴 (문자열 분할)
SPLIT_PATTERN = re.compile(
    r"^\s*SPLIT\s+(?P<source>[\w\d\-\>\[\]]+)\s+AT\s+.*?\s+INTO\s+(?P<targets>.*?)\s*\.",
    re.IGNORECASE,
)

# INTO 절을 포함한 SELECT 문
SELECT_INTO_PATTERN = re.compile(
    r"^\s*SELECT.*?\s+INTO\s+(?:TABLE\s+)?(?P<target>[\w\d\-\>\[\]]+)(?:\s+FROM\s+.*?)?\s*$",
    re.IGNORECASE | re.DOTALL,
)

# 데이터베이스 작업 패턴들 (중요한 Sink 포인트들)
# UPDATE 문 패턴
UPDATE_SET_PATTERN = re.compile(
    r"^\s*UPDATE:?\s+(?P<table>[\w\d_]+|\([\w\d_]+\))\s+SET\s+(?P<assignments>.*?)\s*(?:WHERE\s+.*?)?\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# UPDATE ... FROM ... 구문 패턴
UPDATE_FROM_PATTERN = re.compile(
    r"^\s*UPDATE:?\s+(?P<table>[\w\d_]+|\([\w\d_]+\))\s+FROM\s+(?:TABLE\s+)?(?P<source>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# INSERT 문 패턴
INSERT_PATTERN = re.compile(
    r"^\s*INSERT\s+(?:INTO\s+)?(?P<table>[\w\d_]+|\([\w\d_]+\))\s+(?:FROM\s+(?:TABLE\s+)?(?P<source>[\w\d\-\>\[\]\(\)]+)|VALUES\s+(?P<values>.*?))\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# MODIFY 문 패턴
MODIFY_PATTERN = re.compile(
    r"^\s*MODIFY:?\s+(?P<table>[\w\d_]+|\([\w\d_]+\))\s+FROM\s+(?:TABLE\s+)?(?P<source>[\w\d\-\>\[\]\(\)]+)(?:\s+.*?)?\s*\.",
    re.IGNORECASE,
)

# DELETE 문 패턴
DELETE_PATTERN = re.compile(
    r"^\s*DELETE\s+(?:FROM\s+)?(?P<table>[\w\d_]+)\s+WHERE\s+(?P<conditions>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# 구조체 필드 할당에서 특정 필드 추출 (ERDAT, ERZET, AENAM 등)
AUDIT_FIELD_PATTERN = re.compile(
    r"^\s*(?P<structure>[\w\d_]+)\-(?P<field>(?:ERDAT|ERZET|AEDAT|AENAM|UDATE|UTIME|UNAME|CDATE|CTIME|CUSER|LDATE|LTIME|LUSER|CREATED_BY|CHANGED_BY|CREATED_AT|CHANGED_AT))\s*=\s*(?P<source>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# COMMIT WORK / ROLLBACK 패턴 (트랜잭션 종료점)
COMMIT_PATTERN = re.compile(
    r"^\s*(?:COMMIT\s+WORK|ROLLBACK\s+WORK?)\s*\.",
    re.IGNORECASE,
)

# CALL TRANSACTION 패턴 (또 다른 중요한 sink)
CALL_TRANSACTION_PATTERN = re.compile(
    r"^\s*CALL\s+TRANSACTION\s+'(?P<tcode>[\w\d_]+)'\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# BDC (Batch Data Communication) 패턴
BDC_FIELD_PATTERN = re.compile(
    r"^\s*(?P<bdc_table>[\w\d_]+)\-(?:FNAM|FVAL)\s*=\s*(?P<value>[\w\d\-\>\[\]'\"]+)\s*\.",
    re.IGNORECASE,
)

# SELECT WHERE 절 패턴 (테이블과 필드 매핑)
SELECT_WHERE_PATTERN = re.compile(
    r"^\s*SELECT.*?FROM\s+(?P<table>[\w\d_]+).*?WHERE.*?(?P<field>[\w\d_]+)\s*=\s*@?(?P<variable>[\w\d\-\>\[\]]+)",
    re.IGNORECASE | re.DOTALL,
)

# 단독 WHERE 조건 패턴 (SELECT와 분리된 WHERE 절)
STANDALONE_WHERE_PATTERN = re.compile(
    r"^\s*(?:AND\s+|OR\s+)?(?P<field>[\w\d_]+)\s*=\s*@?(?P<variable>[\w\d\-\>\[\]]+)",
    re.IGNORECASE,
)

# 조건문 (IF, CHECK) 패턴
CONDITIONAL_CHECK_PATTERN = re.compile(
    r"^\s*(?P<keyword>IF|CHECK)\s+(?P<variable>[\w\d\-\>\[\]]+)\s*(?:EQ|=)\s*(?P<value>.*?)\s*\.",
    re.IGNORECASE,
)

# ===== 확장 패턴들 (기존 extended_patterns.py에서 통합) =====

# 1. SELECT-OPTIONS 패턴
# 예: SELECT-OPTIONS s_usnam FOR sy-uname NO INTERVALS NO-EXTENSION.
SELECT_OPTIONS_PATTERN = re.compile(
    r"^\s*SELECT-OPTIONS\s+(?P<target>[\w\d_]+)\s+FOR\s+(?P<source>sy-uname)\s+NO-INTERVALS\s+NO-EXTENSION\s*\.",
    re.IGNORECASE,
)

# 2. READ TABLE WITH KEY 패턴
# 예: READ TABLE lt_data WITH KEY ernam = sy-uname.
READ_TABLE_WITH_KEY_PATTERN = re.compile(
    r"^\s*READ\s+TABLE\s+.*?\s+WITH\s+KEY\s+.*?ernam\s*=\s*(?P<source>sy-uname)\b.*",
    re.IGNORECASE | re.DOTALL,
)

# 3. DATA TYPE 패턴
# 예: DATA: gv_user TYPE sy-uname.
DATA_TYPE_PATTERN = re.compile(
    r"^\s*DATA\s*:?\s*(?P<target>\w+)\s+TYPE\s+(?P<source>sy-uname)\s*[,.]",
    re.IGNORECASE,
)

# 4. DATA LIKE 패턴
# 예: DATA gv_user LIKE sy-uname.
DATA_LIKE_PATTERN = re.compile(
    r"^\s*DATA\s*:?\s*(?P<target>\w+)\s+LIKE\s+(?P<source>sy-uname)\s*[,.]",
    re.IGNORECASE,
)

# 5. MODIFY table 패턴 (단독)
# 참고: ztable-ernam = sy-uname은 기존 ASSIGN_PATTERN으로 감지 가능
MODIFY_TABLE_PATTERN = re.compile(
    r"^\s*MODIFY\s+(?P<table>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# 6. 연속적인 MOVE 구문 패턴
# 예: MOVE : sy-datum to gt_7100-datum, sy-uname to gt_7100-ernam.
CHAIN_MOVE_PATTERN = re.compile(
    r"^\s*MOVE\s*:\s*(?P<moves>.*?)\s*\.", re.IGNORECASE | re.DOTALL
)

# 연속 MOVE 내에서 개별 할당을 찾기 위한 패턴
MOVE_PAIR_PATTERN = re.compile(
    r",?\s*(?P<source>[\w\d\-\>\[\]]+)\s+TO\s+(?P<target>[\w\d\-\>\[\]]+)"
)

# ===== 하드코딩 패턴들 =====

# SY-UNAME 하드코딩 비교 패턴
# 예: sy-uname = 'ADMIN', sy-uname EQ 'TEST', sy-uname <> 'GUEST', sy-uname NE 'SYSTEM'
SYUNAME_HARDCODE_PATTERN = re.compile(
    r"(?:^|\s+)(?P<variable>sy-uname)\s*(?P<operator>=|EQ|NE|<>|GT|LT|GE|LE)\s*(?P<value>'[^']*'|\"[^\"]*\"|\w+)",
    re.IGNORECASE,
)

# IF문 내 SY-UNAME 하드코딩 패턴
# 예: IF sy-uname = 'ADMIN', IF sy-uname EQ 'TEST'
IF_SYUNAME_HARDCODE_PATTERN = re.compile(
    r"^\s*IF\s+(?P<variable>sy-uname)\s*(?P<operator>=|EQ|NE|<>|GT|LT|GE|LE)\s*(?P<value>'[^']*'|\"[^\"]*\")",
    re.IGNORECASE,
)

# CHECK문 내 SY-UNAME 하드코딩 패턴
# 예: CHECK sy-uname = 'ADMIN', CHECK sy-uname NE 'GUEST'
CHECK_SYUNAME_HARDCODE_PATTERN = re.compile(
    r"^\s*CHECK\s+(?P<variable>sy-uname)\s*(?P<operator>=|EQ|NE|<>|GT|LT|GE|LE)\s*(?P<value>'[^']*'|\"[^\"]*\")",
    re.IGNORECASE,
)

# WHERE 절 내 SY-UNAME 하드코딩 패턴
# 예: WHERE user_id = sy-uname AND sy-uname = 'ADMIN'
WHERE_SYUNAME_HARDCODE_PATTERN = re.compile(
    r"WHERE.*?(?P<variable>sy-uname)\s*(?P<operator>=|EQ|NE|<>|GT|LT|GE|LE)\s*(?P<value>'[^']*'|\"[^\"]*\")",
    re.IGNORECASE | re.DOTALL,
)

# CASE문 내 SY-UNAME 하드코딩 패턴
# 예: CASE sy-uname. WHEN 'ADMIN'.
CASE_SYUNAME_HARDCODE_PATTERN = re.compile(
    r"(?:CASE\s+(?P<variable>sy-uname)|WHEN\s+(?P<value>'[^']*'|\"[^\"]*\"|\w+))",
    re.IGNORECASE,
)

# ASSERT문 내 SY-UNAME 하드코딩 패턴
# 예: ASSERT sy-uname = 'TESTUSER'
ASSERT_SYUNAME_HARDCODE_PATTERN = re.compile(
    r"^\s*ASSERT\s+(?P<variable>sy-uname)\s*(?P<operator>=|EQ|NE|<>|GT|LT|GE|LE)\s*(?P<value>'[^']*'|\"[^\"]*\")",
    re.IGNORECASE,
)

# ===== WRITE 문 패턴들 =====

# 1. 단순 WRITE 패턴
# 예: WRITE sy-uname.
WRITE_SIMPLE_PATTERN = re.compile(
    r"^\s*WRITE\s+(?P<variable>[\w\d\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# 2. 연속 WRITE 패턴 (여러 줄 지원)
# 예: WRITE: sy-datum, sy-uname, sy-uzeit.
# 예: WRITE:  SY-DATUM,
#            SY-UNAME .
WRITE_CHAIN_PATTERN = re.compile(
    r"^\s*WRITE\s*:\s*(?P<variables>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# 3. 여러 변수를 포함한 WRITE 패턴
# 예: WRITE sy-datum, sy-uname, sy-uzeit.
WRITE_MULTIPLE_PATTERN = re.compile(
    r"^\s*WRITE\s+(?P<variables>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# WRITE 문 내에서 개별 변수를 추출하기 위한 패턴
WRITE_VAR_PATTERN = re.compile(
    r"(?:^|,)\s*(?P<variable>[\w\d\-\>\[\]]+)(?=\s*[,.]|$)",
    re.IGNORECASE,
)

# ===== LOOP 구문 패턴들 =====

# 1. LOOP AT 패턴
# 예: LOOP AT t_flight_data INTO s_flight_data.
LOOP_AT_PATTERN = re.compile(
    r"^\s*LOOP\s+AT\s+(?P<table>[\w\d_\[\]]+)\s+INTO\s+(?P<workarea>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# 2. ENDLOOP 패턴
ENDLOOP_PATTERN = re.compile(
    r"^\s*ENDLOOP\s*\.",
    re.IGNORECASE,
)

# ===== 강화된 INSERT 패턴들 =====

# INSERT table FROM TABLE 패턴 (기존 INSERT_PATTERN 보완)
# 예: INSERT ztable FROM TABLE it_flight_add
INSERT_TABLE_FROM_TABLE_PATTERN = re.compile(
    r"^\s*INSERT\s+(?P<table>[\w\d_]+)\s+FROM\s+TABLE\s+(?P<source_table>[\w\d_\[\]]+)\s*\.",
    re.IGNORECASE,
)

# ===== 강화된 UPDATE SET 패턴들 =====

# UPDATE SET에서 sy-uname 직접 사용 감지 패턴
# 예: UPDATE table SET field = sy-uname, field2 = value
UPDATE_SET_SYUNAME_PATTERN = re.compile(
    r"^\s*UPDATE:?\s+(?P<table>[\w\d_]+|\([\w\d_]+\))\s+SET\s+(?P<assignments>.*?)\s*(?:WHERE\s+.*?)?\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# UPDATE SET 필드 할당에서 sy-uname 직접 감지
# 예: field_name = sy-uname
UPDATE_FIELD_SYUNAME_PATTERN = re.compile(
    r"(?P<field>[\w\d_]+)\s*=\s*(?P<source>sy-uname)",
    re.IGNORECASE,
)

# UPDATE SET 필드 할당에서 구조체 필드 감지
# 예: field_name = ls_structure-field_name
UPDATE_FIELD_STRUCTURE_PATTERN = re.compile(
    r"(?P<field>[\w\d_]+)\s*=\s*(?P<source>[\w\d_]+\-[\w\d_]+)",
    re.IGNORECASE,
)

# ===== 확장된 데이터베이스 작업 패턴들 =====

# INSERT INTO table VALUES 패턴 (직접 값 삽입)
# 예: INSERT INTO ztable VALUES ('field1', 'field2', sy-uname).
INSERT_VALUES_PATTERN = re.compile(
    r"^\s*INSERT\s+(?:INTO\s+)?(?P<table>[\w\d_]+)\s+VALUES\s+(?P<values>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# INSERT table VALUES structure 패턴 (구조체 값으로 삽입)
# 예: INSERT ztable VALUES ls_structure.
INSERT_VALUES_STRUCTURE_PATTERN = re.compile(
    r"^\s*INSERT\s+(?P<table>[\w\d_]+)\s+VALUES\s+(?P<structure>[\w\d_\-\>\[\]]+)\s*\.",
    re.IGNORECASE,
)

# 동적 테이블명 INSERT 패턴
# 예: INSERT (lv_table_name) FROM ls_structure.
INSERT_DYNAMIC_TABLE_PATTERN = re.compile(
    r"^\s*INSERT\s+\((?P<table_var>[\w\d_]+)\)\s+FROM\s+(?P<source>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# UPDATE table FROM TABLE 패턴 (내부 테이블에서 일괄 업데이트)
# 예: UPDATE ztable FROM TABLE lt_data.
UPDATE_FROM_TABLE_PATTERN = re.compile(
    r"^\s*UPDATE\s+(?P<table>[\w\d_]+)\s+FROM\s+TABLE\s+(?P<source_table>[\w\d_\[\]]+)\s*\.",
    re.IGNORECASE,
)

# 동적 테이블명 UPDATE 패턴
# 예: UPDATE (lv_table_name) SET field = value.
UPDATE_DYNAMIC_TABLE_PATTERN = re.compile(
    r"^\s*UPDATE\s+\((?P<table_var>[\w\d_]+)\)\s+SET\s+(?P<assignments>.*?)\s*(?:WHERE\s+.*?)?\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# MODIFY table FROM TABLE 패턴 (내부 테이블에서 일괄 수정)
# 예: MODIFY ztable FROM TABLE lt_data.
MODIFY_FROM_TABLE_PATTERN = re.compile(
    r"^\s*MODIFY\s+(?P<table>[\w\d_]+)\s+FROM\s+TABLE\s+(?P<source_table>[\w\d_\[\]]+)\s*\.",
    re.IGNORECASE,
)

# 단독 MODIFY 패턴 (workarea 사용)
# 예: MODIFY ztable. (현재 workarea 기준)
MODIFY_WORKAREA_PATTERN = re.compile(
    r"^\s*MODIFY\s+(?P<table>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# 동적 테이블명 MODIFY 패턴
# 예: MODIFY (lv_table_name) FROM ls_structure.
MODIFY_DYNAMIC_TABLE_PATTERN = re.compile(
    r"^\s*MODIFY\s+\((?P<table_var>[\w\d_]+)\)\s+FROM\s+(?P<source>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# DELETE FROM table WITH KEY 패턴
# 예: DELETE FROM ztable WHERE field = sy-uname.
DELETE_FROM_TABLE_PATTERN = re.compile(
    r"^\s*DELETE\s+FROM\s+(?P<table>[\w\d_]+)\s+WHERE\s+(?P<conditions>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# 동적 테이블명 DELETE 패턴
# 예: DELETE (lv_table_name) WHERE field = value.
DELETE_DYNAMIC_TABLE_PATTERN = re.compile(
    r"^\s*DELETE\s+\((?P<table_var>[\w\d_]+)\)\s+WHERE\s+(?P<conditions>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# ===== UPDATE 후 검증 패턴들 =====

# UPDATE 후 조건부 검증 패턴
# 예: UPDATE ... 다음에 오는 IF sy-uname = 'value'.
POST_UPDATE_IF_VALIDATION_PATTERN = re.compile(
    r"^\s*IF\s+.*?sy-uname.*?",
    re.IGNORECASE | re.DOTALL,
)

# UPDATE 후 CHECK 검증 패턴
# 예: UPDATE ... 다음에 오는 CHECK sy-uname = 'value'.
POST_UPDATE_CHECK_VALIDATION_PATTERN = re.compile(
    r"^\s*CHECK\s+.*?sy-uname.*?",
    re.IGNORECASE | re.DOTALL,
)

# UPDATE 후 ASSERT 검증 패턴
# 예: UPDATE ... 다음에 오는 ASSERT sy-uname = 'value'.
POST_UPDATE_ASSERT_VALIDATION_PATTERN = re.compile(
    r"^\s*ASSERT\s+.*?sy-uname.*?",
    re.IGNORECASE | re.DOTALL,
)

# ===== 스코프 경계 패턴들 (추적 종료 조건) =====

# PERFORM 호출 패턴 (다른 서브루틴으로의 전환)
# 예: PERFORM subroutine_name USING param1 param2.
PERFORM_CALL_PATTERN = re.compile(
    r"^\s*PERFORM\s+(?P<subroutine>[\w\d_]+)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# INCLUDE 구문 패턴 (다른 파일 포함)
# 예: INCLUDE zprog_include.
INCLUDE_PATTERN = re.compile(
    r"^\s*INCLUDE\s+(?P<include_name>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# FORM 정의 시작 패턴 (서브루틴 정의 시작)
# 예: FORM subroutine_name USING param1 TYPE type1.
FORM_DEFINITION_PATTERN = re.compile(
    r"^\s*FORM\s+(?P<form_name>[\w\d_]+)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# ENDFORM 패턴 (서브루틴 정의 끝)
ENDFORM_PATTERN = re.compile(
    r"^\s*ENDFORM\s*\.",
    re.IGNORECASE,
)

# FUNCTION 정의 시작 패턴 (함수 모듈 정의)
# 예: FUNCTION z_function_name.
FUNCTION_DEFINITION_PATTERN = re.compile(
    r"^\s*FUNCTION\s+(?P<function_name>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# ENDFUNCTION 패턴 (함수 모듈 정의 끝)
ENDFUNCTION_PATTERN = re.compile(
    r"^\s*ENDFUNCTION\s*\.",
    re.IGNORECASE,
)

# METHOD 정의 시작 패턴 (메소드 정의)
# 예: METHOD method_name.
METHOD_DEFINITION_PATTERN = re.compile(
    r"^\s*METHOD\s+(?P<method_name>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# ENDMETHOD 패턴 (메소드 정의 끝)
ENDMETHOD_PATTERN = re.compile(
    r"^\s*ENDMETHOD\s*\.",
    re.IGNORECASE,
)

# CLASS 정의 시작 패턴
# 예: CLASS cl_class_name DEFINITION.
CLASS_DEFINITION_PATTERN = re.compile(
    r"^\s*CLASS\s+(?P<class_name>[\w\d_]+)\s+(?:DEFINITION|IMPLEMENTATION)\s*\.",
    re.IGNORECASE,
)

# ENDCLASS 패턴
ENDCLASS_PATTERN = re.compile(
    r"^\s*ENDCLASS\s*\.",
    re.IGNORECASE,
)

# CALL FUNCTION 확장 패턴 (스코프 경계로도 사용)
# 예: CALL FUNCTION 'FUNCTION_NAME' DESTINATION 'RFC_DEST'
CALL_FUNCTION_SCOPE_PATTERN = re.compile(
    r"^\s*CALL\s+FUNCTION\s+(?P<function_name>[\w\d'_]+)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# SUBMIT 프로그램 실행 패턴 (다른 프로그램으로의 전환)
# 예: SUBMIT program_name WITH selection_screen.
SUBMIT_PATTERN = re.compile(
    r"^\s*SUBMIT\s+(?P<program_name>[\w\d_]+)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# ===== 동적 호출 패턴들 (추가 스코프 경계) =====

# 동적 PERFORM 호출 패턴 (변수명 사용)
# 예: PERFORM (lv_subroutine) USING param1.
DYNAMIC_PERFORM_PATTERN = re.compile(
    r"^\s*PERFORM\s+\(\s*(?P<subroutine_var>[\w\d_]+)\s*\)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# 동적 CALL FUNCTION 패턴 (변수명 사용)
# 예: CALL FUNCTION lv_function_name EXPORTING...
DYNAMIC_CALL_FUNCTION_PATTERN = re.compile(
    r"^\s*CALL\s+FUNCTION\s+(?P<function_var>[\w\d_]+)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# 동적 METHOD 호출 패턴 (변수명 사용)
# 예: CALL METHOD (lv_object)->(lv_method)
DYNAMIC_CALL_METHOD_PATTERN = re.compile(
    r"^\s*CALL\s+METHOD\s+(?P<object_ref>[\w\d_\(\)\-\>]+)\s*(?P<params>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# 객체 메소드 호출 패턴 (스코프 경계로 간주)
# 예: lo_object->method_name( param1 = value1 ).
OBJECT_METHOD_CALL_PATTERN = re.compile(
    r"^\s*(?P<object>[\w\d_]+)\-\>(?P<method>[\w\d_]+)\s*\(\s*(?P<params>.*?)\s*\)\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# TRY-CATCH 구문 시작 (새로운 스코프)
# 예: TRY.
TRY_BLOCK_PATTERN = re.compile(
    r"^\s*TRY\s*\.",
    re.IGNORECASE,
)

# CATCH 구문 (스코프 전환)
# 예: CATCH cx_exception.
CATCH_BLOCK_PATTERN = re.compile(
    r"^\s*CATCH\s+(?P<exception_class>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# ENDTRY 구문
ENDTRY_PATTERN = re.compile(
    r"^\s*ENDTRY\s*\.",
    re.IGNORECASE,
)
