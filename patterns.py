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
UPDATE_PATTERN = re.compile(
    r"^\s*UPDATE\s+(?P<table>[\w\d_]+)\s+SET\s+(?P<assignments>.*?)\s*(?:WHERE\s+.*?)?\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# INSERT 문 패턴
INSERT_PATTERN = re.compile(
    r"^\s*INSERT\s+(?:INTO\s+)?(?P<table>[\w\d_]+)\s+(?:VALUES\s+(?P<values>.*?)|FROM\s+(?P<source>[\w\d\-\>\[\]]+))\s*\.",
    re.IGNORECASE | re.DOTALL,
)

# MODIFY 문 패턴
MODIFY_PATTERN = re.compile(
    r"^\s*MODIFY\s+(?P<table>[\w\d_]+)\s+FROM\s+(?P<source>[\w\d\-\>\[\]]+)(?:\s+.*?)?\s*\.",
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
