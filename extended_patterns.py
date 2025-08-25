import re

# 1. s_usnam for sy-uname no intervals no-extension
# 예: SELECT-OPTIONS s_usnam FOR sy-uname NO INTERVALS NO-EXTENSION.
SELECT_OPTIONS_PATTERN = re.compile(
    r"^\s*SELECT-OPTIONS\s+(?P<target>[\w\d_]+)\s+FOR\s+(?P<source>sy-uname)\s+NO-INTERVALS\s+NO-EXTENSION\s*\.",
    re.IGNORECASE,
)

# 2. read table with key ernam = sy-uname
# 예: READ TABLE lt_data WITH KEY ernam = sy-uname.
READ_TABLE_WITH_KEY_PATTERN = re.compile(
    r"^\s*READ\s+TABLE\s+.*?\s+WITH\s+KEY\s+.*?ernam\s*=\s*(?P<source>sy-uname)\b.*",
    re.IGNORECASE | re.DOTALL,
)

# 3. data: l_uname type sy-uname,
# 예: DATA: gv_user TYPE sy-uname.
DATA_TYPE_PATTERN = re.compile(
    r"^\s*DATA\s*:?\s*(?P<target>\w+)\s+TYPE\s+(?P<source>sy-uname)\s*[,.]",
    re.IGNORECASE,
)

# 4. data: l_uname like sy-uname,
# 예: DATA gv_user LIKE sy-uname.
DATA_LIKE_PATTERN = re.compile(
    r"^\s*DATA\s*:?\s*(?P<target>\w+)\s+LIKE\s+(?P<source>sy-uname)\s*[,.]",
    re.IGNORECASE,
)

# 5. ztable-ernam = sy-uname. modify ztable.
# MODIFY ztable. 부분을 위한 패턴
# 참고: ztable-ernam = sy-uname은 기존 ASSIGN_PATTERN으로 감지 가능
MODIFY_TABLE_PATTERN = re.compile(
    r"^\s*MODIFY\s+(?P<table>[\w\d_]+)\s*\.",
    re.IGNORECASE,
)

# 6. move : sy-datum to gt_7100-datum, (다음줄에) sy-uname to gt_7100-ernam.
# 연속적인 MOVE 구문 패턴
CHAIN_MOVE_PATTERN = re.compile(
    r"^\s*MOVE\s*:\s*(?P<moves>.*?)\s*\.",
    re.IGNORECASE | re.DOTALL
)

# 연속 MOVE 내에서 개별 할당을 찾기 위한 패턴
# 쉼표(,)로 구분된 여러 할당을 처리하도록 수정
MOVE_PAIR_PATTERN = re.compile(
    r",?\s*(?P<source>[\w\d\-\>\[\]]+)\s+TO\s+(?P<target>[\w\d\-\>\[\]]+)"
)
