# SAP ABAP SY-UNAME 추적기 사용 가이드

## 🎯 개요

이 도구는 SAP ABAP 코드에서 `SY-UNAME` 시스템 변수가 어떻게 사용되는지 추적하여 보안 분석을 수행합니다. 특히 RFC 함수 호출에서 사용자 정보가 어떻게 전달되는지 파악할 수 있습니다.

## 📋 사전 준비

### 1. 필요한 파일들

```
input/
├── source_code.abap          # 분석할 ABAP 소스 코드
└── sy_uname_locations.csv    # SY-UNAME이 위치한 파일과 라인 번호
```

### 2. sy_uname_locations.csv 형식

```csv
file_path,line_number
input/source_code.abap,90
src/programs/user_auth.abap,45
src/modules/security_check.abap,123
```

## 🚀 단계별 실행 가이드

### Step 1: 소스 코드 준비

1. 분석할 ABAP 파일을 `input/` 폴더에 복사
2. 파일 인코딩이 UTF-8인지 확인

```bash
# 파일 인코딩 확인
file -bi input/source_code.abap

# UTF-8이 아닌 경우 변환
iconv -f CP1252 -t UTF-8 input/source_code.abap > input/source_code_utf8.abap
```

### Step 2: SY-UNAME 위치 식별

ABAP 코드에서 `SY-UNAME`이 사용되는 위치를 찾아 CSV 파일에 기록합니다.

```bash
# 자동 검색 (참고용)
grep -n -i "sy-uname" input/*.abap
```

**예시 결과:**
```
input/source_code.abap:90:  lv_lifnr = sy-uname.
input/source_code.abap:156:  " Store current user from sy-uname
```

### Step 3: 분석 실행

```bash
# 기본 분석 실행
python main.py

# 상세 디버깅 모드
python test_comprehensive.py
```

### Step 4: 결과 확인

분석 결과는 `output/analysis_result.json`에 저장됩니다.

```bash
# 결과 확인
cat output/analysis_result.json | python -m json.tool
```

## 📊 결과 해석

### 성공적인 추적 사례

```json
{
  "source_file": "input/source_code.abap",
  "source_line": 90,
  "result": {
    "status": "Found",
    "type": "RFC",
    "name": "Z_RFC_GET_VENDOR_REMOTE_INFO",
    "parameter": "i_vendor_number",
    "final_variable": "iv_lifnr",
    "path": [
      "Line 90: Assignment 'sy-uname' -> 'lv_lifnr'",
      "Line 119: Structure assignment 'lv_lifnr' -> 'gs_po_list-lifnr'",
      "Line 131: PERFORM 'call_remote_system' USING parameter 3: 'lv_lifnr'",
      "Line 173: RFC parameter 'i_vendor_number' = 'iv_lifnr'"
    ],
    "tainted_variables": ["sy-uname", "lv_lifnr", "gs_po_list-lifnr", "iv_lifnr"]
  }
}
```

**해석:**
- ✅ **추적 성공**: SY-UNAME이 RFC 함수까지 추적됨
- 🎯 **최종 목적지**: `Z_RFC_GET_VENDOR_REMOTE_INFO` RFC 함수의 `i_vendor_number` 파라미터
- 📈 **변수 흐름**: sy-uname → lv_lifnr → gs_po_list-lifnr → iv_lifnr
- 🔍 **보안 관점**: 사용자 ID가 외부 시스템으로 전송됨

### 부분 추적 사례

```json
{
  "source_file": "input/source_code.abap", 
  "source_line": 90,
  "result": {
    "status": "Not Found in Snippet",
    "tainted_variables": ["sy-uname", "lv_lifnr"],
    "path": [
      "Line 90: Assignment 'sy-uname' -> 'lv_lifnr'"
    ]
  }
}
```

**해석:**
- ⚠️ **부분 추적**: 일부만 추적됨
- 📍 **추적 범위**: 200줄 스니펫 내에서 완전한 흐름을 찾지 못함
- 🔍 **추가 조사 필요**: 더 넓은 범위나 다른 파일에서 계속될 가능성

## 🔧 고급 사용법

### 1. 커스텀 패턴 추가

`patterns.py`를 수정하여 새로운 ABAP 패턴을 추가할 수 있습니다.

```python
# 새로운 패턴 예시: METHOD 호출
METHOD_CALL_PATTERN = re.compile(
    r"^\s*(?P<object>[\w\d_]+)->(?P<method>[\w\d_]+)\(\s*(?P<params>.*)\)",
    re.IGNORECASE
)
```

### 2. 분석 범위 조정

`main.py`에서 스니펫 크기를 조정할 수 있습니다.

```python
# 기본값: 위아래 100줄씩 (총 200줄)
start = max(0, line_number - 101)
end = min(len(all_lines), line_number + 100)

# 확장: 위아래 200줄씩 (총 400줄)
start = max(0, line_number - 201)
end = min(len(all_lines), line_number + 200)
```

### 3. 다중 파일 분석

여러 ABAP 파일을 동시에 분석하는 경우:

```csv
file_path,line_number
programs/main.abap,45
includes/user_auth.inc,23
forms/security.abap,67
classes/user_manager.abap,89
```

## 🐛 문제 해결

### 일반적인 문제들

#### 1. "File not found" 오류
```bash
[ERROR] File not found: input/source_code.abap
```
**해결방법**: 파일 경로와 이름을 확인하고 올바른 위치에 파일이 있는지 확인

#### 2. 인코딩 오류
```bash
UnicodeDecodeError: 'utf-8' codec can't decode
```
**해결방법**: 파일을 UTF-8로 변환
```bash
iconv -f CP1252 -t UTF-8 input/source_code.abap > input/source_code_fixed.abap
```

#### 3. 패턴 매칭 실패
**증상**: 변수 추적이 중간에 끊어짐
**해결방법**: 
1. `debug_patterns.py` 실행하여 패턴 확인
2. 필요시 새로운 패턴 추가

#### 4. 메모리 부족
**증상**: 대용량 파일 처리 시 느려짐
**해결방법**: 스니펫 크기를 줄이거나 파일을 분할

### 디버깅 도구

```bash
# 패턴 매칭 테스트
python debug_patterns.py

# 스니펫 상세 분석
python test_snippet.py

# 전체 종합 테스트
python test_comprehensive.py
```

## 📈 성능 최적화

### 1. 대용량 코드베이스 처리

```python
# main.py 수정 예시
def process_large_codebase():
    batch_size = 10  # 한번에 처리할 파일 수
    for batch in chunks(file_list, batch_size):
        process_batch(batch)
```

### 2. 병렬 처리

```python
import multiprocessing as mp

def parallel_analysis():
    with mp.Pool() as pool:
        results = pool.map(analyze_file, file_list)
```

## 📚 추가 자료

- [ABAP 문법 레퍼런스](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm)
- [SAP 보안 가이드라인](https://help.sap.com/docs/SAP_NETWEAVER_AS_ABAP/7ad18510da704e8198a4db0ab8a4a12e/46f42b00bcfc15cfe10000000a11461f.html)
- [정규식 테스트 도구](https://regex101.com/)

## 🤝 지원 및 기여

문제가 발생하거나 개선사항이 있으면 언제든 연락주세요!

---
**마지막 업데이트**: 2024년 1월  
**지원 채널**: GitHub Issues
