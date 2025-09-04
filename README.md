# 🔍 SAP ABAP SY-UNAME 추적기 (v2.0)

> **SY-UNAME이 실제 데이터베이스 테이블의 어떤 필드에 저장되는지 정확히 추적하는 고도화된 분석 도구**

[![Python](https://img.shields.io/badge/Python-3.7%2B-blue.svg)](https://www.python.org/)
[![ABAP](https://img.shields.io/badge/ABAP-SAP-orange.svg)](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

## 🎯 프로젝트 개요

이 도구는 SAP ABAP 코드에서 `SY-UNAME` 시스템 변수의 흐름을 추적하여, **실제 데이터베이스 테이블과 필드명**을 정확히 감지합니다. 기존 도구들과 달리 구조체 변수(`ls_document-created_by`)가 아닌 **실제 테이블명과 필드명**(`ZDOCUMENTS`, `CREATED_BY`)을 제공합니다.

### 🌟 핵심 기능 (v2.0)

- ✅ **실제 데이터베이스 테이블/필드 감지**: `ZDOCUMENTS.CREATED_BY`
- ✅ **21가지 이상 ABAP 패턴 지원**: 할당, MOVE, RFC, 데이터베이스 작업 등
- ✅ **내부 테이블 추적 강화**: `APPEND`를 통해 내부 테이블로 전파된 후 `INSERT`/`MODIFY`되는 복합 흐름 추적
- ✅ **정확한 스코프 제어**: `PERFORM`, `FORM`, `METHOD` 등 스코프의 시작과 끝(`ENDFORM` 등)을 명확히 인지하여 분석 범위 제어
- ✅ **스코프 외부 사용 추론**: `PERFORM` 호출 시 어떤 오염된 변수가 전달되는지 추측하여 다음 분석 단계를 제시
- ✅ **변수 초기화(`CLEAR`) 반영**: `CLEAR` 구문으로 오염 상태가 제거되는 것을 반영하여 분석 정확도 향상
- ✅ **비즈니스 크리티컬 Sink**: `UPDATE`/`INSERT`/`MODIFY`/`DELETE`/`RFC` 감지

## 🚀 빠른 시작

### 1. 설치 (필요 시)

```bash
git clone <repository>
cd abap-tracker
# 현재 프로젝트는 Python 표준 라이브러리만 사용하므로 별도 설치는 필요 없습니다.
```

### 2. 사용법

#### 2.1. 입력 파일 준비

1.  **ABAP 소스 파일**: 분석할 ABAP 소스 코드(`*.abap`)를 `input/` 디렉토리에 배치합니다.
2.  **SY-UNAME 위치 정보**: `input/sy_uname_locations.csv` 파일을 생성하고, 아래 형식에 맞게 분석할 위치를 지정합니다.

    **`input/sy_uname_locations.csv` 형식:**
    ```csv
    id,file_path,line_number
    1,test_comprehensive_scenario.abap,18
    2,test_extended_scenarios.abap,10
    3,test_extended_scenarios.abap,17
    4,test_extended_scenarios.abap,22
    ```
    - `id`: 각 분석 항목의 고유 식별자
    - `file_path`: `input/` 디렉토리에 있는 ABAP 파일명
    - `line_number`: `SY-UNAME`이 사용된 라인 번호

#### 2.2. 분석 실행

```bash
# 기본 실행 (JSON 결과만 출력)
python main.py

# 상세 정보와 함께 실행 (권장)
python main.py --verbose

# JSON과 CSV 결과 모두 생성
python main.py --format both

# CSV 결과만 생성 (Excel에서 보기 용이)
python main.py --format csv
```

#### 2.3. 결과 확인

-   **콘솔 출력**: 실행 시 터미널에서 분석 결과를 실시간으로 확인할 수 있습니다.
-   **JSON 파일**: `output/analysis_result.json`에 모든 분석 결과의 상세 정보가 저장됩니다.
-   **CSV 파일**: `output/sy_uname_analysis_results.csv`에 요약된 결과가 저장되어 Excel 등에서 쉽게 활용할 수 있습니다.

### 3. 결과 해석

**1. Sink 발견 (`Found`)**
```
📍 1/4. test_comprehensive_scenario.abap (라인 18)
   ✅ 데이터베이스: MODIFY FROM tainted table 'lt_data' ZUSER_DATA
```
- `SY-UNAME`의 흐름이 최종적으로 DB 테이블이나 RFC 호출 등 의미 있는 종착점(Sink)에 도달했음을 의미합니다.

**2. 스코프 경계 도달 (`Scope Boundary Reached`)**
```
📍 3/4. test_extended_scenarios.abap (라인 17)
   ⛔ 스코프 경계: PERFORM PROCESS_USER_DATA (라인 18)
      💡 추측: 오염된 변수 "gv_user_perform"이(가) PERFORM 'PROCESS_USER_DATA' 내부에서 사용될 것으로 추측됩니다.
```
- `PERFORM`, `ENDFORM` 등 현재 분석 범위를 벗어나는 구문을 만나 추적을 중단했음을 의미합니다.
- `💡 추측:` 메시지를 통해 어떤 변수가 다른 스코프로 전달되었는지 힌트를 얻을 수 있습니다.

**3. Sink 미발견 (`Not Found`)**
```
   ⚠️ SY-UNAME 추적됨 but Z/Y 테이블/RFC Sink 미발견
      • 분석된 문장: 5개
      • 전파된 변수: 1개
```
- 스코프 내에서 변수 흐름을 추적했지만, DB 조작이나 RFC 호출과 같은 최종 Sink를 찾지 못했음을 의미합니다.

## 🏗️ 프로젝트 구조

```
abap-tracker/
├── 📄 main.py                     # 메인 실행 엔진
├── 🧠 analyzer.py                 # 핵심 분석 엔진
├── 🎯 patterns.py                 # ABAP 구문 정규식 패턴
├── 📝 todolist.md                 # 개발 계획 및 과제
├── 📁 input/
│   ├── test_comprehensive_scenario.abap # 종합 테스트 시나리오
│   ├── test_extended_scenarios.abap   # 확장 테스트 시나리오
│   └── sy_uname_locations.csv     # SY-UNAME 위치 정보
└── 📁 output/
    ├── analysis_result.json       # JSON 분석 결과
    └── sy_uname_analysis_results.csv # CSV 분석 결과
```

## 🛠️ 고급 기능 및 추적 로직

### 내부 테이블 추적
`APPEND` 구문을 통해 오염된 구조체(Work Area)가 내부 테이블(Internal Table)로 전달되는 흐름을 추적합니다.
```abap
" 1. 구조체 필드 오염
ls_data-created_by = sy-uname.

" 2. 내부 테이블로 오염 전파
APPEND ls_data TO lt_data.

" 3. 오염된 내부 테이블을 사용한 DB 조작 감지 (Sink)
MODIFY zuser_data FROM TABLE lt_data.
```
→ **결과**: `MODIFY ZUSER_DATA` 감지 성공

### 변수 초기화(`CLEAR`) 반영
`CLEAR` 구문으로 변수가 초기화되면, 해당 변수는 '오염된' 상태에서 제거되어 더 이상 추적되지 않습니다.
```abap
" 1. 변수 오염
lv_temp_user = sy-uname.

" 2. 변수 초기화 (오염 제거)
CLEAR lv_temp_user.

" 3. 초기화된 변수를 사용한 할당 (추적되지 않음)
ls_data-created_by = lv_temp_user.
```
→ **결과**: `ls_data-created_by`는 오염되지 않은 것으로 판단하여 정확도 향상

---

> **"이제 SY-UNAME이 실제로 어떤 테이블의 어떤 필드에 저장되는지 더 정확하고 넓은 범위에서 추적할 수 있습니다!"** 🎯
