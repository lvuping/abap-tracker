import re
import os
import csv
from datetime import datetime
from typing import List, Dict, Tuple


class ABAPSourceAnalyzer:
    def __init__(self, target_domains: List[str]):
        """
        ABAP 소스코드 분석기 초기화

        Args:
            target_domains: 찾고자 하는 도메인 리스트 (예: ['sy-uname', 'sy-datum'])
        """
        self.target_domains = [domain.lower() for domain in target_domains]

        # PARAMETERS 패턴 (대소문자 구분 없음, 콜론과 쉼표로 구분된 여러 줄 지원)
        # 예: PARAMETERS: p_user TYPE sy-uname, p_date FOR sy-datum
        #     PARAMETERS p_user TYPE sy-uname,
        #                p_date FOR sy-datum.
        self.parameters_pattern = re.compile(
            r"^\s*parameters\s*[:]?", re.IGNORECASE | re.MULTILINE
        )

        # SELECT-OPTIONS 패턴 (대소문자 구분 없음, 콜론과 쉼표로 구분된 여러 줄 지원)
        # 예: SELECT-OPTIONS: s_uname FOR sy-uname, s_date FOR sy-datum
        #     SELECT-OPTIONS s_uname FOR sy-uname,
        #                    s_date FOR sy-datum.
        self.select_options_pattern = re.compile(
            r"^\s*select-options\s*[:]?", re.IGNORECASE | re.MULTILINE
        )

    def analyze_file(self, file_path: str) -> List[Dict]:
        """
        단일 파일을 분석하여 매칭되는 라인을 찾음.
        'parameters' 또는 'select-options' 키워드 발견 시, 위아래 20줄을 포함한 컨텍스트에서
        타겟 도메인을 검색함.

        Args:
            file_path: 분석할 ABAP 파일 경로

        Returns:
            매칭된 결과 리스트 (파일명, 라인번호, 라인내용, 매칭된 도메인 포함)
        """
        results = []
        processed_lines = set()  # 중복 처리를 방지하기 위해 이미 처리된 라인 번호를 저장

        try:
            with open(file_path, "r", encoding="utf-8") as file:
                lines = file.readlines()

            for i, line in enumerate(lines):
                if i in processed_lines:
                    continue

                # 'parameters' 또는 'select-options' 키워드 검색
                is_param = self.parameters_pattern.search(line)
                is_select = self.select_options_pattern.search(line)

                if is_param or is_select:
                    # 컨텍스트 윈도우 설정 (위/아래 20줄)
                    start_line = max(0, i - 20)
                    end_line = min(len(lines), i + 21)
                    
                    context_lines = lines[start_line:end_line]
                    context_block = "".join(context_lines)

                    # 컨텍스트 블록 내에서 도메인 검색
                    matched_domains = self._extract_domains_from_statement(context_block)

                    if matched_domains:
                        statement_type = "PARAMETERS" if is_param else "SELECT-OPTIONS"
                        
                        # 결과 추가
                        results.append({
                            "file_name": os.path.basename(file_path),
                            "file_path": file_path,
                            "line_number": i + 1,  # 0-based to 1-based
                            "line_content": context_block.strip(),
                            "statement_type": statement_type,
                            "matched_domains": matched_domains,
                        })
                        
                        # 이 컨텍스트 블록에 포함된 라인들은 더 이상 처리하지 않음
                        for j in range(start_line, end_line):
                            processed_lines.add(j)

        except Exception as e:
            print(f"파일 처리 중 오류 발생 ({file_path}): {e}")

        return results

    def _extract_domains_from_statement(self, full_statement: str) -> List[str]:
        """
        전체 구문에서 타겟 도메인이 포함되어 있는지 확인 (단어 단위 검색)

        Args:
            full_statement: 전체 PARAMETERS 또는 SELECT-OPTIONS 구문

        Returns:
            매칭된 도메인 리스트
        """
        matched_domains = []
        statement_lower = full_statement.lower()
        valid_identifier_chars = 'abcdefghijklmnopqrstuvwxyz0123456789_-'

        for domain in self.target_domains:
            domain_to_find = domain.lower()
            start_index = 0
            while True:
                index = statement_lower.find(domain_to_find, start_index)
                if index == -1:
                    break

                # 단어 경계 확인 (시작)
                if index > 0:
                    char_before = statement_lower[index - 1]
                    if char_before in valid_identifier_chars:
                        start_index = index + 1
                        continue
                
                # 단어 경계 확인 (끝)
                end_pos = index + len(domain_to_find)
                if end_pos < len(statement_lower):
                    char_after = statement_lower[end_pos]
                    if char_after in valid_identifier_chars:
                        start_index = index + 1
                        continue

                # 전체 단어 일치
                matched_domains.append(domain)
                break  # 현재 구문에서 도메인을 찾았으므로 다음 도메인으로 이동

        return list(set(matched_domains))

    

    def analyze_directory(
        self, directory_path: str, file_extensions: List[str] = None
    ) -> List[Dict]:
        """
        디렉토리 내의 모든 ABAP 파일을 분석
        각 파일에서 타겟 도메인을 찾으면 즉시 다음 파일로 이동

        Args:
            directory_path: 분석할 디렉토리 경로
            file_extensions: 분석할 파일 확장자 리스트 (기본값: ['.abap', '.txt'])

        Returns:
            모든 매칭된 결과 리스트
        """
        if file_extensions is None:
            file_extensions = [".abap", ".txt", ".inc"]

        all_results = []
        processed_files = 0
        found_files = 0

        for root, dirs, files in os.walk(directory_path):
            for file in files:
                if any(file.lower().endswith(ext.lower()) for ext in file_extensions):
                    file_path = os.path.join(root, file)
                    processed_files += 1

                    print(f"   📁 처리 중: {file} ({processed_files}번째)")

                    # 파일 분석 실행
                    results = self.analyze_file(file_path)

                    if results:
                        all_results.extend(results)
                        found_files += 1
                        print(f"   🎯 {file}에서 {len(results)}개 결과 발견")
                    else:
                        print(f"   ⚪ {file}에서 타겟 도메인 없음")

        print(
            f"\n📊 처리 완료: {processed_files}개 파일 중 {found_files}개에서 타겟 도메인 발견"
        )
        return all_results

    def print_results(self, results: List[Dict]):
        """
        분석 결과를 보기 좋게 출력

        Args:
            results: analyze_file 또는 analyze_directory의 결과
        """
        if not results:
            print("매칭되는 결과가 없습니다.")
            return

        print(f"\n총 {len(results)}개의 매칭 결과를 찾았습니다:\n")
        print("-" * 80)

        for i, result in enumerate(results, 1):
            print(f"{i}. 파일: {result['file_name']}")
            print(f"   라인 번호: {result['line_number']}")
            print(f"   구문 타입: {result['statement_type']}")
            print(f"   매칭된 도메인: {', '.join(result['matched_domains'])}")
            print(f"   라인 내용: {result['line_content']}")
            print("-" * 80)

    def export_to_csv(self, results: List[Dict], output_file: str = None) -> bool:
        """
        분석 결과를 CSV 파일로 출력

        Args:
            results: 분석 결과 리스트
            output_file: 출력 파일 경로 (기본값: output/abap_parameters_analysis.csv)

        Returns:
            성공 여부
        """
        if not results:
            print("❌ 출력할 결과가 없습니다.")
            return False

        # 출력 디렉토리 확인
        output_dir = "output"
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        # 기본 출력 파일명
        if output_file is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_file = f"{output_dir}/abap_parameters_analysis_{timestamp}.csv"

        try:
            # CSV 컬럼 정의
            fieldnames = [
                "ID",
                "File_Name",
                "File_Path",
                "Line_Number",
                "Statement_Type",
                "Matched_Domains",
                "Line_Content",
                "Analysis_Date",
            ]

            with open(output_file, "w", newline="", encoding="utf-8-sig") as csvfile:
                writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

                # 헤더 작성
                writer.writeheader()

                # 데이터 작성
                for idx, result in enumerate(results, 1):
                    # File_Name 정리: "input/" 경로와 ".abap" 확장자 제거
                    clean_filename = result["file_name"]
                    if clean_filename.startswith("input/"):
                        clean_filename = clean_filename[6:]  # "input/" 제거
                    if clean_filename.endswith(".abap"):
                        clean_filename = clean_filename[:-5]  # ".abap" 제거
                    elif clean_filename.endswith(".txt"):
                        clean_filename = clean_filename[:-4]  # ".txt" 제거
                    elif clean_filename.endswith(".inc"):
                        clean_filename = clean_filename[:-4]  # ".inc" 제거

                    row = {
                        "ID": idx,
                        "File_Name": clean_filename,
                        "File_Path": result["file_path"],
                        "Line_Number": result["line_number"],
                        "Statement_Type": result["statement_type"],
                        "Matched_Domains": ", ".join(result["matched_domains"]),
                        "Line_Content": result["line_content"],
                        "Analysis_Date": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                    }
                    writer.writerow(row)

            print(f"✅ CSV 출력 완료: {output_file}")
            print(f"📊 총 {len(results)}개 결과 저장")
            return True

        except Exception as e:
            print(f"❌ CSV 출력 오류: {e}")
            return False


def analyze_and_export_to_csv(
    target_domains: List[str], input_directory: str = "input", output_file: str = None
):
    """
    입력 디렉토리의 ABAP 파일들을 분석하고 CSV로 출력하는 메인 함수

    Args:
        target_domains: 찾고자 하는 도메인 리스트
        input_directory: 분석할 입력 디렉토리
        output_file: 출력 CSV 파일 경로
    """
    print("🔍 ABAP PARAMETERS/SELECT-OPTIONS 분석 및 CSV 출력 시작")
    print("=" * 80)
    print(f"🎯 타겟 도메인: {', '.join(target_domains)}")
    print(f"📁 입력 디렉토리: {input_directory}")
    print()

    # 분석기 초기화
    analyzer = ABAPSourceAnalyzer(target_domains)

    # 입력 디렉토리 존재 확인
    if not os.path.exists(input_directory):
        print(f"❌ 오류: 입력 디렉토리 {input_directory}를 찾을 수 없습니다.")
        return False

    # 디렉토리 분석 실행
    print("🔍 ABAP 파일 분석 중...")
    results = analyzer.analyze_directory(input_directory)

    if not results:
        print("❌ 매칭되는 결과가 없습니다.")
        print(
            "💡 이는 정상적인 결과입니다. 타겟 도메인이 포함된 파일이 없다는 의미입니다."
        )
        return True  # 결과가 없는 것도 성공으로 처리

    # 결과 출력
    print(f"📊 분석 완료: {len(results)}개 결과 발견")
    analyzer.print_results(results)

    # CSV 출력
    print("\n💾 CSV 출력 중...")
    success = analyzer.export_to_csv(results, output_file)

    if success:
        print()
        print("🎉 분석 및 CSV 출력이 성공적으로 완료되었습니다!")
        print()
        print("📋 CSV 파일 구조:")
        print("   A: ID (1,2,3,...)")
        print("   B: File_Name (소스코드 파일이름)")
        print("   C: File_Path (전체 파일 경로)")
        print("   D: Line_Number (발견된 라인번호)")
        print("   E: Statement_Type (PARAMETERS/SELECT-OPTIONS)")
        print("   F: Matched_Domains (매칭된 도메인들)")
        print("   G: Line_Content (해당 라인 내용)")
        print("   H: Analysis_Date (분석 실행 시간)")
        print()
        print("💡 Excel에서 열어서 확인하실 수 있습니다.")
    else:
        print("❌ CSV 출력 중 오류가 발생했습니다.")

    return success


if __name__ == "__main__":
    print("🧪 ABAP 분석기 단순 검색 로직 테스트")
    print("=" * 80)

    # --- 테스트 케이스 정의 ---
    test_cases = {
        "simple_find": {
            "content": """
* 다양한 위치에 타겟 도메인이 있는 경우
PARAMETERS p_user TYPE sy-uname.
SELECT-OPTIONS s_date FOR sy-datum.
PARAMETERS p_lang LIKE sy-langu.
SELECT-OPTIONS s_id FOR ztable-id DEFAULT sy-uname.
            """,
            "targets": ["sy-uname", "sy-datum", "sy-langu"],
            "expected_found": True,
            "expected_domains": ["sy-uname", "sy-datum", "sy-langu"],
        },
        "comment_find": {
            "content": """
* 주석 안에 타겟 도메인이 있는 경우
PARAMETERS p_field TYPE string. " sy-uname을 참조하는 필드
SELECT-OPTIONS s_field FOR ztable-field. " 여기도 sy-datum 관련
            """,
            "targets": ["sy-uname", "sy-datum"],
            "expected_found": True,
            "expected_domains": ["sy-uname", "sy-datum"],
        },
        "substring_negative": {
            "content": """
* 타겟 도메인이 다른 단어의 일부인 경우 (검색되면 안됨)
PARAMETERS p_user_table TYPE zsy-uname_table.
SELECT-OPTIONS s_date_range FOR zsy-datum_range.
            """,
            "targets": ["sy-uname", "sy-datum"],
            "expected_found": False,
        },
        "mixed_case": {
            "content": """
* 대소문자가 섞여있는 경우
PARAMETERS p_user TYPE SY-UNAME.
SELECT-OPTIONS s_date FOR Sy-Datum.
            """,
            "targets": ["sy-uname", "sy-datum"],
            "expected_found": True,
            "expected_domains": ["sy-uname", "sy-datum"],
        },
        "no_target": {
            "content": """
* 타겟 도메인이 없는 경우
PARAMETERS p_field1 TYPE c.
SELECT-OPTIONS s_field2 FOR ztable-field.
            """,
            "targets": ["sy-uname", "sy-datum"],
            "expected_found": False,
        },
        "multiline_complex": {
            "content": """
* 여러 줄 복합 구문
SELECT-OPTIONS: s_user FOR ztable-user, " 기본값 sy-uname 고려
                s_date FOR ztable-date, " 기본값 sy-datum 고려
                s_langu FOR ztable-lang DEFAULT sy-langu.
            """,
            "targets": ["sy-uname", "sy-datum", "sy-langu"],
            "expected_found": True,
            "expected_domains": ["sy-uname", "sy-datum", "sy-langu"],
        },
        "long_code_find": {
            "content": '''
* 아주 긴 ABAP 코드 중간에 키워드가 있는 경우
REPORT z_long_report.

INITIALIZATION.
  DATA: gv_flag TYPE c.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM display_data.

*----------------------------------------------------------------------*
*       FORM get_data
*----------------------------------------------------------------------*
FORM get_data.
  DATA: lt_mara TYPE TABLE OF mara,
        ls_mara TYPE mara.

  SELECT * FROM mara
    INTO TABLE lt_mara
    UP TO 100 ROWS.

  LOOP AT lt_mara INTO ls_mara.
    " Some processing logic here
  ENDLOOP.
  
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...

  PARAMETERS p_user TYPE sy-uname.

  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
  " ... 많은 라인의 코드가 여기에 있다고 가정 ...
ENDFORM.
            ''',
            "targets": ["sy-uname"],
            "expected_found": True,
            "expected_domains": ["sy-uname"],
        },
    }

    # --- 테스트 실행 ---
    os.makedirs("input", exist_ok=True)
    all_tests_passed = True

    for test_name, config in test_cases.items():
        print(f"\n--- 🧪 테스트 실행: {test_name} ---")
        file_path = f"input/{test_name}.abap"
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(config["content"])
        
        analyzer = ABAPSourceAnalyzer(config["targets"])
        results = analyzer.analyze_file(file_path)

        test_passed = True
        if config["expected_found"]:
            if not results:
                test_passed = False
                print(f"   ❌ 실패: 결과를 찾아야 하지만 찾지 못했습니다.")
            else:
                found_domains = set()
                for res in results:
                    found_domains.update(res["matched_domains"])
                
                expected_domains = set(config["expected_domains"])
                if found_domains != expected_domains:
                    test_passed = False
                    print(f"   ❌ 실패: 찾은 도메인이 예상과 다릅니다.")
                    print(f"     - 예상: {sorted(list(expected_domains))}")
                    print(f"     - 실제: {sorted(list(found_domains))}")
                else:
                    print(f"   ✅ 성공: 예상된 도메인을 모두 찾았습니다: {sorted(list(found_domains))}")
        else: # not expected_found
            if results:
                test_passed = False
                print(f"   ❌ 실패: 결과를 찾지 않아야 하지만 찾았습니다: {results}")
            else:
                print(f"   ✅ 성공: 예상대로 결과를 찾지 않았습니다.")

        if not test_passed:
            all_tests_passed = False

    # --- 최종 결과 요약 ---
    print("\n" + "=" * 80)
    if all_tests_passed:
        print("🎉 모든 테스트가 성공적으로 완료되었습니다!")
    else:
        print("🔥 하나 이상의 테스트가 실패했습니다.")
    print("=" * 80)
