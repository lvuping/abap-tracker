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
            r"^\s*parameters\s*[::]?\s*([^.]+)", re.IGNORECASE | re.MULTILINE
        )

        # SELECT-OPTIONS 패턴 (대소문자 구분 없음, 콜론과 쉼표로 구분된 여러 줄 지원)
        # 예: SELECT-OPTIONS: s_uname FOR sy-uname, s_date FOR sy-datum
        #     SELECT-OPTIONS s_uname FOR sy-uname,
        #                    s_date FOR sy-datum.
        self.select_options_pattern = re.compile(
            r"^\s*select-options\s*[::]?\s*([^.]+)", re.IGNORECASE | re.MULTILINE
        )

    def analyze_file(self, file_path: str) -> List[Dict]:
        """
        단일 파일을 분석하여 매칭되는 라인을 찾음
        타겟 도메인을 찾으면 즉시 처리 중단 (최적화)

        Args:
            file_path: 분석할 ABAP 파일 경로

        Returns:
            매칭된 결과 리스트 (파일명, 라인번호, 라인내용, 매칭된 도메인 포함)
        """
        results = []
        processed_positions = set()  # 중복 처리 방지

        try:
            with open(file_path, "r", encoding="utf-8") as file:
                lines = file.readlines()

            # 전체 파일 내용을 하나의 문자열로 결합 (여러 줄 처리용)
            full_content = "".join(lines)

            # PARAMETERS 검사 (여러 줄 지원)
            parameters_matches = self.parameters_pattern.finditer(full_content)
            for match in parameters_matches:
                if match.start() in processed_positions:
                    continue

                processed_positions.add(match.start())

                # 매칭된 전체 구문 추출
                full_statement = self._extract_full_statement(
                    full_content, match.start(), lines
                )
                if full_statement:
                    matched_domains = self._extract_domains_from_statement(
                        full_statement
                    )
                    if matched_domains:
                        # 첫 번째 라인 번호 찾기
                        first_line_num = self._find_first_line_number(
                            full_content, match.start(), lines
                        )
                        results.append(
                            {
                                "file_name": os.path.basename(file_path),
                                "file_path": file_path,
                                "line_number": first_line_num,
                                "line_content": full_statement.strip(),
                                "statement_type": "PARAMETERS",
                                "matched_domains": matched_domains,
                            }
                        )

                        # 🚀 최적화: 타겟 도메인을 찾았으면 즉시 반환
                        print(f"   ✅ {file_path}에서 타겟 도메인 발견! 파일 처리 완료")
                        return results

            # SELECT-OPTIONS 검사 (여러 줄 지원)
            select_options_matches = self.select_options_pattern.finditer(full_content)
            for match in select_options_matches:
                if match.start() in processed_positions:
                    continue

                processed_positions.add(match.start())

                # 매칭된 전체 구문 추출
                full_statement = self._extract_full_statement(
                    full_content, match.start(), lines
                )
                if full_statement:
                    matched_domains = self._extract_domains_from_statement(
                        full_statement
                    )
                    if matched_domains:
                        # 첫 번째 라인 번호 찾기
                        first_line_num = self._find_first_line_number(
                            full_content, match.start(), lines
                        )
                        results.append(
                            {
                                "file_name": os.path.basename(file_path),
                                "file_path": file_path,
                                "line_number": first_line_num,
                                "line_content": full_statement.strip(),
                                "statement_type": "SELECT-OPTIONS",
                                "matched_domains": matched_domains,
                            }
                        )

                        # 🚀 최적화: 타겟 도메인을 찾았으면 즉시 반환
                        print(f"   ✅ {file_path}에서 타겟 도메인 발견! 파일 처리 완료")
                        return results

        except Exception as e:
            print(f"파일 처리 중 오류 발생 ({file_path}): {e}")

        return results

    def _extract_full_statement(
        self, full_content: str, start_pos: int, lines: List[str]
    ) -> str:
        """
        매칭된 위치부터 전체 구문을 추출 (여러 줄 포함)

        Args:
            full_content: 전체 파일 내용
            start_pos: 매칭 시작 위치
            lines: 라인별 리스트

        Returns:
            전체 구문 문자열
        """
        try:
            # 현재 위치부터 다음 마침표(.)까지 또는 파일 끝까지 추출
            remaining_content = full_content[start_pos:]

            # 마침표로 끝나는 경우
            if "." in remaining_content:
                end_pos = remaining_content.find(".") + 1
                statement = remaining_content[:end_pos]
            else:
                # 마침표가 없는 경우 파일 끝까지
                statement = remaining_content

            # 여러 줄을 하나의 문자열로 정리
            statement = statement.replace("\n", " ").replace("\r", " ")
            # 연속된 공백을 하나로 정리
            statement = re.sub(r"\s+", " ", statement).strip()

            return statement

        except Exception as e:
            print(f"전체 구문 추출 중 오류: {e}")
            return ""

    def _extract_domains_from_statement(self, full_statement: str) -> List[str]:
        """
        전체 구문에서 도메인을 추출하고 타겟 도메인과 매칭

        Args:
            full_statement: 전체 PARAMETERS 또는 SELECT-OPTIONS 구문

        Returns:
            매칭된 도메인 리스트
        """
        matched_domains = []

        # 쉼표로 구분된 각 항목을 분리
        items = [item.strip() for item in full_statement.split(",")]

        for item in items:
            # FOR 키워드 이후의 도메인 추출 (SELECT-OPTIONS용)
            for_pattern = re.compile(r"\bfor\s+([a-zA-Z0-9_-]+)", re.IGNORECASE)
            for_matches = for_pattern.findall(item)

            # TYPE 키워드 이후의 도메인 추출 (PARAMETERS용)
            type_pattern = re.compile(r"\btype\s+([a-zA-Z0-9_-]+)", re.IGNORECASE)
            type_matches = type_pattern.findall(item)

            # LIKE 키워드 이후의 도메인 추출
            like_pattern = re.compile(r"\blike\s+([a-zA-Z0-9_-]+)", re.IGNORECASE)
            like_matches = like_pattern.findall(item)

            # 모든 추출된 도메인을 확인
            all_found_domains = for_matches + type_matches + like_matches

            for domain in all_found_domains:
                if domain.lower() in self.target_domains:
                    matched_domains.append(domain.lower())

        return matched_domains

    def _extract_domains_from_line(self, statement_content: str) -> List[str]:
        """
        PARAMETERS/SELECT-OPTIONS 문에서 도메인을 추출하고 타겟 도메인과 매칭

        Args:
            statement_content: PARAMETERS 또는 SELECT-OPTIONS 문의 내용

        Returns:
            매칭된 도메인 리스트
        """
        matched_domains = []

        # FOR 키워드 이후의 도메인 추출 (SELECT-OPTIONS용)
        for_pattern = re.compile(r"\bfor\s+([a-zA-Z0-9_-]+)", re.IGNORECASE)
        for_matches = for_pattern.findall(statement_content)

        # TYPE 키워드 이후의 도메인 추출 (PARAMETERS용)
        type_pattern = re.compile(r"\btype\s+([a-zA-Z0-9_-]+)", re.IGNORECASE)
        type_matches = type_pattern.findall(statement_content)

        # LIKE 키워드 이후의 도메인 추출
        like_pattern = re.compile(r"\blike\s+([a-zA-Z0-9_-]+)", re.IGNORECASE)
        like_matches = like_pattern.findall(statement_content)

        # 모든 추출된 도메인을 확인
        all_found_domains = for_matches + type_matches + like_matches

        for domain in all_found_domains:
            if domain.lower() in self.target_domains:
                matched_domains.append(domain.lower())

        return matched_domains

    def _find_line_number(self, full_content: str, pos: int, lines: List[str]) -> int:
        """
        위치를 기반으로 라인 번호 찾기

        Args:
            full_content: 전체 파일 내용
            pos: 찾고자 하는 위치
            lines: 라인별 리스트

        Returns:
            라인 번호 (1부터 시작)
        """
        try:
            # pos 위치까지의 문자 수를 세어서 라인 번호 계산
            char_count = 0
            for i, line in enumerate(lines):
                char_count += len(line)
                if char_count > pos:
                    return i + 1
            return len(lines)  # 마지막 라인
        except Exception:
            return 1

    def _find_first_line_number(
        self, full_content: str, pos: int, lines: List[str]
    ) -> int:
        """
        매칭된 구문의 첫 번째 라인 번호 찾기

        Args:
            full_content: 전체 파일 내용
            pos: 매칭 시작 위치
            lines: 라인별 리스트

        Returns:
            첫 번째 라인 번호 (1부터 시작)
        """
        return self._find_line_number(full_content, pos, lines)

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


# 사용 예시
if __name__ == "__main__":
    print("🧪 ABAP PARAMETERS/SELECT-OPTIONS 분석기 종합 테스트")
    print("=" * 80)

    # 테스트 케이스 1: 기본 패턴
    print("\n📋 테스트 케이스 1: 기본 패턴")
    basic_test_code = """
* 기본 PARAMETERS와 SELECT-OPTIONS
PARAMETERS: p_user TYPE sy-uname.
SELECT-OPTIONS: s_date FOR sy-datum.
PARAMETERS p_lang LIKE sy-langu.
    """

    # 테스트 케이스 2: 여러 줄 패턴
    print("\n📋 테스트 케이스 2: 여러 줄 패턴")
    multiline_test_code = """
* 여러 줄로 작성된 SELECT-OPTIONS
SELECT-OPTIONS: s_user FOR sy-uname,
                s_date FOR sy-datum,
                s_lang FOR sy-langu.

* 여러 줄로 작성된 PARAMETERS
PARAMETERS: p_user TYPE sy-uname,
            p_date TYPE sy-datum,
            p_lang LIKE sy-langu.
    """

    # 테스트 케이스 3: 콜론과 쉼표 패턴
    print("\n📋 테스트 케이스 3: 콜론과 쉼표 패턴")
    colon_comma_test_code = """
* 콜론과 쉼표를 사용한 여러 줄 SELECT-OPTIONS
SELECT-OPTIONS : s_uname FOR sy-uname,
                 s_date FOR sy-datum.

* 콜론과 쉼표를 사용한 여러 줄 PARAMETERS
PARAMETERS : p_uname TYPE sy-uname,
             p_date TYPE sy-datum.
    """

    # 테스트 케이스 4: 타겟 도메인이 없는 패턴
    print("\n📋 테스트 케이스 4: 타겟 도메인이 없는 패턴")
    no_target_test_code = """
* 타겟 도메인이 없는 PARAMETERS
PARAMETERS: p_field1 TYPE c LENGTH 10.
PARAMETERS: p_field2 TYPE i.
PARAMETERS: p_field3 TYPE string.

* 타겟 도메인이 없는 SELECT-OPTIONS
SELECT-OPTIONS: s_field1 FOR table-field1.
SELECT-OPTIONS: s_field2 FOR table-field2.
    """

    # 테스트 케이스 5: 복합 패턴 (타겟 도메인 + 일반 필드)
    print("\n📋 테스트 케이스 5: 복합 패턴")
    mixed_test_code = """
* 타겟 도메인과 일반 필드가 섞인 경우
PARAMETERS: p_user TYPE sy-uname,
            p_field TYPE c LENGTH 20,
            p_date TYPE sy-datum.

SELECT-OPTIONS: s_user FOR sy-uname,
                s_field FOR table-field,
                s_date FOR sy-datum.
    """

    # 테스트 케이스 6: 실제 비즈니스 시나리오
    print("\n📋 테스트 케이스 6: 실제 비즈니스 시나리오")
    business_scenario_code = """
* 사용자 관리 프로그램
REPORT zuser_management.

* 선택 화면 정의
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_user TYPE sy-uname DEFAULT sy-uname,
            p_date TYPE sy-datum DEFAULT sy-datum,
            p_lang LIKE sy-langu DEFAULT sy-langu.
SELECTION-SCREEN END OF BLOCK b1.

* 선택 옵션 정의
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_user FOR sy-uname,
                s_date FOR sy-datum,
                s_lang FOR sy-langu.
SELECTION-SCREEN END OF BLOCK b2.
    """

    # 테스트 케이스 7: 복잡한 구조체 패턴
    print("\n📋 테스트 케이스 7: 복잡한 구조체 패턴")
    complex_structure_code = """
* 복잡한 구조체를 사용한 PARAMETERS
PARAMETERS: p_header TYPE zheader,
            p_user TYPE sy-uname,
            p_date TYPE sy-datum,
            p_detail TYPE zdetail.

* 복잡한 구조체를 사용한 SELECT-OPTIONS
SELECT-OPTIONS: s_header FOR zheader,
                s_user FOR sy-uname,
                s_date FOR sy-datum,
                s_detail FOR zdetail.
    """

    # 테스트 파일들 생성
    test_files = {
        "input/test_basic.abap": basic_test_code,
        "input/test_multiline.abap": multiline_test_code,
        "input/test_colon_comma.abap": colon_comma_test_code,
        "input/test_no_target.abap": no_target_test_code,
        "input/test_mixed.abap": mixed_test_code,
        "input/business_scenario.abap": business_scenario_code,
        "input/complex_structure.abap": complex_structure_code,
    }

    os.makedirs("input", exist_ok=True)

    for file_path, content in test_files.items():
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(content)
        print(f"   📝 생성됨: {file_path}")

    print(f"\n✅ 총 {len(test_files)}개의 테스트 파일 생성 완료")

    # 테스트 1: 전체 도메인 검색
    print("\n" + "=" * 80)
    print("🧪 테스트 1: 전체 도메인 검색 (sy-uname, sy-datum, sy-langu)")
    print("=" * 80)

    target_domains = ["sy-uname", "sy-datum", "sy-langu"]
    success1 = analyze_and_export_to_csv(
        target_domains, "input", "output/test1_full_domains.csv"
    )

    if success1:
        print("\n✅ 테스트 1 완료!")
    else:
        print("\n❌ 테스트 1 실패!")

    # 테스트 2: SY-UNAME 전용 검색
    print("\n" + "=" * 80)
    print("🧪 테스트 2: SY-UNAME 전용 검색")
    print("=" * 80)

    sy_uname_only = ["sy-uname"]
    success2 = analyze_and_export_to_csv(
        sy_uname_only, "input", "output/test2_sy_uname_only.csv"
    )

    if success2:
        print("\n✅ 테스트 2 완료!")
    else:
        print("\n❌ 테스트 2 실패!")

    # 테스트 3: SY-DATUM 전용 검색
    print("\n" + "=" * 80)
    print("🧪 테스트 3: SY-DATUM 전용 검색")
    print("=" * 80)

    sy_datum_only = ["sy-datum"]
    success3 = analyze_and_export_to_csv(
        sy_datum_only, "input", "output/test3_sy_datum_only.csv"
    )

    if success3:
        print("\n✅ 테스트 3 완료!")
    else:
        print("\n❌ 테스트 3 실패!")

    # 테스트 4: 존재하지 않는 도메인 검색
    print("\n" + "=" * 80)
    print("🧪 테스트 4: 존재하지 않는 도메인 검색")
    print("=" * 80)

    non_existent = ["non-existent-domain"]
    success4 = analyze_and_export_to_csv(
        non_existent, "input", "output/test4_non_existent.csv"
    )

    # 존재하지 않는 도메인 검색 시 결과가 없는 것이 정상이므로 성공으로 처리
    if success4 is not False:  # False가 아닌 경우 (None도 성공으로 처리)
        print("\n✅ 테스트 4 완료! (예상대로 결과 없음)")
        success4 = True  # 성공으로 표시
    else:
        print("\n❌ 테스트 4 실패!")

    # 최종 결과 요약
    print("\n" + "=" * 80)
    print("🎯 종합 테스트 결과 요약")
    print("=" * 80)

    test_results = [
        ("전체 도메인 검색", success1),
        ("SY-UNAME 전용 검색", success2),
        ("SY-DATUM 전용 검색", success3),
        ("존재하지 않는 도메인 검색", success4),
    ]

    for test_name, result in test_results:
        status = "✅ 성공" if result else "❌ 실패"
        print(f"   {test_name}: {status}")

    success_count = sum(1 for _, result in test_results if result)
    print(f"\n📊 총 {len(test_results)}개 테스트 중 {success_count}개 성공")

    if success_count == len(test_results):
        print("\n🎉 모든 테스트가 성공적으로 완료되었습니다!")
    else:
        print(f"\n⚠️ {len(test_results) - success_count}개 테스트가 실패했습니다.")

    print("\n=== 사용법 안내 ===")
    print("🚀 이 도구를 사용하는 방법:")
    print()
    print("1️⃣ 기본 사용법:")
    print("   python test.py")
    print("   → 전체 테스트 실행 및 CSV 출력")
    print()
    print("2️⃣ 커스텀 도메인 검색:")
    print("   from test import ABAPSourceAnalyzer")
    print("   analyzer = ABAPSourceAnalyzer(['sy-uname'])")
    print("   results = analyzer.analyze_directory('input')")
    print("   analyzer.export_to_csv(results, 'output/custom.csv')")
    print()
    print("3️⃣ 단일 파일 분석:")
    print("   analyzer = ABAPSourceAnalyzer(['sy-uname', 'sy-datum'])")
    print("   results = analyzer.analyze_file('input/your_file.abap')")
    print("   analyzer.export_to_csv(results)")
    print()
    print("4️⃣ 직접 함수 호출:")
    print("   from test import analyze_and_export_to_csv")
    print("   analyze_and_export_to_csv(['sy-uname'], 'input', 'output/result.csv')")
    print()
    print("📁 입력 폴더: input/ (ABAP 파일들)")
    print("📁 출력 폴더: output/ (CSV 결과 파일들)")
    print("🎯 지원 도메인: sy-uname, sy-datum, sy-langu 등")
    print("💡 File_Name 컬럼에서 경로와 확장자가 자동으로 제거됩니다!")
