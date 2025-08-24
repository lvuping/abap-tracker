#!/usr/bin/env python3
"""
SY-UNAME 추적 결과를 CSV로 출력하는 스크립트

사용법:
    python export_to_csv.py

결과:
    output/sy_uname_analysis_results.csv 파일 생성
"""

import csv
import os
from datetime import datetime
from analyzer import trace_sy_uname_in_snippet


def analyze_and_export_to_csv():
    """
    sy_uname_locations.csv를 읽어서 분석 후 결과를 CSV로 출력
    """

    print("🔍 SY-UNAME 추적 결과 CSV 출력 시작")
    print("=" * 80)

    # 입력 파일 확인
    locations_file = "input/sy_uname_locations.csv"
    if not os.path.exists(locations_file):
        print(f"❌ 오류: {locations_file} 파일을 찾을 수 없습니다.")
        return False

    # 출력 디렉토리 확인
    output_dir = "output"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # 출력 파일 경로
    output_file = f"{output_dir}/sy_uname_analysis_results.csv"

    results = []

    # sy_uname_locations.csv 읽기
    try:
        with open(locations_file, "r", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            locations = list(reader)
    except Exception as e:
        print(f"❌ 오류: {locations_file} 읽기 실패 - {e}")
        return False

    print(f"📁 입력 파일: {locations_file}")
    print(f"📊 총 분석 대상: {len(locations)}개")
    print()

    # 각 위치별로 분석 실행
    for idx, location in enumerate(locations, 1):
        # ID 값 추출 (없으면 idx 사용)
        id_value = location.get("id", str(idx))

        file_path = location["file_path"]

        # .abap 확장자 자동 추가
        if not file_path.endswith(".abap"):
            file_path = file_path + ".abap"

        try:
            line_number = int(location["line_number"])
        except ValueError:
            print(f"❌ 오류: 라인 번호가 잘못되었습니다 - {location['line_number']}")
            continue

        print(f"📍 {idx}. 분석 중: {file_path} 라인 {line_number}")

        # 파일 존재 확인
        if not os.path.exists(file_path):
            print(f"   ❌ 파일 없음: {file_path}")
            result = create_empty_result(id_value, file_path, line_number, "파일 없음")
            results.append(result)
            continue

        try:
            # 파일 읽기
            with open(file_path, "r", encoding="utf-8") as f:
                all_lines = f.readlines()

            # 분석 범위 추출 (main.py와 동일한 로직)
            start = max(0, line_number - 201)
            end = min(len(all_lines), line_number + 1000)
            snippet = all_lines[start:end]
            relative_start_line = line_number - start - 1

            # SY-UNAME 추적 분석 실행
            analysis_result = trace_sy_uname_in_snippet(snippet, relative_start_line)

            # 결과를 CSV 행으로 변환
            csv_result = convert_analysis_to_csv_row(
                id_value, file_path, line_number, analysis_result
            )
            results.append(csv_result)

            # 간단한 결과 출력
            status = analysis_result.get("status", "Unknown")
            if status == "Found":
                result_type = analysis_result.get("type", "")
                if result_type in [
                    "DATABASE_INSERT_FIELD",
                    "DATABASE_UPDATE_FIELD",
                    "DATABASE_MODIFY_FIELD",
                    "DATABASE_SELECT_WHERE",
                ]:
                    table = analysis_result.get("table", "")
                    fields = ", ".join(analysis_result.get("fields", []))
                    operation = analysis_result.get("operation", "")
                    print(f"   ✅ 데이터베이스: {table}.{fields} ({operation})")
                elif result_type == "RFC":
                    rfc_name = analysis_result.get("name", "")
                    print(f"   ✅ RFC: {rfc_name}")
                elif result_type == "AUDIT_FIELD":
                    structure = analysis_result.get("structure", "")
                    field = analysis_result.get("field", "")
                    print(f"   ✅ 감사 필드: {structure}-{field}")
                else:
                    print(f"   ✅ 발견: {result_type}")
            else:
                print(f"   ⚠️ 미발견: {status}")

        except Exception as e:
            print(f"   ❌ 분석 오류: {e}")
            result = create_empty_result(idx, file_path, line_number, f"분석 오류: {e}")
            results.append(result)

    # CSV 파일로 출력
    try:
        write_results_to_csv(results, output_file)
        print()
        print("=" * 80)
        print(f"✅ CSV 출력 완료: {output_file}")
        print(f"📊 총 {len(results)}개 결과 저장")

        # 결과 요약
        found_count = sum(1 for r in results if r["Status"] == "Found")
        print(f"🎯 성공: {found_count}개, 미발견: {len(results) - found_count}개")

        return True

    except Exception as e:
        print(f"❌ CSV 출력 오류: {e}")
        return False


def convert_analysis_to_csv_row(id_num, file_path, line_number, analysis_result):
    """
    분석 결과를 CSV 행 형태로 변환
    """

    # 기본 정보
    row = {
        "ID": id_num,
        "Source_File": file_path,
        "SY_UNAME_Line": line_number,
        "Status": analysis_result.get("status", "Unknown"),
        "Type": analysis_result.get("type", ""),
        "Final_Table": "",
        "Final_Fields": "",
        "RFC_Name": "",
        "RFC_Parameter": "",
        "Description": analysis_result.get("description", ""),
        "Final_Variable": analysis_result.get("final_variable", ""),
        "Tainted_Variables_Count": len(analysis_result.get("tainted_variables", [])),
        "Trace_Steps": len(analysis_result.get("path", [])),
    }

    # 타입별 상세 정보 추가
    result_type = analysis_result.get("type", "")

    if result_type in [
        "DATABASE_INSERT_FIELD",
        "DATABASE_UPDATE_FIELD",
        "DATABASE_MODIFY_FIELD",
        "DATABASE_SELECT_WHERE",
    ]:
        # 데이터베이스 테이블/필드 정보
        row["Final_Table"] = analysis_result.get("table", "")
        row["Final_Fields"] = ", ".join(analysis_result.get("fields", []))

    elif result_type.startswith("DATABASE_"):
        # 기타 데이터베이스 작업
        row["Final_Table"] = analysis_result.get("table", "")

    elif result_type == "RFC":
        # RFC 호출 정보
        row["RFC_Name"] = analysis_result.get("name", "")
        row["RFC_Parameter"] = analysis_result.get("parameter", "")

    elif result_type == "AUDIT_FIELD":
        # 감사 필드 정보 (구조체-필드로 표시)
        structure = analysis_result.get("structure", "")
        field = analysis_result.get("field", "")
        row["Final_Fields"] = f"{structure}-{field}"

    # 추적 경로를 개별 컬럼으로 추가 (최대 20단계)
    trace_path = analysis_result.get("path", [])
    for i, step in enumerate(trace_path[:20], 1):  # 최대 20단계
        row[f"Trace_Step_{i:02d}"] = step

    return row


def create_empty_result(id_num, file_path, line_number, error_message):
    """
    오류 발생 시 빈 결과 생성
    """
    return {
        "ID": id_num,
        "Source_File": file_path,
        "SY_UNAME_Line": line_number,
        "Status": "Error",
        "Type": "",
        "Final_Table": "",
        "Final_Fields": "",
        "RFC_Name": "",
        "RFC_Parameter": "",
        "Description": error_message,
        "Final_Variable": "",
        "Tainted_Variables_Count": 0,
        "Trace_Steps": 0,
    }


def write_results_to_csv(results, output_file):
    """
    결과를 CSV 파일로 출력
    """

    if not results:
        print("❌ 출력할 결과가 없습니다.")
        return

    # 모든 가능한 컬럼 수집 (동적으로 추적 경로 컬럼 포함)
    all_columns = set()
    for result in results:
        all_columns.update(result.keys())

    # 컬럼 순서 정의
    ordered_columns = [
        "ID",
        "Source_File",
        "SY_UNAME_Line",
        "Status",
        "Type",
        "Final_Table",
        "Final_Fields",
        "RFC_Name",
        "RFC_Parameter",
        "Description",
        "Final_Variable",
        "Tainted_Variables_Count",
        "Trace_Steps",
    ]

    # 추적 경로 컬럼들을 순서대로 추가
    trace_columns = sorted(
        [col for col in all_columns if col.startswith("Trace_Step_")]
    )
    ordered_columns.extend(trace_columns)

    # 나머지 컬럼들 추가
    remaining_columns = [col for col in all_columns if col not in ordered_columns]
    ordered_columns.extend(remaining_columns)

    # CSV 파일 작성
    with open(output_file, "w", newline="", encoding="utf-8-sig") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=ordered_columns)

        # 헤더 작성
        writer.writeheader()

        # 데이터 작성
        for result in results:
            # 빈 컬럼들을 빈 문자열로 채움
            complete_result = {col: result.get(col, "") for col in ordered_columns}
            writer.writerow(complete_result)


def main():
    """
    메인 함수
    """

    print("🚀 SY-UNAME 추적 결과 CSV 출력 도구")
    print(f"📅 실행 시간: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()

    success = analyze_and_export_to_csv()

    if success:
        print()
        print("🎉 CSV 출력이 성공적으로 완료되었습니다!")
        print()
        print("📋 CSV 파일 구조:")
        print("   A: ID (1,2,3,...)")
        print("   B: Source_File (소스코드 파일이름)")
        print("   C: SY_UNAME_Line (SY-UNAME 라인번호)")
        print("   D: Final_Table (최종 사용 테이블)")
        print("   E: Final_Fields (최종 사용 필드)")
        print("   F: RFC_Name (최종 사용된 RFC 이름)")
        print("   G: RFC_Parameter (최종 사용된 RFC 파라미터)")
        print("   H~: Trace_Step_01, 02, ... (추적 경로)")
        print()
        print("💡 Excel에서 열어서 확인하실 수 있습니다.")
    else:
        print()
        print("❌ CSV 출력 중 오류가 발생했습니다.")
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
