#!/usr/bin/env python3
"""
SAP ABAP SY-UNAME 추적기 메인 실행 스크립트

기능:
- SY-UNAME 변수 흐름 추적
- 데이터베이스 테이블/필드 감지
- RFC 호출 감지
- JSON 및 CSV 결과 출력

사용법:
    python main.py                    # 기본 JSON 출력
    python main.py --csv             # CSV 출력 추가
    python main.py --format csv      # CSV만 출력
    python main.py --verbose         # 상세 출력
"""

import csv
import json
import sys
import os
from datetime import datetime
from analyzer import trace_sy_uname_in_snippet


def analyze_sy_uname_locations(output_format="json", verbose=False):
    """
    SY-UNAME 위치들을 분석하고 결과를 반환

    Args:
        output_format: "json", "csv", "both" 중 하나
        verbose: 상세 출력 여부
    """
    sy_uname_locations_file = "input/sy_uname_locations.csv"
    all_results = []

    # 입력 파일 확인
    if not os.path.exists(sy_uname_locations_file):
        print(f"❌ 오류: {sy_uname_locations_file} 파일을 찾을 수 없습니다.")
        return False

    print(f"🔍 SY-UNAME 추적 분석 시작")
    print(f"📁 입력 파일: {sy_uname_locations_file}")
    print("=" * 80)

    with open(sy_uname_locations_file, "r", encoding="utf-8", errors="replace") as f:
        csv_lines = f.readlines()

    # 총 라인 수 계산 (빈 라인 제외)
    total_locations = sum(1 for row in csv.reader(csv_lines) if row) - 1

    # CSV 내용을 다시 읽기 위해 csv.reader를 새로 생성
    reader = csv.reader(csv_lines)
    next(reader)  # 헤더 스킵

    for idx, row in enumerate(reader, 1):
        if len(row) == 3:  # id, file_path, line_number
            id_value, file_path, line_number = row
        elif len(row) == 2:  # 기존 형식 호환성 유지
            file_path, line_number = row
            id_value = None
        else:
            print(f"⚠️ 잘못된 CSV 형식: {row}")
            continue

        if not line_number:
            print(f"⚠️ line number가 없는 행을 건너뜁니다: {row}")
            continue
        
        line_number = int(line_number)

        # .abap 확장자 자동 추가
        if not file_path.endswith(".abap"):
            file_path = file_path + ".abap"

        # input/ 디렉토리 경로 자동 추가
        if not file_path.startswith("input/"):
            file_path = "input/" + file_path

        if verbose:
            print(
                f"📍 {idx}/{total_locations}. 분석 중: ID={id_value}, {file_path} 라인 {line_number}"
            )
        else:
            print(
                f"📍 {idx}/{total_locations}. {os.path.basename(file_path)} (라인 {line_number})"
            )

        try:
            with open(file_path, "r", encoding="utf-8", errors="replace") as source_file:
                all_lines = source_file.readlines()

            # 분석할 코드 범위(앞 15줄, 뒤 150줄) 추출
            start = max(0, line_number - 16)
            end = min(len(all_lines), line_number + 150)
                snippet = all_lines[start:end]

                # snippet 내에서 sy-uname이 있는 상대적 라인 번호
                relative_start_line = line_number - start - 1

                result = trace_sy_uname_in_snippet(snippet, relative_start_line)

                if verbose:
                    # 상세 분석 결과 출력
                    print(f"   결과: {result['status']}")
                    print(
                        f"   추적 변수: {', '.join(result.get('tainted_variables', []))}"
                    )
                    print(f"   추적 단계: {len(result.get('trace_path', []))}단계")
                    if result.get("trace_path"):
                        print("   추적 경로:")
                        for step in result["trace_path"]:
                            print(f"     {step}")

                if result["status"] == "Found":
                    # 발견된 Sink 유형에 따라 다른 정보 출력
                    if result["type"] == "RFC":
                        print(f"   ✅ RFC 호출: {result['name']}")
                    elif result["type"] == "AUDIT_FIELD":
                        print(
                            f"   ✅ 감사 필드: {result['structure']}-{result['field']}"
                        )
                    elif result["type"] in [
                        "DATABASE_UPDATE_FIELD",
                        "DATABASE_INSERT_FIELD",
                        "DATABASE_MODIFY_FIELD",
                        "DATABASE_SELECT_WHERE",
                    ]:
                        print(
                            f"   🎯 데이터베이스: {result['table']}.{', '.join(result['fields'])} ({result.get('operation', 'UNKNOWN')})"
                        )
                    elif result["type"].startswith("DATABASE_"):
                        print(
                            f"   ✅ 데이터베이스: {result.get('operation', 'UNKNOWN')} {result['table']}"
                        )
                    elif result["type"] == "CALL_TRANSACTION":
                        print(f"   ✅ 트랜잭션: {result['transaction']}")
                    else:
                        print(f"   ✅ Sink: {result['type']}")
                elif result["status"] == "Scope Boundary Reached":
                    # 스코프 경계 도달 처리
                    boundary_type = result["type"]
                    boundary_line = result.get("boundary_line", "Unknown")

                    if boundary_type == "PERFORM_CALL":
                        subroutine = result.get("subroutine", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: PERFORM {subroutine} (라인 {boundary_line})"
                        )
                        suggestion = result.get("suggestion")
                        if suggestion:
                            print(f"      💡 추측: {suggestion}")
                    elif boundary_type == "INCLUDE":
                        include_name = result.get("include_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: INCLUDE {include_name} (라인 {boundary_line})"
                        )
                    elif boundary_type == "SUBMIT":
                        program_name = result.get("program_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: SUBMIT {program_name} (라인 {boundary_line})"
                        )
                    elif boundary_type == "FORM_DEFINITION":
                        form_name = result.get("form_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: FORM {form_name} 정의 (라인 {boundary_line})"
                        )
                    elif boundary_type == "FUNCTION_DEFINITION":
                        function_name = result.get("function_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: FUNCTION {function_name} 정의 (라인 {boundary_line})"
                        )
                    elif boundary_type == "METHOD_DEFINITION":
                        method_name = result.get("method_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: METHOD {method_name} 정의 (라인 {boundary_line})"
                        )
                    elif boundary_type == "CLASS_DEFINITION":
                        class_name = result.get("class_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: CLASS {class_name} 정의 (라인 {boundary_line})"
                        )
                    elif boundary_type == "DYNAMIC_PERFORM":
                        subroutine_var = result.get("subroutine_variable", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: 동적 PERFORM ({subroutine_var}) (라인 {boundary_line})"
                        )
                    elif boundary_type == "DYNAMIC_CALL_FUNCTION":
                        function_var = result.get("function_variable", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: 동적 CALL FUNCTION {function_var} (라인 {boundary_line})"
                        )
                    elif boundary_type == "DYNAMIC_CALL_METHOD":
                        object_ref = result.get("object_reference", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: 동적 CALL METHOD {object_ref} (라인 {boundary_line})"
                        )
                    elif boundary_type == "OBJECT_METHOD_CALL":
                        object_name = result.get("object_name", "Unknown")
                        method_name = result.get("method_name", "Unknown")
                        print(
                            f"   ⛔ 스코프 경계: {object_name}->{method_name}() (라인 {boundary_line})"
                        )
                    elif boundary_type == "TRY_BLOCK":
                        print(
                            f"   ⛔ 스코프 경계: TRY 블록 시작 (라인 {boundary_line})"
                        )
                    else:
                        print(
                            f"   ⛔ 스코프 경계: {boundary_type} (라인 {boundary_line})"
                        )

                    tainted_count = len(result.get("tainted_variables", []))
                    if tainted_count > 1:  # sy-uname 제외
                        print(f"      전파된 변수: {tainted_count-1}개")
                else:
                    # 엄격한 검증에 따른 구체적인 실패 이유 출력
                    error_type = result.get("error_type")

                    if error_type == "SYUNAME_NOT_AT_SPECIFIED_LINE":
                        print(f"   ❌ 지정된 라인에 SY-UNAME 없음")
                        print(
                            f"      라인 {result.get('specified_line')}: '{result.get('actual_content', 'N/A')}'"
                        )
                    elif error_type == "NO_ZY_TABLE_RFC_SINK_FOUND":
                        print(f"   ⚠️ SY-UNAME 추적됨 but Z/Y 테이블/RFC Sink 미발견")
                        analysis = result.get("analysis_summary", {})
                        print(
                            f"      • 분석된 문장: {analysis.get('total_statements_analyzed', 0)}개"
                        )
                        print(
                            f"      • 전파된 변수: {analysis.get('variables_propagated', 0)}개"
                        )
                        print(
                            f"      • 추적 단계: {analysis.get('trace_steps', 0)}단계"
                        )

                        # 힌트 정보 표시
                        hints = result.get("hints", [])
                        if hints:
                            print(f"      💡 힌트:")
                            for hint in hints:
                                print(f"         - {hint}")

                        if verbose and result.get("tainted_variables"):
                            print(
                                f"      • 오염된 변수들: {', '.join(result['tainted_variables'])}"
                            )
                    elif error_type == "NO_SINK_FOUND_AFTER_TRACING":
                        print(f"   ⚠️ SY-UNAME 추적됨 but 유효한 Sink 미발견")
                        analysis = result.get("analysis_summary", {})
                        print(
                            f"      • 분석된 문장: {analysis.get('total_statements_analyzed', 0)}개"
                        )
                        print(
                            f"      • 전파된 변수: {analysis.get('variables_propagated', 0)}개"
                        )
                        print(
                            f"      • 추적 단계: {analysis.get('trace_steps', 0)}단계"
                        )
                        if verbose and result.get("tainted_variables"):
                            print(
                                f"      • 오염된 변수들: {', '.join(result['tainted_variables'])}"
                            )
                    else:
                        print(
                            f"   ⚠️ 분석 실패: {result.get('reason', 'Unknown reason')}"
                        )

                # 모든 결과를 저장 (성공/실패 무관)
                all_results.append(
                    {
                        "id": id_value,
                        "source_file": file_path,
                        "source_line": line_number,
                        "result": result,
                    }
                )

            except FileNotFoundError:
                print(f"   ❌ 파일 없음: {file_path}")
                all_results.append(
                    {
                        "id": id_value,
                        "source_file": file_path,
                        "source_line": line_number,
                        "result": {
                            "status": "File Not Found",
                            "error": "File not found",
                        },
                    }
                )
            except Exception as e:
                print(f"   ❌ 분석 오류: {e}")
                all_results.append(
                    {
                        "id": id_value,
                        "source_file": file_path,
                        "source_line": line_number,
                        "result": {"status": "Error", "error": str(e)},
                    }
                )

    # 결과 저장
    output_dir = "output"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # JSON 출력
    if output_format in ["json", "both"]:
        json_file = f"{output_dir}/analysis_result.json"
        with open(json_file, "w", encoding="utf-8") as out_file:
            json.dump(all_results, out_file, indent=2, ensure_ascii=False)
        print(f"\n📄 JSON 결과 저장: {json_file}")

    # CSV 출력
    if output_format in ["csv", "both"]:
        csv_file = f"{output_dir}/sy_uname_analysis_results.csv"
        export_to_csv(all_results, csv_file)
        print(f"📊 CSV 결과 저장: {csv_file}")

    # 결과 요약
    print("\n" + "=" * 80)
    print(f"🎉 분석 완료!")

    found_count = sum(1 for r in all_results if r["result"].get("status") == "Found")
    scope_boundary_count = sum(
        1 for r in all_results if r["result"].get("status") == "Scope Boundary Reached"
    )
    error_count = sum(
        1
        for r in all_results
        if r["result"].get("status") in ["Error", "File Not Found"]
    )
    syuname_not_found_count = sum(
        1
        for r in all_results
        if r["result"].get("error_type") == "SYUNAME_NOT_AT_SPECIFIED_LINE"
    )
    traced_but_no_sink_count = sum(
        1
        for r in all_results
        if r["result"].get("error_type")
        in ["NO_SINK_FOUND_AFTER_TRACING", "NO_ZY_TABLE_RFC_SINK_FOUND"]
    )

    print(f"📊 총 분석: {len(all_results)}개")
    print(f"✅ 성공 (Sink 발견): {found_count}개")
    print(f"⛔ 스코프 경계 도달: {scope_boundary_count}개")
    print(f"❌ SY-UNAME 라인 불일치: {syuname_not_found_count}개")
    print(f"⚠️ 추적 후 Sink 미발견: {traced_but_no_sink_count}개")
    print(f"❌ 파일/분석 오류: {error_count}개")

    other_not_found = (
        len(all_results)
        - found_count
        - scope_boundary_count
        - error_count
        - syuname_not_found_count
        - traced_but_no_sink_count
    )
    if other_not_found > 0:
        print(f"❓ 기타 미발견: {other_not_found}개")

    return True


def export_to_csv(results, output_file):
    """
    분석 결과를 CSV 파일로 출력
    """
    if not results:
        print("❌ CSV 출력할 결과가 없습니다.")
        return

    csv_rows = []
    for result_item in results:
        analysis_result = result_item["result"]

        # 기본 정보
        row = {
            "ID": result_item["id"],
            "Source_File": result_item["source_file"],
            "SY_UNAME_Line": result_item["source_line"],
            "Status": analysis_result.get("status", "Unknown"),
            "Type": analysis_result.get("type", ""),
            "Final_Table": "",
            "Final_Fields": "",
            "RFC_Name": "",
            "RFC_Parameter": "",
            "Description": analysis_result.get("description", ""),
            "Suggestion": analysis_result.get("suggestion", ""),
            "Final_Variable": analysis_result.get("final_variable", ""),
            "Boundary_Line": analysis_result.get("boundary_line", ""),
            "Boundary_Details": "",
            "Tainted_Variables_Count": len(
                analysis_result.get("tainted_variables", [])
            ),
            "Trace_Steps": len(
                analysis_result.get("trace_path", analysis_result.get("path", []))
            ),
        }

        # 타입별 상세 정보 추가
        result_type = analysis_result.get("type", "")

        if result_type in [
            "DATABASE_INSERT_FIELD",
            "DATABASE_UPDATE_FIELD",
            "DATABASE_MODIFY_FIELD",
            "DATABASE_SELECT_WHERE",
        ]:
            row["Final_Table"] = analysis_result.get("table", "")
            row["Final_Fields"] = ", ".join(analysis_result.get("fields", []))
        elif result_type.startswith("DATABASE_"):
            row["Final_Table"] = analysis_result.get("table", "")
        elif result_type == "RFC":
            row["RFC_Name"] = analysis_result.get("name", "")
            row["RFC_Parameter"] = analysis_result.get("parameter", "")
        elif result_type == "AUDIT_FIELD":
            structure = analysis_result.get("structure", "")
            field = analysis_result.get("field", "")
            row["Final_Fields"] = f"{structure}-{field}"
        elif result_type in [
            "PERFORM_CALL",
            "INCLUDE",
            "SUBMIT",
            "FORM_DEFINITION",
            "FUNCTION_DEFINITION",
            "METHOD_DEFINITION",
            "CLASS_DEFINITION",
            "DYNAMIC_PERFORM",
            "DYNAMIC_CALL_FUNCTION",
            "DYNAMIC_CALL_METHOD",
            "OBJECT_METHOD_CALL",
            "TRY_BLOCK",
        ]:
            # 스코프 경계 정보 처리
            if result_type == "PERFORM_CALL":
                row["Boundary_Details"] = (
                    f"PERFORM {analysis_result.get('subroutine', 'Unknown')}"
                )
            elif result_type == "INCLUDE":
                row["Boundary_Details"] = (
                    f"INCLUDE {analysis_result.get('include_name', 'Unknown')}"
                )
            elif result_type == "SUBMIT":
                row["Boundary_Details"] = (
                    f"SUBMIT {analysis_result.get('program_name', 'Unknown')}"
                )
            elif result_type == "FORM_DEFINITION":
                row["Boundary_Details"] = (
                    f"FORM {analysis_result.get('form_name', 'Unknown')} 정의"
                )
            elif result_type == "FUNCTION_DEFINITION":
                row["Boundary_Details"] = (
                    f"FUNCTION {analysis_result.get('function_name', 'Unknown')} 정의"
                )
            elif result_type == "METHOD_DEFINITION":
                row["Boundary_Details"] = (
                    f"METHOD {analysis_result.get('method_name', 'Unknown')} 정의"
                )
            elif result_type == "CLASS_DEFINITION":
                row["Boundary_Details"] = (
                    f"CLASS {analysis_result.get('class_name', 'Unknown')} 정의"
                )
            elif result_type == "DYNAMIC_PERFORM":
                row["Boundary_Details"] = (
                    f"동적 PERFORM ({analysis_result.get('subroutine_variable', 'Unknown')})"
                )
            elif result_type == "DYNAMIC_CALL_FUNCTION":
                row["Boundary_Details"] = (
                    f"동적 CALL FUNCTION {analysis_result.get('function_variable', 'Unknown')}"
                )
            elif result_type == "DYNAMIC_CALL_METHOD":
                row["Boundary_Details"] = (
                    f"동적 CALL METHOD {analysis_result.get('object_reference', 'Unknown')}"
                )
            elif result_type == "OBJECT_METHOD_CALL":
                object_name = analysis_result.get("object_name", "Unknown")
                method_name = analysis_result.get("method_name", "Unknown")
                row["Boundary_Details"] = f"{object_name}->{method_name}()"
            elif result_type == "TRY_BLOCK":
                row["Boundary_Details"] = "TRY 블록 시작"

        # 추적 경로를 개별 컬럼으로 추가 (최대 10단계)
        trace_path = analysis_result.get("trace_path", analysis_result.get("path", []))
        for i, step in enumerate(trace_path[:10], 1):
            row[f"Trace_Step_{i:02d}"] = step

        csv_rows.append(row)

    # 모든 가능한 컬럼 수집
    all_columns = set()
    for row in csv_rows:
        all_columns.update(row.keys())

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
        "Suggestion",
        "Final_Variable",
        "Boundary_Line",
        "Boundary_Details",
        "Tainted_Variables_Count",
        "Trace_Steps",
    ]

    # 추적 경로 컬럼들을 순서대로 추가
    trace_columns = sorted(
        [col for col in all_columns if col.startswith("Trace_Step_")]
    )
    ordered_columns.extend(trace_columns)

    # CSV 파일 작성
    with open(output_file, "w", newline="", encoding="utf-8-sig") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=ordered_columns)
        writer.writeheader()

        for row in csv_rows:
            complete_row = {col: row.get(col, "") for col in ordered_columns}
            writer.writerow(complete_row)


def main():
    """
    메인 함수
    """
    print("🚀 SAP ABAP SY-UNAME 추적기")
    print(f"📅 실행 시간: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()

    # 명령행 인자 처리
    output_format = "json"  # 기본값
    verbose = False

    if "--csv" in sys.argv:
        output_format = "both"
    elif "--format" in sys.argv:
        try:
            format_idx = sys.argv.index("--format")
            if format_idx + 1 < len(sys.argv):
                output_format = sys.argv[format_idx + 1]
        except (ValueError, IndexError):
            pass

    if "--verbose" in sys.argv or "-v" in sys.argv:
        verbose = True

    # 분석 실행
    success = analyze_sy_uname_locations(output_format, verbose)

    if not success:
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
