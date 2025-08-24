import csv
import json
from analyzer import trace_sy_uname_in_snippet


def main():
    sy_uname_locations_file = "input/sy_uname_locations.csv"
    all_results = []

    with open(sy_uname_locations_file, "r", encoding="utf-8") as f:
        reader = csv.reader(f)
        next(reader)  # 헤더 스킵

        for row in reader:
            if len(row) == 3:  # id, file_path, line_number
                id_value, file_path, line_number = row
            elif len(row) == 2:  # 기존 형식 호환성 유지
                file_path, line_number = row
                id_value = None
            else:
                print(f"⚠️ 잘못된 CSV 형식: {row}")
                continue

            line_number = int(line_number)

            # .abap 확장자 자동 추가
            if not file_path.endswith(".abap"):
                file_path = file_path + ".abap"

            print(f"Analyzing: ID={id_value}, {file_path} at line {line_number}...")

            try:
                with open(file_path, "r", encoding="utf-8") as source_file:
                    all_lines = source_file.readlines()

                # 분석할 코드 범위(앞 200줄, 뒤 1000줄) 추출
                start = max(0, line_number - 201)
                end = min(len(all_lines), line_number + 1000)
                snippet = all_lines[start:end]

                # snippet 내에서 sy-uname이 있는 상대적 라인 번호
                relative_start_line = line_number - start - 1

                result = trace_sy_uname_in_snippet(snippet, relative_start_line)

                # 디버깅: 분석 결과 출력
                print(f"  Result status: {result['status']}")
                print(f"  Tainted variables: {result.get('tainted_variables', [])}")
                print(f"  Trace path length: {len(result.get('path', []))}")
                if result.get("path"):
                    print("  Trace path:")
                    for step in result["path"]:
                        print(f"    {step}")

                if result["status"] == "Found":
                    # 발견된 Sink 유형에 따라 다른 정보 출력
                    if result["type"] == "RFC":
                        print(f"  ✅ RFC 호출 감지: {result['name']}")
                    elif result["type"] == "AUDIT_FIELD":
                        print(
                            f"  ✅ 감사 필드 감지: {result['structure']}-{result['field']}"
                        )
                    elif result["type"] in [
                        "DATABASE_UPDATE_FIELD",
                        "DATABASE_INSERT_FIELD",
                        "DATABASE_MODIFY_FIELD",
                        "DATABASE_SELECT_WHERE",
                    ]:
                        print(f"  🎯 데이터베이스 테이블/필드 감지!")
                        print(f"      📊 테이블: {result['table']}")
                        print(f"      📋 필드: {', '.join(result['fields'])}")
                        print(f"      🔧 작업: {result['operation']}")
                        if result.get("source_structure"):
                            print(f"      🏗️ 소스 구조체: {result['source_structure']}")
                    elif result["type"].startswith("DATABASE_"):
                        print(
                            f"  ✅ 데이터베이스 작업 감지: {result['operation']} {result['table']}"
                        )
                    elif result["type"] == "CALL_TRANSACTION":
                        print(f"  ✅ 트랜잭션 호출 감지: {result['transaction']}")
                    else:
                        print(f"  ✅ 중요한 Sink 감지: {result['type']}")

                    print(f"      설명: {result.get('description', '상세 정보 없음')}")
                    print(f"      최종 변수: {result['final_variable']}")

                    all_results.append(
                        {
                            "id": id_value,
                            "source_file": file_path,
                            "source_line": line_number,
                            "result": result,
                        }
                    )
                else:
                    print("  ⚠ 중요한 Sink를 찾지 못했지만 변수 추적은 성공")
                    # 결과를 찾지 못했어도 추적 정보를 저장
                    all_results.append(
                        {
                            "id": id_value,
                            "source_file": file_path,
                            "source_line": line_number,
                            "result": result,
                        }
                    )
            except FileNotFoundError:
                print(f"  [ERROR] File not found: {file_path}")
            except Exception as e:
                print(f"  [ERROR] An error occurred: {e}")

    # 최종 결과 저장
    with open("output/analysis_result.json", "w", encoding="utf-8") as out_file:
        json.dump(all_results, out_file, indent=2, ensure_ascii=False)

    print("\nAnalysis complete! Results saved to output/analysis_result.json")


if __name__ == "__main__":
    main()
