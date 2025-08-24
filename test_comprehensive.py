#!/usr/bin/env python3
"""
종합 테스트 및 검증 스크립트
"""

import json
from analyzer import trace_sy_uname_in_snippet


def run_comprehensive_test():
    """
    실제 프로젝트의 전체 워크플로우를 테스트
    """

    print("=== SAP ABAP SY-UNAME 분석기 종합 테스트 ===\n")

    # 1. 실제 파일 분석
    with open("input/source_code.abap", "r", encoding="utf-8") as f:
        all_lines = f.readlines()

    line_number = 90
    start = max(0, line_number - 101)
    end = min(len(all_lines), line_number + 100)
    snippet = all_lines[start:end]
    relative_start_line = line_number - start - 1

    print(f"📁 분석 파일: input/source_code.abap")
    print(f"🎯 SY-UNAME 위치: Line {line_number}")
    print(f"📊 분석 범위: Line {start+1} ~ {end} (총 {len(snippet)}줄)")
    print()

    # 2. 변수 흐름 분석
    result = trace_sy_uname_in_snippet(snippet, relative_start_line)

    print("🔍 **변수 추적 결과**")
    print(f"   상태: {result['status']}")
    print(f"   오염된 변수들: {', '.join(result.get('tainted_variables', []))}")
    print()

    if result.get("path"):
        print("📋 **변수 흐름 경로**")
        for i, step in enumerate(result["path"], 1):
            print(f"   {i}. {step}")
        print()

    # 3. RFC 호출 정보 (찾은 경우)
    if result["status"] == "Found":
        print("✅ **RFC 호출 감지 성공!**")
        print(f"   RFC 이름: {result['name']}")
        print(f"   파라미터: {result['parameter']}")
        print(f"   최종 변수: {result['final_variable']}")
        print()
    else:
        print("❌ **RFC 호출 미감지**")
        print("   분석을 위해 추가 정보를 수집합니다...")
        print()

    # 4. 스니펫 내 중요 패턴 검색
    print("🔍 **스니펫 내 중요 패턴 분석**")

    patterns_found = {
        "assignments": [],
        "structure_assignments": [],
        "perform_calls": [],
        "rfc_calls": [],
        "form_definitions": [],
    }

    for i, line in enumerate(snippet):
        line_num = start + i + 1
        line_upper = line.strip().upper()

        # 할당문
        if (
            "=" in line
            and not line.strip().startswith("*")
            and not line.strip().startswith('"')
        ):
            if (
                "SY-UNAME" in line_upper
                or "LV_LIFNR" in line_upper
                or "IV_LIFNR" in line_upper
            ):
                patterns_found["assignments"].append((line_num, line.strip()))

        # 구조체 할당
        if "-" in line and "=" in line and not line.strip().startswith("*"):
            if "LIFNR" in line_upper:
                patterns_found["structure_assignments"].append((line_num, line.strip()))

        # PERFORM 호출
        if line_upper.strip().startswith("PERFORM"):
            if "LV_LIFNR" in line_upper:
                patterns_found["perform_calls"].append((line_num, line.strip()))

        # RFC 호출
        if "CALL FUNCTION" in line_upper:
            patterns_found["rfc_calls"].append((line_num, line.strip()))

        # FORM 정의
        if line_upper.strip().startswith("FORM"):
            if "LIFNR" in line_upper:
                patterns_found["form_definitions"].append((line_num, line.strip()))

    # 패턴 출력
    for pattern_type, patterns in patterns_found.items():
        if patterns:
            print(f"\n   📌 {pattern_type.replace('_', ' ').title()}:")
            for line_num, content in patterns:
                print(f"      Line {line_num}: {content}")

    print()

    # 5. 결과 요약
    print("📊 **분석 요약**")
    total_patterns = sum(len(patterns) for patterns in patterns_found.values())
    print(f"   - 총 {total_patterns}개의 관련 패턴 발견")
    print(f"   - 변수 전파 단계: {len(result.get('path', []))}단계")
    print(
        f"   - 최종 결과: {'✅ 성공' if result['status'] == 'Found' else '❌ 미완료'}"
    )

    return result


def create_test_report():
    """
    테스트 결과 리포트 생성
    """
    result = run_comprehensive_test()

    # 결과를 JSON 파일로 저장
    with open("output/test_report.json", "w", encoding="utf-8") as f:
        json.dump(
            {
                "test_timestamp": "2024-01-01",
                "test_result": result,
                "test_summary": {
                    "status": result["status"],
                    "variables_tracked": len(result.get("tainted_variables", [])),
                    "trace_steps": len(result.get("path", [])),
                    "success": result["status"] == "Found",
                },
            },
            f,
            indent=2,
            ensure_ascii=False,
        )

    print(f"\n📄 테스트 리포트가 output/test_report.json에 저장되었습니다.")


if __name__ == "__main__":
    create_test_report()
