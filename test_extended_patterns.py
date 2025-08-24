#!/usr/bin/env python3
"""
확장된 ABAP 패턴 테스트 스크립트
다양한 할당 패턴 (=, MOVE, MOVE-CORRESPONDING 등)을 포괄적으로 테스트
"""

import re
from patterns import *


def test_all_assignment_patterns():
    """
    모든 할당 관련 패턴들을 테스트
    """

    print("=== 🔍 확장된 ABAP 패턴 테스트 ===\n")

    # 다양한 ABAP 할당 패턴 테스트 케이스들
    test_cases = [
        # 기본 할당
        (
            "기본 할당",
            [
                "  lv_user = sy-uname.",
                "  gv_current_user = lv_user.",
                "  ls_data-user_id = gv_current_user.",
            ],
        ),
        # MOVE 문
        (
            "MOVE 문",
            [
                "  MOVE sy-uname TO lv_user.",
                "  MOVE lv_user TO gv_target.",
                "  MOVE gv_target TO ls_structure-field.",
            ],
        ),
        # MOVE-CORRESPONDING
        (
            "MOVE-CORRESPONDING",
            [
                "  MOVE-CORRESPONDING gs_source TO gs_target.",
                "  MOVE_CORRESPONDING lt_table TO lt_result.",
                "  move-corresponding wa_input to wa_output.",
            ],
        ),
        # CONCATENATE
        (
            "CONCATENATE",
            [
                "  CONCATENATE 'User:' lv_user INTO lv_message.",
                "  CONCATENATE lv_prefix sy-uname lv_suffix INTO lv_result SEPARATED BY '-'.",
                "  concatenate 'ID=' lv_user into lv_final.",
            ],
        ),
        # REPLACE
        (
            "REPLACE",
            [
                "  REPLACE 'old' IN lv_text WITH lv_user.",
                "  REPLACE ALL OCCURRENCES OF 'temp' IN lv_string WITH sy-uname.",
                "  replace '*' in lv_mask with lv_user.",
            ],
        ),
        # SPLIT
        (
            "SPLIT",
            [
                "  SPLIT lv_user AT '@' INTO lv_name lv_domain.",
                "  SPLIT sy-uname AT '_' INTO TABLE lt_parts.",
                "  split lv_full_name at ' ' into lv_first lv_last.",
            ],
        ),
        # COMPUTE & 계산식
        (
            "COMPUTE & 계산",
            [
                "  COMPUTE lv_result = lv_user + 10.",
                "  lv_total = lv_user * 2.",
                "  compute lv_hash = sy-uname + sy-datum.",
            ],
        ),
        # SELECT INTO
        (
            "SELECT INTO",
            [
                "  SELECT SINGLE name1 FROM lfa1 INTO lv_vendor WHERE lifnr = lv_user.",
                "  SELECT * FROM users INTO TABLE lt_users WHERE userid = sy-uname.",
                "  select user_name from auth_table into lv_name where id = lv_user.",
            ],
        ),
        # 구조체 할당
        (
            "구조체 할당",
            [
                "  gs_header-created_by = sy-uname.",
                "  ls_item-user_id = lv_user.",
                "  wa_data-modified_by = gv_current_user.",
            ],
        ),
        # PERFORM 호출
        (
            "PERFORM 호출",
            [
                "  PERFORM validate_user USING sy-uname.",
                "  PERFORM process_data USING lv_user CHANGING lv_result.",
                "  perform check_authorization using gv_current_user.",
            ],
        ),
        # 복합 패턴
        (
            "복합 패턴",
            [
                "  CALL FUNCTION 'USER_GET_INFO' EXPORTING user_id = sy-uname.",
                "  IF lv_user = sy-uname.",
                "  WHERE created_by = @lv_user AND modified_by = @sy-uname.",
            ],
        ),
    ]

    # 각 카테고리별로 테스트 실행
    total_tests = 0
    total_matches = 0

    for category, test_lines in test_cases:
        print(f"📋 **{category}**")
        category_matches = 0

        for i, line in enumerate(test_lines, 1):
            print(f"  Line {i}: {line}")
            line_upper = line.strip().upper()
            matches_found = []

            # 모든 패턴에 대해 테스트
            patterns_to_test = [
                ("ASSIGN_PATTERN", ASSIGN_PATTERN),
                ("MOVE_PATTERN", MOVE_PATTERN),
                ("MOVE_CORRESPONDING_PATTERN", MOVE_CORRESPONDING_PATTERN),
                ("CONCATENATE_PATTERN", CONCATENATE_PATTERN),
                ("REPLACE_PATTERN", REPLACE_PATTERN),
                ("SPLIT_PATTERN", SPLIT_PATTERN),
                ("COMPUTE_PATTERN", COMPUTE_PATTERN),
                ("SELECT_INTO_PATTERN", SELECT_INTO_PATTERN),
                ("STRUCTURE_ASSIGN_PATTERN", STRUCTURE_ASSIGN_PATTERN),
                ("PERFORM_PATTERN", PERFORM_PATTERN),
                ("RFC_CALL_PATTERN", RFC_CALL_PATTERN),
                ("WHERE_CONDITION_PATTERN", WHERE_CONDITION_PATTERN),
            ]

            for pattern_name, pattern in patterns_to_test:
                if pattern.search(line_upper):
                    matches_found.append(pattern_name)
                    category_matches += 1

            if matches_found:
                for match in matches_found:
                    print(f"    ✅ {match}")
            else:
                print("    ❌ No pattern matched")

            total_tests += 1

        print(f"  📊 카테고리 매칭률: {category_matches}/{len(test_lines)} 패턴 매칭\n")
        total_matches += category_matches

    print(f"🎯 **전체 결과 요약**")
    print(f"   총 테스트 라인: {total_tests}")
    print(f"   총 패턴 매칭: {total_matches}")
    print(f"   평균 매칭률: {total_matches/total_tests*100:.1f}%")
    print()


def test_variable_flow_patterns():
    """
    변수 흐름 추적에 특화된 테스트
    """

    print("=== 🔄 변수 흐름 추적 테스트 ===\n")

    # SY-UNAME이 여러 단계를 거쳐 전파되는 시나리오
    flow_scenarios = [
        {
            "name": "기본 흐름",
            "code": [
                "lv_user = sy-uname.",
                "gv_current = lv_user.",
                "ls_data-user_id = gv_current.",
                "CALL FUNCTION 'RFC_FUNC' EXPORTING user = ls_data-user_id.",
            ],
        },
        {
            "name": "MOVE 흐름",
            "code": [
                "MOVE sy-uname TO lv_original.",
                "MOVE lv_original TO lv_copy.",
                "PERFORM process USING lv_copy.",
            ],
        },
        {
            "name": "CONCATENATE 흐름",
            "code": [
                "lv_user = sy-uname.",
                "CONCATENATE 'USER_' lv_user INTO lv_prefixed.",
                "CALL FUNCTION 'AUTH_CHECK' EXPORTING userid = lv_prefixed.",
            ],
        },
        {
            "name": "MOVE-CORRESPONDING 흐름",
            "code": [
                "gs_source-user = sy-uname.",
                "MOVE-CORRESPONDING gs_source TO gs_target.",
                "PERFORM validate USING gs_target-user.",
            ],
        },
        {
            "name": "복합 흐름",
            "code": [
                "lv_original = sy-uname.",
                "CONCATENATE 'PREFIX_' lv_original INTO lv_temp.",
                "SPLIT lv_temp AT '_' INTO lv_prefix lv_actual_user.",
                "gs_data-created_by = lv_actual_user.",
                "CALL FUNCTION 'LOG_USER' EXPORTING user_id = gs_data-created_by.",
            ],
        },
    ]

    for scenario in flow_scenarios:
        print(f"📋 **{scenario['name']}**")

        for i, line in enumerate(scenario["code"], 1):
            print(f"  Step {i}: {line}")

            # 각 라인에서 매칭되는 패턴 확인
            line_upper = line.strip().upper()

            if ASSIGN_PATTERN.match(line_upper):
                match = ASSIGN_PATTERN.match(line_upper)
                print(f"    → 할당: {match.group('source')} → {match.group('target')}")
            elif MOVE_PATTERN.match(line_upper):
                match = MOVE_PATTERN.match(line_upper)
                print(f"    → MOVE: {match.group('source')} → {match.group('target')}")
            elif MOVE_CORRESPONDING_PATTERN.match(line_upper):
                match = MOVE_CORRESPONDING_PATTERN.match(line_upper)
                print(
                    f"    → MOVE-CORR: {match.group('source')} → {match.group('target')}"
                )
            elif CONCATENATE_PATTERN.match(line_upper):
                match = CONCATENATE_PATTERN.match(line_upper)
                print(
                    f"    → CONCATENATE: {match.group('sources')} → {match.group('target')}"
                )
            elif SPLIT_PATTERN.match(line_upper):
                match = SPLIT_PATTERN.match(line_upper)
                print(
                    f"    → SPLIT: {match.group('source')} → {match.group('targets')}"
                )
            elif STRUCTURE_ASSIGN_PATTERN.match(line_upper):
                match = STRUCTURE_ASSIGN_PATTERN.match(line_upper)
                print(
                    f"    → 구조체: {match.group('source')} → {match.group('target')}"
                )
            elif PERFORM_PATTERN.match(line_upper):
                match = PERFORM_PATTERN.match(line_upper)
                print(
                    f"    → PERFORM: {match.group('subroutine')} ({match.group('params')})"
                )
            elif RFC_CALL_PATTERN.search(line_upper):
                match = RFC_CALL_PATTERN.search(line_upper)
                print(f"    → RFC: {match.group('rfc_name')}")

        print()


def run_enhanced_analyzer_test():
    """
    개선된 analyzer로 실제 변수 추적 테스트
    """

    print("=== 🧪 실제 변수 추적 분석 테스트 ===\n")

    # 테스트용 코드 스니펫 생성
    test_snippet = [
        "* Test various assignment patterns",
        "DATA: lv_user TYPE sy-uname,",
        "      lv_temp TYPE string,",
        "      lv_final TYPE string,",
        "      gs_data TYPE some_structure.",
        "",
        "* Start with sy-uname",
        "lv_user = sy-uname.",
        "",
        "* MOVE pattern",
        "MOVE lv_user TO lv_temp.",
        "",
        "* CONCATENATE pattern",
        "CONCATENATE 'USER_' lv_temp INTO lv_final.",
        "",
        "* Structure assignment",
        "gs_data-user_id = lv_final.",
        "",
        "* RFC call",
        "CALL FUNCTION 'Z_TEST_RFC'",
        "  EXPORTING",
        "    user_parameter = gs_data-user_id.",
    ]

    # analyzer 함수 import 및 실행
    from analyzer import trace_sy_uname_in_snippet

    print("📝 **테스트 코드 스니펫:**")
    for i, line in enumerate(test_snippet, 1):
        marker = " 🎯 " if "sy-uname" in line else "    "
        print(f"{marker}Line {i:2d}: {line}")

    print(f"\n🔍 **변수 추적 분석 실행...**")

    # sy-uname이 있는 라인 찾기 (8번째 라인)
    sy_uname_line = 7  # 0-based index
    result = trace_sy_uname_in_snippet(test_snippet, sy_uname_line)

    print(f"📊 **분석 결과:**")
    print(f"   상태: {result['status']}")
    print(f"   추적된 변수들: {', '.join(result.get('tainted_variables', []))}")
    print(f"   추적 단계: {len(result.get('path', []))}단계")

    if result.get("path"):
        print(f"\n📋 **변수 흐름 경로:**")
        for i, step in enumerate(result["path"], 1):
            print(f"   {i}. {step}")

    if result["status"] == "Found":
        print(f"\n✅ **RFC 감지 성공!**")
        print(f"   RFC 이름: {result['name']}")
        print(f"   파라미터: {result['parameter']}")
        print(f"   최종 변수: {result['final_variable']}")
    else:
        print(f"\n⚠️ **RFC 미감지** - 추가 개선 필요")


def main():
    """
    모든 테스트 실행
    """
    print("🚀 ABAP 패턴 확장 테스트 시작\n")
    print("=" * 60)

    # 1. 패턴 매칭 테스트
    test_all_assignment_patterns()
    print("=" * 60)

    # 2. 변수 흐름 테스트
    test_variable_flow_patterns()
    print("=" * 60)

    # 3. 실제 분석기 테스트
    run_enhanced_analyzer_test()
    print("=" * 60)

    print("🎉 모든 테스트 완료!")


if __name__ == "__main__":
    main()
