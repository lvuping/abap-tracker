#!/usr/bin/env python3
"""
비즈니스 시나리오 테스트 스크립트
데이터베이스 작업, 감사 필드, RFC 호출 등 실제 업무에서 중요한 Sink 포인트들을 테스트
"""

import re
from patterns import *
from analyzer import trace_sy_uname_in_snippet


def test_database_operation_patterns():
    """
    데이터베이스 작업 패턴들을 테스트
    """

    print("=== 🗄️ 데이터베이스 작업 패턴 테스트 ===\n")

    # 실제 비즈니스에서 사용되는 데이터베이스 작업 예시들
    db_test_cases = [
        # UPDATE 문들
        (
            "UPDATE 문",
            [
                "UPDATE zdocuments SET status = 'P', changed_by = lv_user WHERE doc_id = p_docid.",
                "UPDATE zuser_stats SET last_activity = sy-datum, last_action_user = lv_current_user WHERE user_id = lv_user.",
                "update ztable set created_by = lv_creator, modified_by = sy-uname where id = lv_id.",
            ],
        ),
        # INSERT 문들
        (
            "INSERT 문",
            [
                "INSERT zdocuments FROM ls_document.",
                "INSERT zaudit_log FROM ls_audit_record.",
                "INSERT INTO zcustom_audit VALUES (lv_id, lv_user, sy-datum).",
            ],
        ),
        # MODIFY 문들
        (
            "MODIFY 문",
            [
                "MODIFY zdocuments FROM TABLE lt_documents.",
                "MODIFY zuser_data FROM ls_user_record.",
                "modify ztransaction_log from wa_log.",
            ],
        ),
        # DELETE 문들
        (
            "DELETE 문",
            [
                "DELETE FROM zdocuments WHERE doc_id = p_docid AND created_by = lv_user.",
                "DELETE FROM zaudit_log WHERE user_name = lv_current_user AND log_date < lv_cutoff.",
                "delete from ztmp_table where session_user = sy-uname.",
            ],
        ),
    ]

    total_tests = 0
    total_matches = 0

    for category, test_lines in db_test_cases:
        print(f"📋 **{category}**")
        category_matches = 0

        for i, line in enumerate(test_lines, 1):
            print(f"  Line {i}: {line}")
            line_upper = line.strip().upper()
            matches_found = []

            # 데이터베이스 패턴 테스트
            db_patterns = [
                ("UPDATE_PATTERN", UPDATE_PATTERN),
                ("INSERT_PATTERN", INSERT_PATTERN),
                ("MODIFY_PATTERN", MODIFY_PATTERN),
                ("DELETE_PATTERN", DELETE_PATTERN),
            ]

            for pattern_name, pattern in db_patterns:
                match = pattern.match(line_upper)
                if match:
                    matches_found.append(pattern_name)
                    category_matches += 1

                    # 매칭된 정보 출력
                    if pattern_name == "UPDATE_PATTERN":
                        print(f"    ✅ {pattern_name}: 테이블={match.group('table')}")
                        print(f"       SET 절: {match.group('assignments')[:50]}...")
                    elif pattern_name == "INSERT_PATTERN":
                        print(f"    ✅ {pattern_name}: 테이블={match.group('table')}")
                        if match.group("source"):
                            print(f"       FROM: {match.group('source')}")
                    elif pattern_name == "MODIFY_PATTERN":
                        print(
                            f"    ✅ {pattern_name}: 테이블={match.group('table')}, 소스={match.group('source')}"
                        )
                    elif pattern_name == "DELETE_PATTERN":
                        print(f"    ✅ {pattern_name}: 테이블={match.group('table')}")
                        print(f"       WHERE: {match.group('conditions')[:50]}...")

            if not matches_found:
                print("    ❌ No database pattern matched")

            total_tests += 1

        print(f"  📊 카테고리 매칭률: {category_matches}/{len(test_lines)} 패턴 매칭\n")
        total_matches += category_matches

    print(f"🎯 **데이터베이스 패턴 테스트 요약**")
    print(f"   총 테스트 라인: {total_tests}")
    print(f"   총 패턴 매칭: {total_matches}")
    print(f"   매칭률: {total_matches/total_tests*100:.1f}%\n")


def test_audit_field_patterns():
    """
    감사 필드 패턴들을 테스트
    """

    print("=== 📝 감사 필드 패턴 테스트 ===\n")

    # 실제 SAP에서 사용되는 감사 필드들
    audit_test_cases = [
        "ls_document-created_by = lv_current_user.",
        "gs_header-changed_by = sy-uname.",
        "wa_data-erdat = sy-datum.",
        "ls_record-aenam = lv_user.",
        "gs_item-erzet = sy-uzeit.",
        "lt_log-uname = lv_processing_user.",
        "ls_audit-created_at = sy-datum.",
        "gs_change-changed_by = lv_modifier.",
        "wa_log-luser = sy-uname.",
        "ls_header-cuser = lv_creator.",
    ]

    print("📋 **감사 필드 할당 패턴**")

    matches = 0
    for i, line in enumerate(audit_test_cases, 1):
        print(f"  Line {i}: {line}")
        line_upper = line.strip().upper()

        # 감사 필드 패턴 테스트
        audit_match = AUDIT_FIELD_PATTERN.match(line_upper)
        if audit_match:
            structure = audit_match.group("structure")
            field = audit_match.group("field")
            source = audit_match.group("source")

            print(f"    ✅ AUDIT_FIELD_PATTERN: {structure}-{field} = {source}")
            matches += 1
        else:
            # 일반 구조체 할당인지 확인
            struct_match = STRUCTURE_ASSIGN_PATTERN.match(line_upper)
            if struct_match:
                print(f"    ⚠️ 일반 구조체 할당 (감사 필드 아님)")
            else:
                print(f"    ❌ 패턴 매칭 실패")

    print(f"\n📊 **감사 필드 패턴 요약**")
    print(f"   총 테스트: {len(audit_test_cases)}")
    print(f"   감사 필드 매칭: {matches}")
    print(f"   매칭률: {matches/len(audit_test_cases)*100:.1f}%\n")


def test_business_scenario_analysis():
    """
    실제 비즈니스 시나리오 코드를 분석
    """

    print("=== 🏢 비즈니스 시나리오 분석 테스트 ===\n")

    try:
        # 비즈니스 시나리오 파일 읽기
        with open("input/business_scenario_code.abap", "r", encoding="utf-8") as f:
            lines = f.readlines()

        print(f"📁 **파일**: business_scenario_code.abap (총 {len(lines)}줄)")

        # SY-UNAME이 있는 라인 45 주변 분석
        line_number = 45
        start = max(0, line_number - 101)
        end = min(len(lines), line_number + 100)
        snippet = lines[start:end]
        relative_start_line = line_number - start - 1

        print(f"🎯 **SY-UNAME 위치**: Line {line_number}")
        print(f"📊 **분석 범위**: Line {start+1} ~ {end} (총 {len(snippet)}줄)")

        # 주변 코드 미리보기
        print(f"\n📝 **SY-UNAME 주변 코드**:")
        for i in range(
            max(0, relative_start_line - 3), min(len(snippet), relative_start_line + 5)
        ):
            marker = " 🎯 " if i == relative_start_line else "    "
            print(f"{marker}Line {start+i+1:3d}: {snippet[i].rstrip()}")

        # 변수 추적 분석 실행
        print(f"\n🔍 **변수 추적 분석 실행...**")
        result = trace_sy_uname_in_snippet(snippet, relative_start_line)

        print(f"\n📊 **분석 결과**:")
        print(f"   상태: {result['status']}")
        print(f"   추적된 변수들: {', '.join(result.get('tainted_variables', []))}")
        print(f"   추적 단계: {len(result.get('path', []))}단계")

        if result.get("path"):
            print(f"\n📋 **변수 흐름 경로**:")
            for i, step in enumerate(result["path"], 1):
                print(f"   {i:2d}. {step}")

        # 결과 분석
        if result["status"] == "Found":
            print(f"\n✅ **중요한 Sink 발견!**")
            print(f"   유형: {result['type']}")

            if result["type"] == "DATABASE_UPDATE":
                print(f"   데이터베이스 테이블: {result['table']}")
                print(f"   작업: {result['operation']}")
                print(f"   설명: {result['description']}")
            elif result["type"] == "DATABASE_INSERT":
                print(f"   데이터베이스 테이블: {result['table']}")
                print(f"   작업: {result['operation']}")
                print(f"   설명: {result['description']}")
            elif result["type"] == "AUDIT_FIELD":
                print(f"   구조체: {result['structure']}")
                print(f"   감사 필드: {result['field']}")
                print(f"   설명: {result['description']}")
            elif result["type"] == "RFC":
                print(f"   RFC 이름: {result['name']}")
                print(f"   파라미터: {result['parameter']}")

            print(f"   최종 변수: {result['final_variable']}")
        else:
            print(f"\n⚠️ **Sink 미발견** - 추적은 성공했지만 중요한 종착점을 찾지 못함")

        # 스니펫 내 중요 패턴 검색
        print(f"\n🔍 **스니펫 내 중요 패턴 분석**")

        important_patterns = {
            "database_operations": [],
            "audit_fields": [],
            "rfc_calls": [],
            "structure_assignments": [],
        }

        for i, line in enumerate(snippet):
            line_num = start + i + 1
            line_upper = line.strip().upper()

            # 데이터베이스 작업 찾기
            if any(
                op in line_upper for op in ["UPDATE ", "INSERT ", "MODIFY ", "DELETE "]
            ):
                important_patterns["database_operations"].append(
                    (line_num, line.strip())
                )

            # 감사 필드 찾기
            if any(
                field in line_upper
                for field in ["CREATED_BY", "CHANGED_BY", "ERDAT", "AENAM"]
            ):
                important_patterns["audit_fields"].append((line_num, line.strip()))

            # RFC 호출 찾기
            if "CALL FUNCTION" in line_upper:
                important_patterns["rfc_calls"].append((line_num, line.strip()))

            # 구조체 할당 찾기
            if "-" in line and "=" in line and not line.strip().startswith("*"):
                important_patterns["structure_assignments"].append(
                    (line_num, line.strip())
                )

        # 패턴 결과 출력
        for pattern_type, patterns in important_patterns.items():
            if patterns:
                pattern_name = pattern_type.replace("_", " ").title()
                print(f"\n   📌 **{pattern_name}**: {len(patterns)}개 발견")
                for line_num, content in patterns[:5]:  # 처음 5개만 표시
                    print(f"      Line {line_num}: {content[:80]}...")
                if len(patterns) > 5:
                    print(f"      ... 그 외 {len(patterns)-5}개 더")

    except FileNotFoundError:
        print("❌ business_scenario_code.abap 파일을 찾을 수 없습니다.")
    except Exception as e:
        print(f"❌ 분석 중 오류 발생: {e}")


def test_all_new_patterns():
    """
    모든 새로운 패턴들을 종합 테스트
    """

    print("=== 🎯 신규 패턴 종합 테스트 ===\n")

    # 추가된 모든 패턴들 테스트
    comprehensive_test_cases = [
        # 데이터베이스 작업
        "UPDATE zdocuments SET changed_by = lv_user WHERE doc_id = p_id.",
        "INSERT zaudit_log FROM ls_audit.",
        "MODIFY ztable FROM wa_record.",
        "DELETE FROM ztmp WHERE user_id = lv_current_user.",
        # 감사 필드
        "ls_header-created_by = sy-uname.",
        "gs_data-aenam = lv_user.",
        "wa_record-changed_by = lv_modifier.",
        # 트랜잭션 호출
        "CALL TRANSACTION 'VA01' USING lt_bdc.",
        # BDC 필드
        "ls_bdc-fval = lv_user.",
        # 커밋/롤백
        "COMMIT WORK.",
        "ROLLBACK WORK.",
    ]

    pattern_tests = [
        ("UPDATE_PATTERN", UPDATE_PATTERN),
        ("INSERT_PATTERN", INSERT_PATTERN),
        ("MODIFY_PATTERN", MODIFY_PATTERN),
        ("DELETE_PATTERN", DELETE_PATTERN),
        ("AUDIT_FIELD_PATTERN", AUDIT_FIELD_PATTERN),
        ("CALL_TRANSACTION_PATTERN", CALL_TRANSACTION_PATTERN),
        ("BDC_FIELD_PATTERN", BDC_FIELD_PATTERN),
        ("COMMIT_PATTERN", COMMIT_PATTERN),
    ]

    print("📋 **종합 패턴 매칭 테스트**")

    total_matches = 0
    for i, line in enumerate(comprehensive_test_cases, 1):
        print(f"\n  Line {i}: {line}")
        line_upper = line.strip().upper()

        line_matches = 0
        for pattern_name, pattern in pattern_tests:
            if pattern.search(line_upper):
                print(f"    ✅ {pattern_name}")
                line_matches += 1
                total_matches += 1

        if line_matches == 0:
            print(f"    ❌ No pattern matched")

    print(f"\n🎯 **종합 테스트 결과**")
    print(f"   총 테스트 라인: {len(comprehensive_test_cases)}")
    print(f"   총 패턴 매칭: {total_matches}")
    print(f"   평균 매칭률: {total_matches/len(comprehensive_test_cases)*100:.1f}%")


def main():
    """
    모든 비즈니스 시나리오 테스트 실행
    """

    print("🚀 비즈니스 시나리오 테스트 시작\n")
    print("=" * 80)

    # 1. 데이터베이스 작업 패턴 테스트
    test_database_operation_patterns()
    print("=" * 80)

    # 2. 감사 필드 패턴 테스트
    test_audit_field_patterns()
    print("=" * 80)

    # 3. 실제 비즈니스 시나리오 분석
    test_business_scenario_analysis()
    print("=" * 80)

    # 4. 종합 패턴 테스트
    test_all_new_patterns()
    print("=" * 80)

    print("🎉 모든 비즈니스 시나리오 테스트 완료!")


if __name__ == "__main__":
    main()
