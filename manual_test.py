#!/usr/bin/env python3
"""
SY-UNAME 추적기 수동 테스트 스크립트

사용법:
    python manual_test.py <파일명> <라인번호>

예시:
    python manual_test.py input/business_scenario_code.abap 45
    python manual_test.py input/source_code.abap 90
"""

import sys
import os
from analyzer import trace_sy_uname_in_snippet


def print_colored(text, color_code):
    """색상 출력을 위한 함수"""
    print(f"\033[{color_code}m{text}\033[0m")


def manual_test(file_path, line_number):
    """
    지정된 파일과 라인에서 SY-UNAME 추적 테스트 실행

    Args:
        file_path: ABAP 파일 경로
        line_number: SY-UNAME이 있는 라인 번호 (1-based)
    """

    print_colored("=" * 80, "36")  # 시안색
    print_colored(f"🔍 SY-UNAME 추적기 수동 테스트", "33")  # 노란색
    print_colored("=" * 80, "36")

    print(f"\n📁 파일: {file_path}")
    print(f"📍 라인: {line_number}")

    # 파일 존재 확인
    if not os.path.exists(file_path):
        print_colored(
            f"\n❌ 오류: 파일을 찾을 수 없습니다 - {file_path}", "31"
        )  # 빨간색
        return False

    try:
        # 파일 읽기
        with open(file_path, "r", encoding="utf-8") as f:
            all_lines = f.readlines()

        total_lines = len(all_lines)
        print(f"📊 전체 라인 수: {total_lines}")

        # 라인 번호 유효성 검사 (1-based → 0-based 변환)
        line_number_0based = line_number - 1
        if line_number_0based < 0 or line_number_0based >= total_lines:
            print_colored(
                f"\n❌ 오류: 라인 번호가 범위를 벗어났습니다 (1-{total_lines})", "31"
            )
            return False

        # 분석 범위 계산 (main.py와 동일한 로직)
        start = max(0, line_number - 201)  # 앞으로 200줄
        end = min(total_lines, line_number + 1000)  # 뒤로 1000줄
        snippet = all_lines[start:end]
        relative_start_line = line_number - start - 1

        print(f"🎯 분석 범위: Line {start+1} ~ {end} (총 {len(snippet)}줄)")
        print(f"📋 SY-UNAME 상대 위치: {relative_start_line + 1}")

        # SY-UNAME이 있는 라인 확인
        target_line = all_lines[line_number_0based].strip()
        print(f"\n📝 대상 라인 내용:")
        print(f"   Line {line_number}: {target_line}")

        if "sy-uname" not in target_line.lower():
            print_colored(f"\n⚠️ 경고: 해당 라인에 'sy-uname'이 없습니다!", "33")
            print("   계속 진행하지만 결과가 정확하지 않을 수 있습니다.")

        # 주변 코드 미리보기 (앞뒤 3줄)
        print(f"\n👀 주변 코드 미리보기:")
        preview_start = max(0, relative_start_line - 3)
        preview_end = min(len(snippet), relative_start_line + 4)

        for i in range(preview_start, preview_end):
            line_num = start + i + 1
            marker = " 🎯 " if i == relative_start_line else "    "
            content = snippet[i].rstrip()
            print(f"{marker}Line {line_num:3d}: {content}")

        print_colored(f"\n🔍 SY-UNAME 추적 분석 시작...", "33")
        print("-" * 60)

        # SY-UNAME 추적 실행
        result = trace_sy_uname_in_snippet(snippet, relative_start_line)

        # 결과 출력
        print_colored(f"\n📊 분석 결과", "32")  # 초록색
        print("-" * 40)

        status = result.get("status", "Unknown")
        if status == "Found":
            print_colored(f"✅ 상태: {status}", "32")
        else:
            print_colored(f"⚠️ 상태: {status}", "33")

        # 추적된 변수들
        tainted_vars = result.get("tainted_variables", [])
        print(f"🔗 추적된 변수: {len(tainted_vars)}개")
        if tainted_vars:
            for i, var in enumerate(tainted_vars[:10], 1):  # 최대 10개만 표시
                print(f"   {i:2d}. {var}")
            if len(tainted_vars) > 10:
                print(f"   ... 그 외 {len(tainted_vars)-10}개 더")

        # 추적 경로
        trace_path = result.get("path", [])
        print(f"\n📈 추적 경로: {len(trace_path)}단계")
        if trace_path:
            for i, step in enumerate(trace_path[:8], 1):  # 최대 8단계만 표시
                print(f"   {i:2d}. {step}")
            if len(trace_path) > 8:
                print(f"   ... 그 외 {len(trace_path)-8}단계 더")

        # 발견된 Sink 정보
        if status == "Found":
            print_colored(f"\n🎯 발견된 중요 정보", "32")
            print("-" * 30)

            sink_type = result.get("type", "")
            print(f"📋 유형: {sink_type}")

            if sink_type in [
                "DATABASE_UPDATE_FIELD",
                "DATABASE_INSERT_FIELD",
                "DATABASE_MODIFY_FIELD",
                "DATABASE_SELECT_WHERE",
            ]:
                print_colored(f"🏆 데이터베이스 테이블/필드 감지!", "32")
                print(f"   📊 테이블: {result.get('table', 'Unknown')}")
                print(f"   📝 필드: {', '.join(result.get('fields', []))}")
                print(f"   🔧 작업: {result.get('operation', 'Unknown')}")
                if result.get("source_structure"):
                    print(f"   🏗️ 구조체: {result.get('source_structure')}")

            elif sink_type == "RFC":
                print_colored(f"📡 RFC 호출 감지!", "32")
                print(f"   📞 RFC 이름: {result.get('name', 'Unknown')}")
                print(f"   📋 파라미터: {result.get('parameter', 'Unknown')}")

            elif sink_type == "AUDIT_FIELD":
                print_colored(f"📝 감사 필드 감지!", "32")
                print(f"   🏗️ 구조체: {result.get('structure', 'Unknown')}")
                print(f"   📋 필드: {result.get('field', 'Unknown')}")

            elif sink_type == "CALL_TRANSACTION":
                print_colored(f"🔄 트랜잭션 호출 감지!", "32")
                print(f"   📞 트랜잭션: {result.get('transaction', 'Unknown')}")

            print(f"\n💡 설명: {result.get('description', '상세 정보 없음')}")

            final_var = result.get("final_variable", "")
            if final_var:
                print(f"🎯 최종 변수: {final_var}")

        print_colored(f"\n🎉 분석 완료!", "32")
        print("=" * 80)

        return True

    except Exception as e:
        print_colored(f"\n❌ 오류 발생: {str(e)}", "31")
        import traceback

        traceback.print_exc()
        return False


def main():
    """메인 함수"""

    if len(sys.argv) != 3:
        print_colored("사용법: python manual_test.py <파일명> <라인번호>", "33")
        print("\n예시:")
        print("  python manual_test.py input/business_scenario_code.abap 45")
        print("  python manual_test.py input/source_code.abap 90")
        print("  python manual_test.py input/extended_test_code.abap 37")
        print("\n💡 팁: 현재 분석 범위는 지정 라인 앞 200줄, 뒤 1000줄입니다.")
        sys.exit(1)

    file_path = sys.argv[1]

    # .abap 확장자 자동 추가
    if not file_path.endswith(".abap"):
        file_path = file_path + ".abap"

    try:
        line_number = int(sys.argv[2])
        if line_number <= 0:
            print_colored("❌ 오류: 라인 번호는 1 이상이어야 합니다.", "31")
            sys.exit(1)
    except ValueError:
        print_colored("❌ 오류: 라인 번호는 정수여야 합니다.", "31")
        sys.exit(1)

    # 테스트 실행
    success = manual_test(file_path, line_number)

    if success:
        print_colored("\n✅ 테스트 성공적으로 완료!", "32")
    else:
        print_colored("\n❌ 테스트 실패!", "31")
        sys.exit(1)


if __name__ == "__main__":
    main()
