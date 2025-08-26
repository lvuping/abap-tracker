#!/usr/bin/env python3
"""
직접 함수 호출 테스트 스크립트
"""

from test import analyze_and_export_to_csv


def test_direct_function_call():
    print("🧪 직접 함수 호출 테스트")
    print("=" * 50)

    print("📁 input 폴더의 모든 파일을 자동으로 스캔합니다...")
    print("🎯 타겟 도메인: sy-uname")
    print("💾 출력 파일: output/direct_call_test.csv")
    print()

    # 이 한 줄만 실행하면 모든 것이 자동으로 처리됩니다!
    success = analyze_and_export_to_csv(
        target_domains=["sy-uname"],
        input_directory="input",
        output_file="output/direct_call_test.csv",
    )

    if success:
        print("\n✅ 직접 함수 호출 테스트 완료!")
        print("💡 input 폴더의 모든 파일이 자동으로 스캔되었습니다.")
    else:
        print("\n❌ 직접 함수 호출 테스트 실패!")


if __name__ == "__main__":
    test_direct_function_call()
