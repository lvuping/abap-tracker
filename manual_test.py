import json
from analyzer import trace_sy_uname_in_snippet

# 시나리오 1: 여러 줄에 걸친 UPDATE SET 구문
scenario_1_code = [
    "UPDATE ztable_a",
    "SET ztest01 = 'some_value',",
    "    ztest02 = 'another_value',",
    "    aenam = sy-uname, "
    "    ernam = 'some_user'",
    "WHERE key = '1'."
]

# 시나리오 2: 여러 줄에 걸친 MOVE: 구문 후 INSERT
sceanrio_2_code = [
    "MOVE:",
    "  sy-uname TO gt_c710-ernam,",
    "  sy-datum TO gt_c710-erdat.",
    "* some comments here",
    "PERFORM some_routine.",
    "INSERT ztable_b FROM TABLE gt_c710."
]

print("--- 검증 시작 ---")

# 시나리오 1 검증
# sy-uname은 3번 라인(인덱스)에 있음
result_1 = trace_sy_uname_in_snippet(scenario_1_code, 3)
print("\n시나리오 1 결과 (Multi-line UPDATE):")
print(json.dumps(result_1, indent=2))
assert result_1.get('type') == 'DATABASE_UPDATE_FIELD'
assert result_1.get('table') == 'ZTABLE_A'
assert 'AENAM' in result_1.get('fields', [])
print("시나리오 1 검증 통과!")

# 시나리오 2 검증
# sy-uname은 1번 라인(인덱스)에 있음
result_2 = trace_sy_uname_in_snippet(sceanrio_2_code, 1)
print("\n시나리오 2 결과 (Multi-line MOVE: and INSERT):")
print(json.dumps(result_2, indent=2))
assert result_2.get('type') == 'DATABASE_INSERT_FIELD'
assert result_2.get('table') == 'ZTABLE_B'
assert 'ERNAM' in result_2.get('fields', [])
print("시나리오 2 검증 통과!")

print("\n--- 모든 검증 통과 ---")