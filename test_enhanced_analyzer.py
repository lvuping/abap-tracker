#!/usr/bin/env python3
"""
Test script for Enhanced ABAP Analyzer
Tests all improvements including advanced patterns, context-aware taint tracking, and performance
"""

import sys
import os
import time
from pathlib import Path

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from core.enhanced_analyzer import EnhancedABAPAnalyzer


def create_test_abap():
    """Create test ABAP file with various patterns"""
    test_code = """
*&---------------------------------------------------------------------*
*& Test Program for Enhanced Analyzer
*&---------------------------------------------------------------------*
REPORT test_enhanced.

DATA: lv_user TYPE syuname,
      lv_temp TYPE string,
      ls_rec TYPE ztable.

* Test 1: Direct system variable assignment
lv_user = sy-uname.

* Test 2: Structure field assignment
ls_rec-created_by = sy-uname.
ls_rec-created_date = sy-datum.

* Test 3: Standard INSERT
INSERT ztable FROM ls_rec.

* Test 4: EXEC SQL pattern (new)
EXEC SQL.
  INSERT INTO ztable (user_id, created_by)
  VALUES (:lv_user, :sy-uname)
ENDEXEC.

* Test 5: Authority check with SY-UNAME (new)
AUTHORITY-CHECK OBJECT 'S_USER_GRP'
  ID 'ACTVT' FIELD '03'
  ID 'CLASS' FIELD sy-uname.

* Test 6: BAPI call with user parameter (new)
CALL FUNCTION 'BAPI_USER_CREATE'
  EXPORTING
    username = sy-uname
    password = 'InitialPass123'.

* Test 7: Complex JOIN with SY-UNAME (new)
SELECT a~mandt, a~user_id, b~role
  FROM usr01 AS a
  INNER JOIN agr_users AS b
    ON a~user_id = b~uname
  INTO TABLE @DATA(lt_users)
  WHERE a~user_id = @sy-uname.

* Test 8: CDS View access (new)
SELECT * FROM Z_USER_CDS( )
  WHERE created_by = @sy-uname
  INTO TABLE @DATA(lt_cds_data).

* Test 9: Field symbol with taint
FIELD-SYMBOLS: <fs_user> TYPE any.
ASSIGN lv_user TO <fs_user>.
INSERT ztable FROM <fs_user>.

* Test 10: FORM with parameters
PERFORM update_user USING lv_user.

FORM update_user USING p_user TYPE syuname.
  DATA: ls_update TYPE ztable.
  ls_update-modified_by = p_user.
  UPDATE ztable FROM ls_update.
ENDFORM.

* Test 11: Multi-line statement with continuation
INSERT INTO ztable VALUES (
  client = sy-mandt,
  user_id = sy-uname,
  created_date = sy-datum,
  created_time = sy-uzeit ).

* Test 12: Chain statement
INSERT: ztable FROM ls_rec,
        zaudit FROM VALUE #( user = sy-uname date = sy-datum ).

* Test 13: Dynamic SQL (new)
DATA: lv_table TYPE string VALUE 'ZTABLE'.
INSERT (lv_table) FROM @ls_rec.

* Test 14: BDC data handling (new)
DATA: lt_bdcdata TYPE TABLE OF bdcdata.
DATA: ls_bdcdata TYPE bdcdata.
ls_bdcdata-fval = sy-uname.
APPEND ls_bdcdata TO lt_bdcdata.

* Test 15: MODIFY with TRANSPORTING
MODIFY ztable FROM ls_rec TRANSPORTING created_by modified_by
  WHERE user_id = sy-uname.
"""

    # Write test file
    test_file = Path("input/test_enhanced.abap")
    test_file.parent.mkdir(exist_ok=True)
    test_file.write_text(test_code)

    return str(test_file)


def test_enhanced_analyzer():
    """Test the enhanced analyzer with all improvements"""
    print("="*80)
    print("ENHANCED ABAP ANALYZER TEST")
    print("="*80)

    # Create test file
    test_file = create_test_abap()
    print(f"\n✅ Created test file: {test_file}")

    # Test 1: Basic functionality
    print("\n🔍 Test 1: Basic Analysis")
    print("-"*40)

    analyzer = EnhancedABAPAnalyzer(
        enable_caching=True,
        enable_sliding_window=True,
        window_size=30
    )

    results = analyzer.analyze_file(test_file)
    print(f"Found {len(results)} patterns")

    # Print results summary
    for i, result in enumerate(results, 1):
        print(f"\n  [{i}] Line {result.line_number}: {result.operation}")
        if result.table:
            print(f"      Table: {result.table}")
        if result.tainted_variables:
            print(f"      Tainted vars: {', '.join(result.tainted_variables)}")
        if result.advanced_patterns:
            print(f"      Advanced patterns: {len(result.advanced_patterns)} found")
        print(f"      Confidence: {result.confidence:.2f}")

    # Test 2: Performance comparison
    print("\n⚡ Test 2: Performance Comparison")
    print("-"*40)

    # With optimizations
    start = time.time()
    analyzer_optimized = EnhancedABAPAnalyzer(
        enable_caching=True,
        enable_sliding_window=True,
        window_size=30
    )
    results_opt = analyzer_optimized.analyze_file(test_file)
    time_optimized = time.time() - start

    # Without optimizations
    start = time.time()
    analyzer_basic = EnhancedABAPAnalyzer(
        enable_caching=False,
        enable_sliding_window=False
    )
    results_basic = analyzer_basic.analyze_file(test_file)
    time_basic = time.time() - start

    print(f"  With optimizations: {time_optimized:.4f}s")
    print(f"  Without optimizations: {time_basic:.4f}s")
    if time_basic > 0:
        speedup = ((time_basic - time_optimized) / time_basic) * 100
        print(f"  Speedup: {speedup:.1f}%")

    # Test 3: Advanced pattern detection
    print("\n🎯 Test 3: Advanced Pattern Detection")
    print("-"*40)

    advanced_patterns = ["EXEC_SQL", "AUTHORITY_CHECK", "BAPI_CALL", "CDS_VIEW", "DYNAMIC_SQL"]
    found_advanced = set()

    for result in results_opt:
        for pattern in result.advanced_patterns:
            pattern_type = pattern.get('operation_type', '')
            for adv_type in advanced_patterns:
                if adv_type in pattern_type:
                    found_advanced.add(adv_type)

    print(f"  Advanced patterns detected: {', '.join(found_advanced)}")
    print(f"  Coverage: {len(found_advanced)}/{len(advanced_patterns)}")

    # Test 4: Context-aware taint tracking
    print("\n🔐 Test 4: Context-Aware Taint Tracking")
    print("-"*40)

    tainted_count = sum(1 for r in results_opt if r.tainted_variables)
    sy_uname_count = sum(1 for r in results_opt if 'SY-UNAME' in str(r.fields.values()))

    print(f"  Operations with tainted variables: {tainted_count}")
    print(f"  Operations with SY-UNAME: {sy_uname_count}")

    # Show scope information
    scopes_found = set()
    for result in results_opt:
        if result.context_info and 'scope' in result.context_info:
            scope = result.context_info['scope']
            if scope != 'UNKNOWN':
                scopes_found.add(scope.split(':')[0])

    if scopes_found:
        print(f"  Scopes detected: {', '.join(scopes_found)}")

    # Test 5: Performance metrics
    print("\n📊 Test 5: Performance Metrics")
    print("-"*40)

    metrics = analyzer_optimized.get_performance_metrics()
    for key, value in metrics.items():
        if isinstance(value, float):
            print(f"  {key}: {value:.4f}")
        else:
            print(f"  {key}: {value}")

    # Summary
    print("\n" + "="*80)
    print("TEST SUMMARY")
    print("="*80)

    total_tests = 5
    passed_tests = 0

    # Check results
    if len(results_opt) >= 10:
        passed_tests += 1
        print("✅ Pattern detection: PASSED")
    else:
        print("❌ Pattern detection: FAILED")

    if time_optimized < time_basic or time_basic == 0:
        passed_tests += 1
        print("✅ Performance optimization: PASSED")
    else:
        print("❌ Performance optimization: FAILED")

    if len(found_advanced) >= 3:
        passed_tests += 1
        print("✅ Advanced patterns: PASSED")
    else:
        print("❌ Advanced patterns: FAILED")

    if tainted_count > 0:
        passed_tests += 1
        print("✅ Taint tracking: PASSED")
    else:
        print("❌ Taint tracking: FAILED")

    if metrics.get('cache_hit_rate', 0) > 0 or not analyzer_optimized.pattern_cache:
        passed_tests += 1
        print("✅ Performance metrics: PASSED")
    else:
        print("❌ Performance metrics: FAILED")

    print(f"\nOverall: {passed_tests}/{total_tests} tests passed")

    return passed_tests == total_tests


if __name__ == "__main__":
    success = test_enhanced_analyzer()
    sys.exit(0 if success else 1)