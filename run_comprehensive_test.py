#!/usr/bin/env python3
"""
Comprehensive test runner for all input files
Tests multiline and colon syntax handling
"""

import os
import sys
import glob
from analyzer import trace_sy_uname_in_snippet

def test_file(filepath):
    """Test a single ABAP file"""
    print(f"\n{'='*60}")
    print(f"Testing: {filepath}")
    print(f"{'='*60}")
    
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        
        # Look for sy-uname in the file
        sy_uname_lines = []
        for i, line in enumerate(lines):
            if 'sy-uname' in line.lower():
                sy_uname_lines.append(i)
        
        if not sy_uname_lines:
            print("  No sy-uname found in file")
            return True
        
        print(f"  Found sy-uname at lines: {[l+1 for l in sy_uname_lines]}")
        
        # Test each sy-uname occurrence
        all_passed = True
        for line_num in sy_uname_lines:
            print(f"\n  Testing sy-uname at line {line_num+1}:")
            
            # Analyze with context (200 lines before, 1000 after)
            start = max(0, line_num - 200)
            end = min(len(lines), line_num + 1000)
            snippet = lines[start:end]
            relative_line = line_num - start
            
            result = trace_sy_uname_in_snippet(snippet, relative_line)
            
            # Print result
            status = result.get('status', 'Unknown')
            if status == 'Found':
                print(f"    ✅ SUCCESS: Found {result.get('type', 'Unknown')}")
                if result.get('table'):
                    print(f"       Table: {result['table']}")
                if result.get('fields'):
                    print(f"       Fields: {', '.join(result['fields'])}")
            elif status == 'Scope Boundary Reached':
                print(f"    ⚠️  BOUNDARY: {result.get('type', 'Unknown')}")
                print(f"       Reason: {result.get('reason', 'Unknown')}")
            else:
                print(f"    ❌ FAILED: {status}")
                if result.get('reason'):
                    print(f"       Reason: {result['reason']}")
                if result.get('error_type'):
                    print(f"       Error Type: {result['error_type']}")
                all_passed = False
        
        return all_passed
        
    except Exception as e:
        print(f"  ❌ ERROR: {e}")
        return False

def main():
    """Run tests on all input files"""
    print("ABAP Tracker Comprehensive Test Suite")
    print("Testing multiline and colon syntax handling")
    print("="*60)
    
    # Get all ABAP files in input directory
    input_files = glob.glob('input/*.abap')
    input_files.sort()
    
    # Test files that specifically test multiline/colon features
    priority_files = [
        'input/test_multiline.abap',
        'input/test_multiline_scenario.abap',
        'input/34_multiline_colon.abap',
        'input/test_colon_comma.abap',
        'input/08_clear_refresh_free.abap',
        'input/07_move_corresponding.abap',
    ]
    
    # Move priority files to front
    for pf in priority_files:
        if pf in input_files:
            input_files.remove(pf)
            input_files.insert(0, pf)
    
    # Track results
    total_files = len(input_files)
    passed_files = 0
    failed_files = []
    
    # Test each file
    for filepath in input_files:
        if test_file(filepath):
            passed_files += 1
        else:
            failed_files.append(filepath)
    
    # Summary
    print(f"\n{'='*60}")
    print("TEST SUMMARY")
    print(f"{'='*60}")
    print(f"Total files tested: {total_files}")
    print(f"Passed: {passed_files}")
    print(f"Failed: {len(failed_files)}")
    
    if failed_files:
        print("\nFailed files:")
        for f in failed_files:
            print(f"  - {f}")
        return 1
    else:
        print("\n✅ ALL TESTS PASSED!")
        return 0

if __name__ == '__main__':
    sys.exit(main())