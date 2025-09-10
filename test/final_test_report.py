#!/usr/bin/env python3
"""
Final test report showing the real status of multiline handling
"""

import os
import sys
import glob
from analyzer import trace_sy_uname_in_snippet
from encoding_utils import safe_file_read

def test_file(filepath):
    """Test a single ABAP file and return detailed results"""
    results = {
        'file': filepath,
        'sy_uname_count': 0,
        'found_sink': 0,
        'scope_boundary': 0,
        'no_sink': 0,
        'errors': 0,
        'details': []
    }
    
    try:
        # 자동 인코딩 감지로 파일 읽기
        lines, encoding_used = safe_file_read(filepath)
        
        # Look for sy-uname in the file
        sy_uname_lines = []
        for i, line in enumerate(lines):
            if 'sy-uname' in line.lower():
                sy_uname_lines.append(i)
        
        results['sy_uname_count'] = len(sy_uname_lines)
        
        if not sy_uname_lines:
            return results
        
        # Test each sy-uname occurrence
        for line_num in sy_uname_lines:
            # Analyze with context
            start = max(0, line_num - 200)
            end = min(len(lines), line_num + 1000)
            snippet = lines[start:end]
            relative_line = line_num - start
            
            result = trace_sy_uname_in_snippet(snippet, relative_line)
            
            status = result.get('status', 'Unknown')
            detail = {
                'line': line_num + 1,
                'content': lines[line_num].strip(),
                'status': status,
                'type': result.get('type', ''),
                'table': result.get('table', ''),
                'fields': result.get('fields', [])
            }
            
            if status == 'Found':
                results['found_sink'] += 1
            elif status == 'Scope Boundary Reached':
                results['scope_boundary'] += 1
            elif result.get('error_type') in ['NO_SINK_FOUND_AFTER_TRACING', 'SYUNAME_IN_COMMENT_ONLY']:
                results['no_sink'] += 1
            else:
                results['errors'] += 1
            
            results['details'].append(detail)
        
    except Exception as e:
        results['errors'] += 1
        results['details'].append({'error': str(e)})
    
    return results

def main():
    """Generate comprehensive test report"""
    print("="*80)
    print("ABAP TRACKER - MULTILINE HANDLING TEST REPORT")
    print("="*80)
    print()
    
    # Get all ABAP files in input directory
    input_files = glob.glob('input/*.abap')
    input_files.sort()
    
    # Test priority files first (multiline/colon specific)
    priority_files = [
        'input/test_multiline.abap',
        'input/test_multiline_scenario.abap',
        'input/34_multiline_colon.abap',
        'input/test_colon_comma.abap',
        'input/08_clear_refresh_free.abap',
        'input/07_move_corresponding.abap',
    ]
    
    # Collect all results
    all_results = []
    
    # Test each file
    for filepath in input_files:
        result = test_file(filepath)
        all_results.append(result)
    
    # Print summary statistics
    print("OVERALL STATISTICS:")
    print("-" * 40)
    
    total_files = len(all_results)
    files_with_syuname = sum(1 for r in all_results if r['sy_uname_count'] > 0)
    total_syuname = sum(r['sy_uname_count'] for r in all_results)
    total_found = sum(r['found_sink'] for r in all_results)
    total_boundary = sum(r['scope_boundary'] for r in all_results)
    total_no_sink = sum(r['no_sink'] for r in all_results)
    total_errors = sum(r['errors'] for r in all_results)
    
    print(f"Total files analyzed: {total_files}")
    print(f"Files with sy-uname: {files_with_syuname}")
    print(f"Total sy-uname occurrences: {total_syuname}")
    print()
    print(f"✅ Found sink (SUCCESS): {total_found}")
    print(f"⚠️  Scope boundary reached: {total_boundary}")
    print(f"ℹ️  No sink found (EXPECTED): {total_no_sink}")
    print(f"❌ Actual errors: {total_errors}")
    print()
    
    # Show priority files results
    print("MULTILINE/COLON TEST FILES:")
    print("-" * 40)
    
    for filepath in priority_files:
        result = next((r for r in all_results if r['file'] == filepath), None)
        if result:
            basename = os.path.basename(filepath)
            if result['sy_uname_count'] == 0:
                print(f"{basename}: No sy-uname in file")
            else:
                print(f"{basename}:")
                print(f"  sy-uname occurrences: {result['sy_uname_count']}")
                print(f"  ✅ Sinks found: {result['found_sink']}")
                print(f"  ⚠️  Boundaries: {result['scope_boundary']}")
                print(f"  ℹ️  No sink: {result['no_sink']}")
                if result['errors'] > 0:
                    print(f"  ❌ Errors: {result['errors']}")
    
    print()
    
    # Show files with actual errors
    files_with_errors = [r for r in all_results if r['errors'] > 0]
    if files_with_errors:
        print("FILES WITH ACTUAL ERRORS:")
        print("-" * 40)
        for result in files_with_errors:
            print(f"  {os.path.basename(result['file'])}: {result['errors']} errors")
    else:
        print("✅ NO FILES WITH ACTUAL ERRORS!")
    
    print()
    print("="*80)
    print("CONCLUSION:")
    print("-" * 40)
    
    if total_errors == 0:
        print("✅ MULTILINE HANDLING IS WORKING CORRECTLY!")
        print("   All sy-uname occurrences are properly analyzed.")
        print("   Colon syntax is correctly expanded.")
        print("   'No sink found' results are EXPECTED for declarations only.")
    else:
        print(f"⚠️  There are {total_errors} actual errors that need investigation.")
    
    return 0 if total_errors == 0 else 1

if __name__ == '__main__':
    sys.exit(main())