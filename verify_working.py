#!/usr/bin/env python3
"""Verify that the exact user case works"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from csv_analyzer import EnhancedCSVAnalyzer
from encoding_utils import safe_file_read

def verify_user_case():
    """Verify the exact user case"""
    
    # Exact code from user
    code = """ptab-ernam = sy-uname.
ptab-erdat = sy-datum
ptab-uzeit = sy-uzeit.
ptab-aedat = sy-aedat.

append ptab_alt. clear ptab_alt.

loop at pt_alt.
at new werks.
delete from zmc0042
where matnr = ptab-matnr
and werks = ptab-werks.
endat.
endloop.

insert zmc0042 from table ptab accepting duplicate keys.
commit work."""
    
    lines = code.split('\n')
    
    # Find SY-UNAME line
    sy_uname_line = None
    for i, line in enumerate(lines):
        if 'ptab-ernam = sy-uname' in line.lower():
            sy_uname_line = i
            break
    
    print(f"✅ Found 'ptab-ernam = sy-uname' at line {sy_uname_line + 1}")
    
    # Initialize analyzer
    analyzer = EnhancedCSVAnalyzer()
    
    # Analyze
    analysis = analyzer.analyze_location(
        id_value="USER_CASE",
        file_path="user_case.abap",
        line_number=sy_uname_line,
        code_snippet=lines,
        actual_line_number=sy_uname_line + 1
    )
    
    # Display results
    print("\n📊 Analysis Results:")
    print(f"  Status: {analysis.status}")
    print(f"  Tables: {analysis.tables}")
    print(f"  Fields: {analysis.fields}")
    print(f"  Operations: {[op for op in analysis.operations if 'INSERT' in op.upper()]}")
    print(f"  Tainted Variables: {[v for v in analysis.tainted_variables if 'ERNAM' in v.upper()]}")
    
    # Verify expected results
    success = True
    errors = []
    
    if 'ZMC0042' not in [t.upper() for t in analysis.tables]:
        errors.append("Table 'zmc0042' not found")
        success = False
    
    if 'ERNAM' not in [f.upper() for f in analysis.fields]:
        errors.append("Field 'ernam' not found")
        success = False
    
    if not any('INSERT' in op.upper() for op in analysis.operations):
        errors.append("Operation 'insert' not found")
        success = False
    
    print("\n📋 Verification:")
    if success:
        print("✅ SUCCESS: Found all required elements:")
        print("   - Table: zmc0042")
        print("   - Field: ernam")  
        print("   - Operation: insert")
        print("\n✅ The Python code IS correctly extracting the pattern!")
    else:
        print("❌ FAILED: Missing elements:")
        for error in errors:
            print(f"   - {error}")
    
    return success

if __name__ == "__main__":
    print("🔍 Verifying User's Exact Case")
    print("=" * 50)
    print("Testing pattern: ptab-ernam = sy-uname → insert zmc0042")
    print()
    
    verify_user_case()