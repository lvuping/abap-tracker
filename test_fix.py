#!/usr/bin/env python3
"""Test script to verify the ABAP pattern extraction"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from csv_analyzer import EnhancedCSVAnalyzer
from encoding_utils import safe_file_read

def test_pattern_extraction():
    """Test the specific ABAP pattern extraction"""
    
    # Read the test file
    test_file = "test_case.abap"
    lines, _ = safe_file_read(test_file)
    
    # Initialize analyzer
    analyzer = EnhancedCSVAnalyzer()
    
    # Find SY-UNAME line
    sy_uname_line = None
    for i, line in enumerate(lines):
        if 'ptab-ernam = sy-uname' in line.lower():
            sy_uname_line = i
            break
    
    if sy_uname_line is None:
        print("❌ Could not find SY-UNAME assignment line")
        return False
    
    print(f"✅ Found SY-UNAME at line {sy_uname_line + 1}")
    
    # Analyze the location
    analysis = analyzer.analyze_location(
        id_value="TEST001",
        file_path=test_file,
        line_number=sy_uname_line,
        code_snippet=lines,
        actual_line_number=sy_uname_line + 1
    )
    
    # Check results
    print("\n📊 Analysis Results:")
    print(f"  Status: {analysis.status}")
    print(f"  Tables: {analysis.tables}")
    print(f"  Fields: {analysis.fields}")
    print(f"  Operations: {analysis.operations}")
    print(f"  Keywords: {analysis.keywords}")
    print(f"  Confidence: {analysis.confidence}")
    print(f"  Tainted Variables: {analysis.tainted_variables}")
    
    # Verify expected results
    success = True
    
    if 'ZMC0042' not in [t.upper() for t in analysis.tables]:
        print("❌ Failed: Table ZMC0042 not found")
        success = False
    else:
        print("✅ Table ZMC0042 found")
    
    if 'ERNAM' not in [f.upper() for f in analysis.fields]:
        print("❌ Failed: Field ERNAM not found")
        success = False
    else:
        print("✅ Field ERNAM found")
    
    if not any('INSERT' in op.upper() for op in analysis.operations):
        print("❌ Failed: INSERT operation not found")
        success = False
    else:
        print("✅ INSERT operation found")
    
    return success

if __name__ == "__main__":
    print("🔍 Testing ABAP Pattern Extraction")
    print("=" * 50)
    
    if test_pattern_extraction():
        print("\n✅ TEST PASSED")
    else:
        print("\n❌ TEST FAILED")