#!/usr/bin/env python3
"""Test with INSERT far from SY-UNAME assignment"""

import sys
import os
import tempfile
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from csv_analyzer import EnhancedCSVAnalyzer
from encoding_utils import safe_file_read

def test_with_distance():
    """Test when INSERT is far from SY-UNAME"""
    
    # Create code with INSERT far from SY-UNAME
    code_lines = []
    
    # Add SY-UNAME assignment
    code_lines.append("* Start of program")
    code_lines.append("DATA: ptab TYPE TABLE OF zmc0042.")
    code_lines.append("")
    code_lines.append("ptab-ernam = sy-uname.")
    code_lines.append("ptab-erdat = sy-datum.")
    
    # Add many lines in between (simulating distance)
    for i in range(200):
        code_lines.append(f"* Line {i + 1} - some other code")
    
    # Add INSERT statement far away
    code_lines.append("* Finally insert the data")
    code_lines.append("insert zmc0042 from table ptab accepting duplicate keys.")
    code_lines.append("commit work.")
    
    code_content = '\n'.join(code_lines)
    
    # Create temp file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.abap', delete=False) as f:
        f.write(code_content)
        temp_file = f.name
    
    try:
        # Read the file
        lines, _ = safe_file_read(temp_file)
        
        # Find SY-UNAME line (should be around line 4)
        sy_uname_line = None
        for i, line in enumerate(lines):
            if 'sy-uname' in line.lower() and '=' in line:
                sy_uname_line = i
                print(f"✅ Found SY-UNAME at line {i + 1}")
                break
        
        # Find INSERT line
        insert_line = None
        for i, line in enumerate(lines):
            if 'insert zmc0042' in line.lower():
                insert_line = i
                print(f"✅ Found INSERT at line {i + 1}")
                break
        
        if insert_line and sy_uname_line:
            distance = insert_line - sy_uname_line
            print(f"📏 Distance between SY-UNAME and INSERT: {distance} lines")
        
        # Analyze with default context (150 lines after)
        analyzer = EnhancedCSVAnalyzer()
        analysis = analyzer.analyze_location(
            id_value="TEST_DISTANCE",
            file_path=temp_file,
            line_number=sy_uname_line,
            code_snippet=lines,
            actual_line_number=sy_uname_line + 1
        )
        
        # Check results
        print("\n📊 Analysis Results:")
        print(f"  Status: {analysis.status}")
        print(f"  Tables: {analysis.tables}")
        print(f"  Fields: {analysis.fields}")
        print(f"  Operations: {analysis.operations[:3] if len(analysis.operations) > 3 else analysis.operations}")
        
        # Check if INSERT was found
        tables_upper = [t.upper() for t in analysis.tables]
        fields_upper = [f.upper() for f in analysis.fields]
        
        if 'ZMC0042' in tables_upper:
            print("✅ Table ZMC0042 found despite distance")
        else:
            print("❌ Table ZMC0042 NOT found - distance too far!")
            print("   This might be the user's issue!")
        
        if 'ERNAM' in fields_upper:
            print("✅ Field ERNAM found")
        else:
            print("❌ Field ERNAM NOT found")
            
    finally:
        os.unlink(temp_file)

if __name__ == "__main__":
    print("🔍 Testing ABAP Pattern Extraction with Distance")
    print("=" * 50)
    test_with_distance()