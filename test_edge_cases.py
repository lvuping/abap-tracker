#!/usr/bin/env python3
"""Test edge cases for ABAP pattern extraction"""

import sys
import os
import tempfile
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from csv_analyzer import EnhancedCSVAnalyzer
from encoding_utils import safe_file_read

def test_case(name, code_content, expected_table, expected_field, expected_operation):
    """Test a specific ABAP code case"""
    
    # Create temp file with the code
    with tempfile.NamedTemporaryFile(mode='w', suffix='.abap', delete=False) as f:
        f.write(code_content)
        temp_file = f.name
    
    try:
        # Read the file
        lines, _ = safe_file_read(temp_file)
        
        # Find SY-UNAME line
        sy_uname_line = None
        for i, line in enumerate(lines):
            if 'sy-uname' in line.lower() and '=' in line:
                sy_uname_line = i
                break
        
        if sy_uname_line is None:
            print(f"❌ {name}: Could not find SY-UNAME assignment")
            return False
        
        # Analyze
        analyzer = EnhancedCSVAnalyzer()
        analysis = analyzer.analyze_location(
            id_value=f"TEST_{name}",
            file_path=temp_file,
            line_number=sy_uname_line,
            code_snippet=lines,
            actual_line_number=sy_uname_line + 1
        )
        
        # Check results
        success = True
        tables_upper = [t.upper() for t in analysis.tables]
        fields_upper = [f.upper() for f in analysis.fields]
        ops_upper = [op.upper() for op in analysis.operations]
        
        if expected_table and expected_table.upper() not in tables_upper:
            print(f"❌ {name}: Table {expected_table} not found. Found: {analysis.tables}")
            success = False
        
        if expected_field and expected_field.upper() not in fields_upper:
            print(f"❌ {name}: Field {expected_field} not found. Found: {analysis.fields}")
            success = False
        
        if expected_operation and not any(expected_operation.upper() in op for op in ops_upper):
            print(f"❌ {name}: Operation {expected_operation} not found. Found: {analysis.operations}")
            success = False
        
        if success:
            print(f"✅ {name}: All expected patterns found")
            
        return success
        
    finally:
        os.unlink(temp_file)

def run_tests():
    """Run all test cases"""
    
    all_passed = True
    
    # Test Case 1: Original user's case
    case1 = """ptab-ernam = sy-uname.
ptab-erdat = sy-datum.
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
    
    if not test_case("Case1_UserOriginal", case1, "ZMC0042", "ERNAM", "INSERT"):
        all_passed = False
    
    # Test Case 2: Direct INSERT VALUES
    case2 = """DATA: lv_user TYPE string.
lv_user = sy-uname.
INSERT INTO zusertab VALUES ( '001', lv_user, sy-datum )."""
    
    if not test_case("Case2_DirectInsert", case2, "ZUSERTAB", None, "INSERT"):
        all_passed = False
    
    # Test Case 3: UPDATE with structure
    case3 = """wa_user-username = sy-uname.
wa_user-date = sy-datum.
UPDATE zuserlog FROM wa_user."""
    
    if not test_case("Case3_UpdateStructure", case3, "ZUSERLOG", "USERNAME", "UPDATE"):
        all_passed = False
    
    # Test Case 4: VALUE constructor
    case4 = """INSERT ztable FROM @( VALUE #( 
    field1 = 'TEST'
    created_by = sy-uname
    created_on = sy-datum ) )."""
    
    if not test_case("Case4_ValueConstructor", case4, "ZTABLE", "CREATED_BY", "INSERT"):
        all_passed = False
    
    # Test Case 5: INSERT with internal table
    case5 = """DATA: itab TYPE TABLE OF ztest.
FIELD-SYMBOLS: <fs> TYPE ztest.
APPEND INITIAL LINE TO itab ASSIGNING <fs>.
<fs>-ernam = sy-uname.
<fs>-erdat = sy-datum.
INSERT ztest FROM TABLE itab."""
    
    if not test_case("Case5_InternalTable", case5, "ZTEST", None, "INSERT"):
        all_passed = False
    
    # Test Case 6: Simple structure assignment and insert
    case6 = """DATA: ls_data TYPE zmc0042.
ls_data-ernam = sy-uname.
ls_data-erdat = sy-datum.
INSERT zmc0042 FROM ls_data."""
    
    if not test_case("Case6_SimpleStructure", case6, "ZMC0042", "ERNAM", "INSERT"):
        all_passed = False
    
    # Test Case 7: MODIFY statement
    case7 = """ptab-changed_by = sy-uname.
ptab-changed_on = sy-datum.
MODIFY ztable FROM ptab."""
    
    if not test_case("Case7_Modify", case7, "ZTABLE", "CHANGED_BY", "MODIFY"):
        all_passed = False
    
    return all_passed

if __name__ == "__main__":
    print("🔍 Testing ABAP Pattern Extraction Edge Cases")
    print("=" * 50)
    
    if run_tests():
        print("\n✅ ALL TESTS PASSED")
    else:
        print("\n❌ SOME TESTS FAILED")