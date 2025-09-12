#!/usr/bin/env python3
"""
Test script for Ultimate Database Handler
Tests ALL INSERT, UPDATE, MODIFY patterns
"""

import sys
import os
from pathlib import Path
from typing import List, Dict, Tuple
from dataclasses import dataclass

# Add parent directory
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from src.complete_db_handler import CompleteDBHandler, OperationType

@dataclass 
class TestPattern:
    """Test pattern definition"""
    id: str
    operation: str
    description: str
    start_line: int
    end_line: int
    expected_table: str
    should_find_syuname: bool

class UltimateTest:
    """Test runner for ultimate handler"""
    
    def __init__(self):
        self.handler = CompleteDBHandler()
        self.test_file = "test/all_abap_patterns.abap"
        self.patterns = self._define_patterns()
    
    def _define_patterns(self) -> List[TestPattern]:
        """Define all test patterns with line numbers"""
        patterns = [
            # INSERT patterns
            TestPattern("I01", "INSERT", "Single record from structure", 18, 21, "ZTABLE", True),
            TestPattern("I02", "INSERT", "Single record with @", 25, 26, "ZTABLE", True),
            TestPattern("I03", "INSERT", "INSERT INTO syntax", 30, 30, "ZTABLE", True),
            TestPattern("I04", "INSERT", "Direct VALUES with literals", 34, 34, "ZTABLE", True),
            TestPattern("I05", "INSERT", "Multiple VALUES rows", 38, 41, "ZTABLE", True),
            TestPattern("I06", "INSERT", "FROM TABLE", 45, 45, "ZTABLE", False),
            TestPattern("I07", "INSERT", "ACCEPTING DUPLICATE KEYS", 49, 49, "ZTABLE", False),
            TestPattern("I08", "INSERT", "VALUE constructor inline", 53, 53, "ZTABLE", True),
            TestPattern("I09", "INSERT", "VALUE with type", 57, 57, "ZTABLE", True),
            TestPattern("I10", "INSERT", "VALUE with @ and parentheses", 61, 64, "ZTABLE", True),
            TestPattern("I11", "INSERT", "LINES OF internal table", 68, 68, "ZTABLE", False),
            TestPattern("I12", "INSERT", "INSERT with INDEX", 72, 72, "LT_TAB", False),
            TestPattern("I13", "INSERT", "INSERT INTO TABLE", 76, 76, "LT_TAB", False),
            TestPattern("I14", "INSERT", "Chain statement", 80, 81, "ZTABLE", True),
            TestPattern("I15", "INSERT", "INITIAL LINE", 85, 85, "LT_TAB", False),
            TestPattern("I16", "INSERT", "ASSIGNING field symbol", 89, 91, "LT_TAB", True),
            TestPattern("I17", "INSERT", "REFERENCE INTO", 95, 97, "LT_TAB", True),
            TestPattern("I18", "INSERT", "CLIENT SPECIFIED", 101, 105, "ZTABLE", True),
            TestPattern("I19", "INSERT", "CORRESPONDING", 109, 111, "ZTABLE", True),
            TestPattern("I20", "INSERT", "CONV type conversion", 115, 115, "ZTABLE", False),
            TestPattern("I21", "INSERT", "Subquery/nested", 119, 119, "ZTABLE", True),
            TestPattern("I23", "INSERT", "VALUE with BASE", 131, 134, "ZTABLE", True),
            TestPattern("I24", "INSERT", "REDUCE operator", 138, 143, "ZTABLE", True),
            TestPattern("I25", "INSERT", "COND operator", 147, 150, "ZTABLE", True),
            
            # UPDATE patterns
            TestPattern("U01", "UPDATE", "Basic SET", 158, 158, "ZTABLE", True),
            TestPattern("U02", "UPDATE", "SET with WHERE", 162, 163, "ZTABLE", True),
            TestPattern("U03", "UPDATE", "Multiple SET fields", 167, 169, "ZTABLE", True),
            TestPattern("U04", "UPDATE", "Multi-line SET", 173, 177, "ZTABLE", True),
            TestPattern("U05", "UPDATE", "FROM structure", 181, 182, "ZTABLE", True),
            TestPattern("U06", "UPDATE", "FROM @structure", 186, 186, "ZTABLE", True),
            TestPattern("U07", "UPDATE", "CLIENT SPECIFIED", 190, 192, "ZTABLE", True),
            TestPattern("U08", "UPDATE", "SET with subquery", 196, 198, "ZTABLE", True),
            TestPattern("U09", "UPDATE", "Complex WHERE", 202, 204, "ZTABLE", True),
            TestPattern("U10", "UPDATE", "SET with CASE", 208, 214, "ZTABLE", True),
            TestPattern("U11", "UPDATE", "SET with calculations", 218, 220, "ZTABLE", True),
            TestPattern("U12", "UPDATE", "Nested parentheses", 224, 225, "ZTABLE", True),
            TestPattern("U13", "UPDATE", "UPDATE with JOIN", 229, 233, "ZTABLE", True),
            TestPattern("U14", "UPDATE", "UPDATE with EXISTS", 237, 239, "ZTABLE", True),
            TestPattern("U15", "UPDATE", "SET = subquery", 243, 246, "ZTABLE", False),
            TestPattern("U16", "UPDATE", "UPDATE internal table", 250, 251, "LT_TAB", True),
            TestPattern("U17", "UPDATE", "UPDATE TRANSPORTING", 255, 256, "LT_TAB", True),
            TestPattern("U18", "UPDATE", "Inline SQL with @", 260, 261, "ZTABLE", True),
            TestPattern("U20", "UPDATE", "Chain UPDATE", 269, 270, "ZTABLE", True),
            
            # MODIFY patterns
            TestPattern("M01", "MODIFY", "Basic FROM structure", 278, 279, "ZTABLE", True),
            TestPattern("M02", "MODIFY", "FROM @structure", 283, 283, "ZTABLE", True),
            TestPattern("M03", "MODIFY", "FROM TABLE", 287, 287, "ZTABLE", False),
            TestPattern("M04", "MODIFY", "TRANSPORTING fields", 291, 293, "ZTABLE", True),
            TestPattern("M05", "MODIFY", "TRANSPORTING WHERE", 297, 298, "LT_TAB", True),
            TestPattern("M06", "MODIFY", "By INDEX", 302, 303, "LT_TAB", True),
            TestPattern("M07", "MODIFY", "INDEX TRANSPORTING", 307, 307, "LT_TAB", True),
            TestPattern("M08", "MODIFY", "MODIFY TABLE sorted", 311, 311, "LT_TAB", True),
            TestPattern("M09", "MODIFY", "TABLE TRANSPORTING", 315, 315, "LT_TAB", True),
            TestPattern("M10", "MODIFY", "CLIENT SPECIFIED", 319, 323, "ZTABLE", True),
            TestPattern("M11", "MODIFY", "VALUE constructor", 327, 329, "ZTABLE", True),
            TestPattern("M12", "MODIFY", "VALUE with type", 333, 334, "ZTABLE", True),
            TestPattern("M13", "MODIFY", "LINES OF", 338, 342, "LT_TAB", True),
            TestPattern("M14", "MODIFY", "Field symbol loop", 346, 348, "LT_TAB", True),
            TestPattern("M15", "MODIFY", "Reference loop", 352, 354, "LT_TAB", True),
            TestPattern("M16", "MODIFY", "Dynamic table", 358, 360, "ZTABLE", True),
            TestPattern("M17", "MODIFY", "CORRESPONDING", 364, 364, "ZTABLE", True),
            TestPattern("M18", "MODIFY", "With COND", 368, 372, "ZTABLE", True),
            TestPattern("M19", "MODIFY", "With REDUCE", 376, 384, "ZTABLE", True),
            TestPattern("M20", "MODIFY", "Chain MODIFY", 388, 389, "ZTABLE", True),
            TestPattern("M24", "MODIFY", "With BASE", 409, 411, "ZTABLE", True),
        ]
        return patterns
    
    def load_file(self) -> List[str]:
        """Load test file"""
        with open(self.test_file, 'r', encoding='utf-8') as f:
            return f.readlines()
    
    def run_tests(self) -> Tuple[int, int, int]:
        """Run all tests"""
        lines = self.load_file()
        passed = 0
        failed = 0
        total = len(self.patterns)
        
        print(f"\n{'='*80}")
        print(f"ULTIMATE DATABASE HANDLER TEST")
        print(f"{'='*80}\n")
        
        # Group by operation
        for op_type in ["INSERT", "UPDATE", "MODIFY"]:
            print(f"\n{op_type} PATTERNS")
            print("-" * 40)
            
            for pattern in self.patterns:
                if pattern.operation != op_type:
                    continue
                
                # Analyze pattern
                ops = self.handler.analyze(lines, pattern.start_line - 1, pattern.end_line)
                
                # Check results
                found = False
                correct_table = False
                has_syuname = False
                
                for op in ops:
                    if op.operation.value == pattern.operation:
                        found = True
                        if op.table == pattern.expected_table:
                            correct_table = True
                        if op.has_sy_uname:
                            has_syuname = True
                        break
                
                # Determine pass/fail
                if found and correct_table:
                    if pattern.should_find_syuname:
                        if has_syuname:
                            status = "✓ PASS"
                            passed += 1
                        else:
                            status = "⚠ PARTIAL"
                            failed += 1
                    else:
                        status = "✓ PASS"
                        passed += 1
                else:
                    status = "✗ FAIL"
                    failed += 1
                
                # Print result
                print(f"{status} {pattern.id}: {pattern.description}")
                if found:
                    print(f"     Table: {ops[0].table if ops else 'N/A'}, "
                          f"SY-UNAME: {'YES' if has_syuname else 'NO'}, "
                          f"Pattern: {ops[0].pattern if ops else 'N/A'}")
                else:
                    print(f"     NOT DETECTED")
        
        # Summary
        print(f"\n{'='*80}")
        print(f"SUMMARY")
        print(f"{'='*80}")
        print(f"Total: {total}")
        print(f"Passed: {passed} ({passed/total*100:.1f}%)")
        print(f"Failed: {failed} ({failed/total*100:.1f}%)")
        
        return passed, failed, total
    
    def run_specific_test(self, pattern_id: str):
        """Run specific test for debugging"""
        lines = self.load_file()
        
        for pattern in self.patterns:
            if pattern.id == pattern_id:
                print(f"\nTesting {pattern_id}: {pattern.description}")
                print(f"Lines {pattern.start_line}-{pattern.end_line}")
                print("-" * 40)
                
                # Show code
                for i in range(pattern.start_line - 1, pattern.end_line):
                    print(f"{i+1:4}: {lines[i].rstrip()}")
                
                print("-" * 40)
                
                # Analyze
                ops = self.handler.analyze(lines, pattern.start_line - 1, pattern.end_line)
                
                if ops:
                    for op in ops:
                        print(f"Found: {op.operation.value}")
                        print(f"Table: {op.table}")
                        print(f"Fields: {op.fields}")
                        print(f"Pattern: {op.pattern}")
                        print(f"Has SY-UNAME: {op.has_sy_uname}")
                else:
                    print("No operations detected")
                
                return
        
        print(f"Pattern {pattern_id} not found")

def main():
    """Main entry point"""
    tester = UltimateTest()
    
    if len(sys.argv) > 1:
        # Test specific pattern
        tester.run_specific_test(sys.argv[1])
    else:
        # Run all tests
        passed, failed, total = tester.run_tests()
        
        # Exit code
        sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()