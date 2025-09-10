#!/usr/bin/env python3
"""
Generate accurate CSV file with only actual SY-UNAME usage locations.
Excludes TYPE declarations and focuses on actual value assignments and comparisons.
"""

import os
import re
import csv
from pathlib import Path
from typing import List, Tuple, Dict

class AccurateCSVGenerator:
    """Generate CSV with only actual SY-UNAME usage (not type declarations)"""
    
    def __init__(self, input_dir: str = "input", output_file: str = "input/sy_uname_actual_usage.csv"):
        self.input_dir = Path(input_dir)
        self.output_file = Path(output_file)
        self.results = []
        
        # Patterns for ACTUAL usage (not type declarations)
        self.usage_patterns = [
            # Direct assignments
            (r'^\s*(\w+(?:-\w+)?)\s*=\s*sy-uname', 'assignment'),
            (r'^\s*(\w+(?:-\w+)?)\s*=.*\bsy-uname\b', 'assignment_expr'),
            
            # MOVE statements
            (r'^\s*MOVE\s+sy-uname\s+TO\s+', 'move_to'),
            (r'^\s*MOVE-CORRESPONDING.*sy-uname', 'move_corresponding'),
            
            # WHERE conditions in SQL
            (r'\bWHERE\s+.*\bsy-uname\b', 'where_clause'),
            (r'\bAND\s+.*\bsy-uname\b', 'and_clause'),
            (r'\bOR\s+.*\bsy-uname\b', 'or_clause'),
            
            # Comparisons
            (r'\bIF\s+.*\bsy-uname\b', 'if_condition'),
            (r'\bWHEN\s+.*\bsy-uname\b', 'when_condition'),
            (r'\bCHECK\s+.*\bsy-uname\b', 'check_condition'),
            
            # Function/method calls with SY-UNAME as parameter
            (r'CALL\s+FUNCTION.*\n*.*=\s*sy-uname', 'function_param'),
            (r'PERFORM\s+\w+\s+USING\s+sy-uname', 'perform_using'),
            
            # INSERT/UPDATE/MODIFY with direct values
            (r'INSERT\s+.*VALUES.*sy-uname', 'insert_values'),
            (r'UPDATE\s+.*SET.*=\s*sy-uname', 'update_set'),
            (r'MODIFY\s+.*FROM.*sy-uname', 'modify_from'),
            
            # Concatenation or string operations
            (r'CONCATENATE.*\bsy-uname\b', 'concatenate'),
            (r'\|.*\{\s*sy-uname\s*\}.*\|', 'string_template'),
            
            # WRITE statements
            (r'WRITE[:\s]+.*sy-uname', 'write_output'),
            
            # APPEND to internal tables
            (r'APPEND.*sy-uname.*TO\s+', 'append_to'),
            
            # SELECT statements with SY-UNAME in field list or WHERE
            (r'SELECT.*\bsy-uname\b.*FROM', 'select_field'),
            (r'SELECT.*WHERE.*\bsy-uname\b', 'select_where'),
        ]
        
        # Patterns to EXCLUDE (type declarations)
        self.exclude_patterns = [
            r'^\s*DATA:.*TYPE\s+sy-uname',  # DATA: var TYPE sy-uname
            r'^\s*DATA\s+.*TYPE\s+sy-uname',  # DATA var TYPE sy-uname
            r'^\s*DATA:.*LIKE\s+sy-uname',    # DATA: var LIKE sy-uname
            r'^\s*DATA\s+.*LIKE\s+sy-uname',  # DATA var LIKE sy-uname
            r'^\s*TYPES:.*TYPE\s+sy-uname',   # TYPES: type TYPE sy-uname
            r'^\s*TYPES\s+.*TYPE\s+sy-uname', # TYPES type TYPE sy-uname
            r'^\s*CONSTANTS:.*TYPE\s+sy-uname', # CONSTANTS with TYPE
            r'^\s*FIELD-SYMBOLS:.*TYPE\s+sy-uname', # FIELD-SYMBOLS
            r'^\s*CLASS-DATA:.*TYPE\s+sy-uname',    # CLASS-DATA
            r'^\s*PARAMETERS:.*TYPE\s+sy-uname',    # PARAMETERS
            r'^\s*SELECT-OPTIONS:.*FOR\s+sy-uname', # SELECT-OPTIONS
            
            # Method/function signatures
            r'^\s*METHODS:.*TYPE\s+sy-uname',
            r'^\s*IMPORTING.*TYPE\s+sy-uname',
            r'^\s*EXPORTING.*TYPE\s+sy-uname',
            r'^\s*CHANGING.*TYPE\s+sy-uname',
            r'^\s*RETURNING.*TYPE\s+sy-uname',
            r'^\s*USING.*TYPE\s+sy-uname',
            r'^\s*FORM\s+.*USING.*TYPE\s+sy-uname',
            
            # Comments
            r'^\s*\*.*sy-uname',  # Comment lines
            r'^\s*".*sy-uname',   # Comment lines with quotes
        ]
    
    def is_type_declaration(self, line: str) -> bool:
        """Check if line is a type declaration (should be excluded)"""
        line_upper = line.upper()
        
        # Quick checks first
        if 'TYPE SY-UNAME' in line_upper or 'LIKE SY-UNAME' in line_upper:
            # Check if it's not in a string or comment
            if not ('"' in line and 'SY-UNAME' in line.split('"')[0]):
                return True
        
        # Check exclude patterns
        for pattern in self.exclude_patterns:
            if re.match(pattern, line, re.IGNORECASE):
                return True
        
        return False
    
    def is_actual_usage(self, line: str) -> Tuple[bool, str]:
        """Check if line contains actual SY-UNAME usage"""
        # First check if it's a type declaration
        if self.is_type_declaration(line):
            return False, ""
        
        # Check for actual usage patterns
        for pattern, usage_type in self.usage_patterns:
            if re.search(pattern, line, re.IGNORECASE):
                return True, usage_type
        
        return False, ""
    
    def process_file(self, file_path: Path) -> List[Dict]:
        """Process a single ABAP file and find SY-UNAME usage"""
        results = []
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
        except UnicodeDecodeError:
            try:
                with open(file_path, 'r', encoding='cp949') as f:
                    lines = f.readlines()
            except:
                print(f"Warning: Could not read {file_path}")
                return results
        
        # Track multi-line statements
        in_data_declaration = False
        in_method_signature = False
        statement_buffer = ""
        statement_start_line = 0
        
        for i, line in enumerate(lines, 1):
            # Skip empty lines and comments
            if not line.strip() or line.strip().startswith('*'):
                continue
            
            # Handle multi-line DATA declarations
            if re.match(r'^\s*DATA:', line, re.IGNORECASE):
                in_data_declaration = True
                statement_buffer = line
                statement_start_line = i
                continue
            elif in_data_declaration:
                statement_buffer += " " + line.strip()
                if '.' in line:  # End of statement
                    in_data_declaration = False
                    # Check the complete statement
                    if 'SY-UNAME' in statement_buffer.upper() and not self.is_type_declaration(statement_buffer):
                        is_usage, usage_type = self.is_actual_usage(statement_buffer)
                        if is_usage:
                            results.append({
                                'line_number': statement_start_line,
                                'usage_type': usage_type,
                                'code': statement_buffer.strip()[:100]
                            })
                    statement_buffer = ""
                continue
            
            # Handle method signatures
            if re.match(r'^\s*(METHODS|FORM|FUNCTION):', line, re.IGNORECASE):
                in_method_signature = True
                statement_buffer = line
                statement_start_line = i
                continue
            elif in_method_signature:
                statement_buffer += " " + line.strip()
                if '.' in line:  # End of statement
                    in_method_signature = False
                    # Skip method signatures with TYPE sy-uname
                    statement_buffer = ""
                continue
            
            # Check single line for SY-UNAME usage
            if 'SY-UNAME' in line.upper():
                is_usage, usage_type = self.is_actual_usage(line)
                if is_usage:
                    results.append({
                        'line_number': i,
                        'usage_type': usage_type,
                        'code': line.strip()[:100]
                    })
        
        return results
    
    def generate_csv(self):
        """Generate CSV file with accurate SY-UNAME usage locations"""
        print("🔍 Scanning ABAP files for actual SY-UNAME usage...")
        print(f"📁 Input directory: {self.input_dir}")
        print("=" * 80)
        
        # Find all ABAP files
        abap_files = list(self.input_dir.glob("*.abap"))
        print(f"Found {len(abap_files)} ABAP files to analyze\n")
        
        all_results = []
        file_stats = {}
        
        for file_path in sorted(abap_files):
            file_results = self.process_file(file_path)
            
            if file_results:
                file_name = file_path.name
                file_stats[file_name] = len(file_results)
                
                print(f"✅ {file_name}: {len(file_results)} actual usage(s) found")
                
                # Add to results with file information
                for result in file_results:
                    all_results.append({
                        'file_path': file_name,
                        'line_number': result['line_number'],
                        'usage_type': result['usage_type'],
                        'code_snippet': result['code']
                    })
            else:
                print(f"⚪ {file_path.name}: No actual usage found")
        
        # Write to CSV
        print(f"\n{'='*80}")
        print(f"📝 Writing results to {self.output_file}")
        
        with open(self.output_file, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = ['id', 'file_path', 'line_number']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            
            for i, result in enumerate(all_results, 1):
                writer.writerow({
                    'id': i,
                    'file_path': result['file_path'],
                    'line_number': result['line_number']
                })
        
        # Print summary
        print(f"\n📊 Summary:")
        print(f"  Total files processed: {len(abap_files)}")
        print(f"  Files with actual usage: {len(file_stats)}")
        print(f"  Total actual usages found: {len(all_results)}")
        
        if file_stats:
            print(f"\n📈 Top files by usage count:")
            for file_name, count in sorted(file_stats.items(), key=lambda x: x[1], reverse=True)[:10]:
                print(f"    {file_name}: {count} usages")
        
        # Also create a detailed report for verification
        report_file = self.output_file.parent / "sy_uname_usage_report.txt"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("SY-UNAME Actual Usage Report\n")
            f.write("=" * 80 + "\n\n")
            
            current_file = ""
            for result in all_results:
                if result['file_path'] != current_file:
                    current_file = result['file_path']
                    f.write(f"\n{current_file}\n")
                    f.write("-" * 40 + "\n")
                
                f.write(f"  Line {result['line_number']:4d} [{result['usage_type']:20s}]: {result['code_snippet']}\n")
        
        print(f"\n📄 Detailed report saved to: {report_file}")
        print(f"✅ CSV generation complete: {self.output_file}")


def main():
    """Main entry point"""
    generator = AccurateCSVGenerator()
    generator.generate_csv()


if __name__ == "__main__":
    main()