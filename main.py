#!/usr/bin/env python3
"""
ABAP Tracker - Enhanced SY-UNAME Analysis Tool
Comprehensive CSV processing with keyword, table, field, and RFC extraction

Usage:
    python main.py analyze                    # Analyze default CSV
    python main.py analyze input/test.csv     # Analyze specific CSV
    python main.py test                        # Run test suite
    python main.py report                      # Generate analysis report
    python main.py --help                      # Show help
"""

import os
import sys
import json
import csv
import argparse
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import asdict

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

# Import project modules
from csv_analyzer import EnhancedCSVAnalyzer, ComprehensiveAnalysis
from encoding_utils import safe_file_read
from unified_analyzer import UnifiedAnalyzer


class ABAPTracker:
    """Enhanced ABAP SY-UNAME Tracker with comprehensive analysis"""
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.results = []
        self.analyzer = EnhancedCSVAnalyzer()
        
        # Setup directories
        self.input_dir = Path("input")
        self.output_dir = Path("output")
        self.test_dir = Path("test")
        self.src_dir = Path("src")
        
        # Ensure directories exist
        for dir_path in [self.output_dir]:
            dir_path.mkdir(exist_ok=True)
        
    def analyze_csv(self, input_file: str = None) -> bool:
        """
        Analyze CSV file with enhanced SY-UNAME tracking
        Extracts keywords, tables, fields, RFC information
        
        CSV format:
            id,file_path,line_number
            1,example.abap,45
            2,another.abap,123
        """
        
        # Default input file
        if not input_file:
            input_file = str(self.input_dir / "sy_uname_locations.csv")
        
        input_path = Path(input_file)
        if not input_path.exists():
            self._print_error(f"Input file not found: {input_file}")
            self._print_csv_format_help()
            return False
        
        print(f"\n🔍 Starting Enhanced SY-UNAME Analysis")
        print(f"📁 Input file: {input_file}")
        print("=" * 80)
        
        # CSV 파일 읽기
        try:
            lines, encoding = safe_file_read(str(input_path))
            if self.verbose:
                print(f"📝 File encoding: {encoding}")
            
            # CSV 파싱
            import io
            csv_content = io.StringIO(''.join(lines))
            reader = csv.DictReader(csv_content)
            
            # Process rows
            rows = list(reader)
            total = len(rows)
            
            if total == 0:
                self._print_warning("No data to process")
                return False
            
            print(f"📊 Found {total} entries to analyze\n")
            
            # Analyze each entry
            for idx, row in enumerate(rows, 1):
                result = self._analyze_entry(row, idx, total)
                if result:
                    self.results.append(result)
                
                # Progress indicator
                if idx % 10 == 0:
                    print(f"  Progress: {idx}/{total} analyzed...")
            
        except Exception as e:
            self._print_error(f"Error processing CSV: {str(e)}")
            return False
        
        # Save results
        if self.results:
            self.save_results()
        
        return True
    
    def _analyze_entry(self, row: Dict, idx: int, total: int) -> Optional[ComprehensiveAnalysis]:
        """Analyze single CSV entry with comprehensive extraction"""
        try:
            # Extract ID, file path, and line number
            id_value = row.get('id', str(idx))
            file_path = row.get('file_path', '').strip()
            line_number = int(row.get('line_number', 0))
            
            # Normalize path
            if file_path and not file_path.endswith('.abap'):
                file_path += '.abap'
            
            # Check if path is relative
            if not os.path.isabs(file_path):
                file_path = str(self.input_dir / file_path)
            
            print(f"📍 [{idx}/{total}] {os.path.basename(file_path)} (line {line_number})")
            
            # Check file existence
            if not os.path.exists(file_path):
                self._print_error(f"   File not found: {file_path}", indent=True)
                return None
            
            # Read file content
            lines, _ = safe_file_read(file_path)
            
            # Extract analysis context - asymmetric range (less above, more below)
            # Above: UPDATE, CALL RFC, colon syntax etc. (30 lines sufficient)
            # Below: Need to trace the actual usage in DB operations (150 lines)
            start = max(0, line_number - 30)  # 30 lines before for context
            end = min(len(lines), line_number + 150)  # 150 lines after to find first DB operation
            snippet = lines[start:end]
            
            # Perform comprehensive analysis
            analysis = self.analyzer.analyze_location(
                id_value=id_value,
                file_path=file_path,
                line_number=line_number - start - 1,  # 0-based relative line number
                code_snippet=snippet,
                actual_line_number=line_number  # Pass the actual line number from CSV
            )
            
            # Print summary
            self._print_analysis_summary(analysis)
            
            return analysis
            
        except Exception as e:
            self._print_error(f"   Analysis error: {str(e)}", indent=True)
            return None
    
    def save_results(self):
        """Save analysis results to CSV and JSON"""
        if not self.results:
            print("No results to save")
            return
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save as CSV
        csv_file = self.output_dir / f"analysis_{timestamp}.csv"
        self._save_csv(str(csv_file))
        
        # Save as JSON
        json_file = self.output_dir / f"analysis_{timestamp}.json"
        self._save_json(str(json_file))
        
        print("\n" + "=" * 80)
        print("✅ Analysis complete!")
        print(f"📁 Results saved:")
        print(f"   CSV: {csv_file}")
        print(f"   JSON: {json_file}")
        
        # Print summary
        self._print_summary()
    
    def _save_json(self, filename: str):
        """Save results as JSON"""
        output = {
            'timestamp': datetime.now().isoformat(),
            'total_analyzed': len(self.results),
            'results': [asdict(r) if hasattr(r, '__dict__') else r for r in self.results]
        }
        
        with open(filename, 'w', encoding='utf-8') as f:
            json.dump(output, f, ensure_ascii=False, indent=2)
    
    def _save_csv(self, filename: str):
        """Save results as comprehensive CSV"""
        if not self.results:
            return
        
        # Get fieldnames from first result
        first_result = self.results[0]
        if isinstance(first_result, ComprehensiveAnalysis):
            fieldnames = list(first_result.to_csv_row().keys())
        else:
            fieldnames = ['ID', 'Source_File', 'Line_Number', 'Status', 'Description']
        
        with open(filename, 'w', newline='', encoding='utf-8-sig') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for result in self.results:
                if isinstance(result, ComprehensiveAnalysis):
                    row = result.to_csv_row()
                else:
                    row = {k: result.get(k, '') for k in fieldnames}
                writer.writerow(row)
    
    def _print_analysis_summary(self, analysis: ComprehensiveAnalysis):
        """Print analysis summary"""
        if analysis.status == 'Success' or analysis.status == 'Complete':
            print(f"   ✅ {analysis.status}")
            
            if analysis.tables:
                print(f"      Tables: {', '.join(analysis.tables[:3])}")
            
            if analysis.rfc_functions:
                print(f"      RFC: {', '.join(analysis.rfc_functions[:2])}")
            
            if analysis.keywords:
                print(f"      Keywords: {', '.join(analysis.keywords[:5])}")
            
            print(f"      Confidence: {analysis.confidence:.2f}")
        elif analysis.status == 'Error':
            print(f"   ❌ {analysis.status}")
        elif analysis.status == 'Partial':
            print(f"   ⚠️  {analysis.status}")
        else:
            print(f"   ❓ {analysis.status}")
    
    def _print_summary(self):
        """Print analysis summary"""
        if not self.results:
            return
        
        total = len(self.results)
        successful = sum(1 for r in self.results if r.status == 'Success')
        with_tables = sum(1 for r in self.results if r.tables)
        with_rfc = sum(1 for r in self.results if r.rfc_functions)
        high_confidence = sum(1 for r in self.results if r.confidence > 0.7)
        
        print("\n📊 Analysis Summary:")
        print(f"  Total analyzed: {total}")
        print(f"  Successful: {successful} ({successful/total*100:.1f}%)")
        print(f"  With DB operations: {with_tables}")
        print(f"  With RFC calls: {with_rfc}")
        print(f"  High confidence (>0.7): {high_confidence}")
    
    def _print_csv_format_help(self):
        """Print CSV format help"""
        print("\n📋 Expected CSV format:")
        print("   id,file_path,line_number")
        print("   1,example_file.abap,45")
        print("   2,another_file.abap,123")
        print("\nNote: id column is optional")
    
    def _print_error(self, msg: str, indent: bool = False):
        """Print error message"""
        prefix = "   " if indent else ""
        print(f"{prefix}❌ {msg}")
    
    def _print_warning(self, msg: str, indent: bool = False):
        """Print warning message"""
        prefix = "   " if indent else ""
        print(f"{prefix}⚠️  {msg}")
    
    def run_tests(self) -> bool:
        """Run test suite"""
        print("🧪 Running Test Suite")
        print("=" * 80)
        
        test_files = list(self.test_dir.glob("*.abap"))
        if not test_files:
            print("No test files found")
            return False
        
        passed = 0
        failed = 0
        
        for test_file in test_files[:10]:  # Limit to 10 for demo
            print(f"\nTesting: {test_file.name}")
            
            try:
                lines, _ = safe_file_read(str(test_file))
                
                # Find SY-UNAME
                sy_uname_lines = []
                for i, line in enumerate(lines):
                    if 'SY-UNAME' in line.upper():
                        sy_uname_lines.append(i + 1)
                
                if sy_uname_lines:
                    # Analyze first occurrence
                    analysis = self.analyzer.analyze_location(
                        id_value="TEST",
                        file_path=str(test_file),
                        line_number=sy_uname_lines[0] - 1,  # Convert to 0-based
                        code_snippet=lines,
                        actual_line_number=sy_uname_lines[0]  # Pass actual line number
                    )
                    
                    if analysis.status in ['Success', 'Partial']:
                        print(f"  ✅ Pass - {len(analysis.tainted_variables)} variables tracked")
                        passed += 1
                    else:
                        print(f"  ❌ Fail - {analysis.status}")
                        failed += 1
                else:
                    print(f"  ⚠️  Skip - No SY-UNAME found")
                    
            except Exception as e:
                print(f"  ❌ Error: {e}")
                failed += 1
        
        print(f"\n{'='*80}")
        print(f"Test Results: {passed} passed, {failed} failed")
        
        return failed == 0
    
    def generate_report(self) -> bool:
        """Generate analysis report"""
        print("📈 Generating Report")
        print("=" * 80)
        
        # Find latest results
        csv_files = list(self.output_dir.glob("analysis_*.csv"))
        if not csv_files:
            print("No analysis results found. Run 'analyze' first.")
            return False
        
        latest = max(csv_files, key=lambda p: p.stat().st_mtime)
        
        print(f"Using: {latest}")
        
        # Read and process results
        with open(latest, 'r', encoding='utf-8-sig') as f:
            reader = csv.DictReader(f)
            rows = list(reader)
        
        # Generate report content
        report = self._generate_report_content(rows)
        
        # Save report
        report_file = self.output_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(report)
        
        print(report)
        print(f"\n📄 Report saved: {report_file}")
        
        return True
    
    def _generate_report_content(self, rows: List[Dict]) -> str:
        """Generate report content"""
        lines = []
        lines.append("=" * 80)
        lines.append("ABAP SY-UNAME TRACKER - ANALYSIS REPORT")
        lines.append("=" * 80)
        lines.append(f"Generated: {datetime.now().isoformat()}")
        lines.append("")
        
        # Summary
        total = len(rows)
        successful = sum(1 for r in rows if r.get('Status') == 'Success')
        
        lines.append("SUMMARY")
        lines.append("-" * 40)
        lines.append(f"Total analyzed: {total}")
        lines.append(f"Successful: {successful} ({successful/total*100:.1f}%)")
        lines.append("")
        
        # Top tables
        tables = {}
        for row in rows:
            if row.get('Tables'):
                for table in row['Tables'].split(','):
                    table = table.strip()
                    if table:
                        tables[table] = tables.get(table, 0) + 1
        
        if tables:
            lines.append("TOP TABLES")
            lines.append("-" * 40)
            for table, count in sorted(tables.items(), key=lambda x: x[1], reverse=True)[:10]:
                lines.append(f"  {table}: {count} occurrences")
            lines.append("")
        
        # Top RFC functions
        rfcs = {}
        for row in rows:
            if row.get('RFC_Functions'):
                for rfc in row['RFC_Functions'].split(','):
                    rfc = rfc.strip()
                    if rfc:
                        rfcs[rfc] = rfcs.get(rfc, 0) + 1
        
        if rfcs:
            lines.append("TOP RFC FUNCTIONS")
            lines.append("-" * 40)
            for rfc, count in sorted(rfcs.items(), key=lambda x: x[1], reverse=True)[:10]:
                lines.append(f"  {rfc}: {count} calls")
            lines.append("")
        
        return "\n".join(lines)


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description='ABAP Tracker - Enhanced SY-UNAME Analysis Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python main.py analyze                    # Analyze default CSV
  python main.py analyze input/test.csv     # Analyze specific CSV
  python main.py test                        # Run test suite
  python main.py report                      # Generate analysis report

CSV format:
  id,file_path,line_number
  1,example.abap,45
  2,another.abap,123
        """
    )
    
    # Subcommands
    subparsers = parser.add_subparsers(dest='command', help='Commands')
    
    # Analyze command
    analyze_parser = subparsers.add_parser('analyze', help='Analyze CSV file')
    analyze_parser.add_argument('csv_file', nargs='?', help='CSV file to analyze')
    analyze_parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    
    # Test command
    test_parser = subparsers.add_parser('test', help='Run test suite')
    test_parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    
    # Report command
    report_parser = subparsers.add_parser('report', help='Generate analysis report')
    
    args = parser.parse_args()
    
    # Initialize tracker
    tracker = ABAPTracker(verbose=getattr(args, 'verbose', False))
    
    # Execute command
    success = False
    
    if args.command == 'analyze':
        success = tracker.analyze_csv(args.csv_file)
    elif args.command == 'test':
        success = tracker.run_tests()
    elif args.command == 'report':
        success = tracker.generate_report()
    else:
        parser.print_help()
        sys.exit(1)
    
    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()