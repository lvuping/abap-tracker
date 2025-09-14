#!/usr/bin/env python3
"""
ABAP Tracker - Enhanced SY-UNAME Analysis Tool with Advanced Pattern Detection
Comprehensive CSV processing with context-aware taint tracking and performance optimizations

Usage:
    python main.py analyze                           # Analyze default CSV with standard mode
    python main.py analyze --enhanced                # Use enhanced analyzer with optimizations
    python main.py analyze input/test.csv            # Analyze specific CSV
    python main.py analyze --enhanced --window 100   # Custom window size
    python main.py test                              # Run test suite
    python main.py report                            # Generate analysis report
    python main.py benchmark                         # Run performance benchmark
    python main.py --help                            # Show help
"""

import os
import sys
import json
import csv
import argparse
import time
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import asdict

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

# Import project modules
from core.csv_analyzer import EnhancedCSVAnalyzer, ComprehensiveAnalysis
from utils.encoding_utils import safe_file_read
from core.unified_analyzer import UnifiedAnalyzer

# Import enhanced modules
try:
    from core.enhanced_analyzer import EnhancedABAPAnalyzer
    from patterns.advanced_patterns import AdvancedPatternDetector
    from utils.context_aware_taint import ContextAwareTaintTracker
    ENHANCED_MODE_AVAILABLE = True
except ImportError:
    ENHANCED_MODE_AVAILABLE = False
    print("⚠️  Enhanced mode modules not found. Using standard analyzer.")


class ABAPTracker:
    """Enhanced ABAP SY-UNAME Tracker with comprehensive analysis"""

    def __init__(self, verbose: bool = False, enhanced: bool = False,
                 window_size: int = 50, enable_cache: bool = True):
        self.verbose = verbose
        self.enhanced = enhanced and ENHANCED_MODE_AVAILABLE
        self.results = []

        # Initialize analyzer based on mode
        if self.enhanced:
            self.analyzer = EnhancedABAPAnalyzer(
                enable_caching=enable_cache,
                enable_sliding_window=True,
                window_size=window_size
            )
            print("🚀 Using Enhanced Analyzer with advanced pattern detection")
        else:
            self.analyzer = EnhancedCSVAnalyzer()
            print("📝 Using Standard Analyzer")

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
        Analyze CSV file with enhanced or standard SY-UNAME tracking

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

        print(f"\n🔍 Starting {'Enhanced' if self.enhanced else 'Standard'} SY-UNAME Analysis")
        print(f"📁 Input file: {input_file}")
        print("=" * 80)

        if self.enhanced:
            # Use enhanced analyzer
            return self._analyze_enhanced(input_path)
        else:
            # Use standard analyzer
            return self._analyze_standard(input_path)

    def _analyze_enhanced(self, input_path: Path) -> bool:
        """Analyze with enhanced analyzer"""
        try:
            # Generate timestamp for output
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_json = self.output_dir / f"analysis_{timestamp}.json"
            output_csv = self.output_dir / f"analysis_{timestamp}.csv"

            # Run enhanced analysis
            print("\n🔬 Running enhanced analysis with:")
            print(f"  • Advanced pattern detection (EXEC SQL, CDS, BAPI, etc.)")
            print(f"  • Context-aware taint tracking")
            print(f"  • Performance optimizations")
            print(f"  • Sliding window: {self.analyzer.sliding_window.window_size if self.analyzer.sliding_window else 'Disabled'}")

            start_time = time.time()
            results, metrics = self.analyzer.analyze_csv(str(input_path))
            analysis_time = time.time() - start_time

            # Process results
            print(f"\n✅ Analysis completed in {analysis_time:.2f} seconds")
            print(f"📊 Found {len(results)} matches")

            # Group results by operation type
            operations = {}
            advanced_patterns = {}
            tainted_count = 0

            for result in results:
                # Count operations
                if result.operation:
                    operations[result.operation] = operations.get(result.operation, 0) + 1

                # Count advanced patterns
                for pattern in result.advanced_patterns:
                    pattern_type = pattern.get('operation_type', 'Unknown')
                    advanced_patterns[pattern_type] = advanced_patterns.get(pattern_type, 0) + 1

                # Count tainted variables
                if result.tainted_variables:
                    tainted_count += 1

            # Print summary
            print("\n📈 Analysis Summary:")
            print("-" * 40)

            if operations:
                print("Database Operations:")
                for op, count in sorted(operations.items()):
                    print(f"  • {op}: {count}")

            if advanced_patterns:
                print("\nAdvanced Patterns Detected:")
                for pattern, count in sorted(advanced_patterns.items()):
                    print(f"  • {pattern}: {count}")

            print(f"\nTaint Analysis:")
            print(f"  • Operations with tainted variables: {tainted_count}")
            print(f"  • Taint coverage: {(tainted_count/len(results)*100):.1f}%" if results else "  • No results")

            # Performance metrics
            print("\n⚡ Performance Metrics:")
            for key, value in metrics.items():
                if isinstance(value, float):
                    if 'time' in key:
                        print(f"  • {key}: {value:.4f}s")
                    elif 'pct' in key or 'rate' in key:
                        print(f"  • {key}: {value:.2f}%")
                    else:
                        print(f"  • {key}: {value:.4f}")
                else:
                    print(f"  • {key}: {value}")

            # Save results
            self._save_enhanced_results(results, output_json, output_csv)

            print(f"\n💾 Results saved to:")
            print(f"  • JSON: {output_json}")
            print(f"  • CSV: {output_csv}")

            return True

        except Exception as e:
            self._print_error(f"Enhanced analysis failed: {str(e)}")
            if self.verbose:
                import traceback
                traceback.print_exc()
            return False

    def _analyze_standard(self, input_path: Path) -> bool:
        """Analyze with standard analyzer (legacy mode)"""
        try:
            lines, encoding = safe_file_read(str(input_path))
            if self.verbose:
                print(f"📝 File encoding: {encoding}")

            # CSV parsing
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

            # Save results
            if self.results:
                self._save_results()

            # Print summary
            self._print_summary()

            return True

        except Exception as e:
            self._print_error(f"Standard analysis failed: {str(e)}")
            return False

    def _save_enhanced_results(self, results: List, output_json: Path, output_csv: Path):
        """Save enhanced analysis results"""
        # Save JSON
        json_data = []
        for result in results:
            result_dict = asdict(result) if hasattr(result, '__dict__') else result
            json_data.append(result_dict)

        with open(output_json, 'w', encoding='utf-8') as f:
            json.dump(json_data, f, indent=2, ensure_ascii=False)

        # Save CSV
        if json_data:
            with open(output_csv, 'w', encoding='utf-8', newline='') as f:
                fieldnames = [
                    'file_path', 'line_number', 'status', 'operation',
                    'table', 'confidence', 'tainted_variables',
                    'advanced_patterns', 'context_scope'
                ]
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()

                for item in json_data:
                    row = {
                        'file_path': item.get('file_path', ''),
                        'line_number': item.get('line_number', ''),
                        'status': item.get('status', ''),
                        'operation': item.get('operation', ''),
                        'table': item.get('table', ''),
                        'confidence': f"{item.get('confidence', 0):.2f}",
                        'tainted_variables': ', '.join(item.get('tainted_variables', [])),
                        'advanced_patterns': len(item.get('advanced_patterns', [])),
                        'context_scope': item.get('context_info', {}).get('scope', '')
                    }
                    writer.writerow(row)

    def benchmark(self) -> bool:
        """Run performance benchmark comparing standard vs enhanced mode"""
        if not ENHANCED_MODE_AVAILABLE:
            print("❌ Enhanced mode not available for benchmarking")
            return False

        print("\n" + "="*80)
        print("PERFORMANCE BENCHMARK")
        print("="*80)

        # Create test file if not exists
        test_file = self.input_dir / "benchmark_test.csv"
        if not test_file.exists():
            # Create sample CSV
            with open(test_file, 'w', newline='') as f:
                writer = csv.writer(f)
                writer.writerow(['id', 'file_path', 'line_number'])
                # Add some sample entries
                test_abap = self.input_dir / "test.abap"
                if test_abap.exists():
                    for i in range(1, 11):
                        writer.writerow([i, str(test_abap), i * 10])

        results = {}

        # Test standard analyzer
        print("\n📝 Testing Standard Analyzer...")
        tracker_std = ABAPTracker(enhanced=False)
        start = time.time()
        success_std = tracker_std.analyze_csv(str(test_file))
        time_std = time.time() - start
        results['standard'] = {
            'time': time_std,
            'success': success_std
        }

        # Test enhanced analyzer
        print("\n🚀 Testing Enhanced Analyzer...")
        tracker_enh = ABAPTracker(enhanced=True)
        start = time.time()
        success_enh = tracker_enh.analyze_csv(str(test_file))
        time_enh = time.time() - start
        results['enhanced'] = {
            'time': time_enh,
            'success': success_enh
        }

        # Compare results
        print("\n" + "="*80)
        print("BENCHMARK RESULTS")
        print("="*80)
        print(f"Standard Analyzer: {time_std:.4f}s")
        print(f"Enhanced Analyzer: {time_enh:.4f}s")

        if time_std > 0:
            speedup = ((time_std - time_enh) / time_std) * 100
            if speedup > 0:
                print(f"✅ Enhanced is {speedup:.1f}% faster")
            else:
                print(f"⚠️  Enhanced is {abs(speedup):.1f}% slower")

        return True

    def run_tests(self) -> bool:
        """Run comprehensive test suite"""
        print("\n" + "="*80)
        print("RUNNING TEST SUITE")
        print("="*80)

        test_results = []

        # Test 1: Basic functionality
        print("\n🧪 Test 1: Basic Functionality")
        try:
            test_csv = self.input_dir / "test.csv"
            if test_csv.exists():
                success = self.analyze_csv(str(test_csv))
                test_results.append(("Basic Functionality", success))
                print(f"  Result: {'✅ PASSED' if success else '❌ FAILED'}")
            else:
                print(f"  ⚠️  Test file not found: {test_csv}")
                test_results.append(("Basic Functionality", False))
        except Exception as e:
            print(f"  ❌ Error: {e}")
            test_results.append(("Basic Functionality", False))

        # Test 2: Enhanced mode (if available)
        if ENHANCED_MODE_AVAILABLE:
            print("\n🧪 Test 2: Enhanced Mode")
            try:
                tracker = ABAPTracker(enhanced=True)
                test_csv = self.input_dir / "test.csv"
                if test_csv.exists():
                    success = tracker.analyze_csv(str(test_csv))
                    test_results.append(("Enhanced Mode", success))
                    print(f"  Result: {'✅ PASSED' if success else '❌ FAILED'}")
                else:
                    test_results.append(("Enhanced Mode", False))
            except Exception as e:
                print(f"  ❌ Error: {e}")
                test_results.append(("Enhanced Mode", False))

        # Summary
        print("\n" + "="*80)
        print("TEST SUMMARY")
        print("="*80)
        passed = sum(1 for _, result in test_results if result)
        total = len(test_results)
        print(f"Passed: {passed}/{total}")

        for test_name, result in test_results:
            status = "✅ PASSED" if result else "❌ FAILED"
            print(f"  {test_name}: {status}")

        return passed == total

    def _analyze_entry(self, row: Dict, idx: int, total: int) -> Optional[Dict]:
        """Analyze single CSV entry (standard mode)"""
        try:
            file_path = row.get('file_path', '').strip()
            line_number = int(row.get('line_number', 0))
            entry_id = row.get('id', idx)

            if not file_path:
                self._print_warning(f"Entry {entry_id}: Missing file path")
                return None

            # Check if file exists
            abap_file = Path(file_path)
            if not abap_file.is_absolute():
                # Try relative to input directory
                abap_file = self.input_dir / file_path

            if not abap_file.exists():
                self._print_warning(f"Entry {entry_id}: File not found: {file_path}")
                return None

            # Read ABAP file
            lines, encoding = safe_file_read(str(abap_file))

            if line_number > len(lines):
                self._print_warning(f"Entry {entry_id}: Line {line_number} exceeds file length")
                return None

            # Create analysis
            analysis = ComprehensiveAnalysis(
                id=entry_id,
                source_file=str(abap_file),
                line_number=line_number,
                abap_lines=lines
            )

            # Run analysis
            result = self.analyzer.analyze_comprehensive(analysis)

            # Progress indicator
            if self.verbose or idx % 10 == 0:
                progress = (idx / total) * 100
                print(f"  [{idx}/{total}] {progress:.1f}% - {file_path}:{line_number}")

            return result

        except Exception as e:
            self._print_error(f"Entry {idx}: {str(e)}")
            return None

    def _save_results(self):
        """Save analysis results (standard mode)"""
        if not self.results:
            return

        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Save JSON
        json_file = self.output_dir / f"analysis_{timestamp}.json"
        with open(json_file, 'w', encoding='utf-8') as f:
            json.dump(self.results, f, indent=2, ensure_ascii=False)

        # Save CSV
        csv_file = self.output_dir / f"analysis_{timestamp}.csv"
        self.analyzer.export_to_csv(self.results, str(csv_file))

        print(f"\n💾 Results saved:")
        print(f"  • JSON: {json_file}")
        print(f"  • CSV: {csv_file}")

    def _print_summary(self):
        """Print analysis summary (standard mode)"""
        if not self.results:
            print("\n⚠️  No results to summarize")
            return

        print("\n" + "="*80)
        print("ANALYSIS SUMMARY")
        print("="*80)

        total = len(self.results)
        with_syuname = sum(1 for r in self.results if r.get('has_syuname'))

        print(f"Total entries analyzed: {total}")
        print(f"Entries with SY-UNAME: {with_syuname} ({with_syuname/total*100:.1f}%)")

        # Operation statistics
        operations = {}
        for result in self.results:
            for op in result.get('db_operations', []):
                operations[op] = operations.get(op, 0) + 1

        if operations:
            print("\nDatabase Operations Found:")
            for op, count in sorted(operations.items(), key=lambda x: x[1], reverse=True):
                print(f"  • {op}: {count}")

    def _print_error(self, message: str):
        """Print error message"""
        print(f"❌ ERROR: {message}")

    def _print_warning(self, message: str):
        """Print warning message"""
        print(f"⚠️  WARNING: {message}")

    def _print_csv_format_help(self):
        """Print CSV format help"""
        print("\n📋 Expected CSV format:")
        print("  id,file_path,line_number")
        print("  1,program.abap,123")
        print("  2,/full/path/to/file.abap,456")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description='ABAP SY-UNAME Tracker - Enhanced Analysis Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python main.py analyze                    # Standard analysis
  python main.py analyze --enhanced         # Enhanced analysis with advanced patterns
  python main.py analyze input/test.csv     # Analyze specific file
  python main.py analyze --enhanced --window 100 --no-cache  # Custom settings
  python main.py benchmark                  # Compare standard vs enhanced
  python main.py test                       # Run test suite
  python main.py report                     # Generate report

Enhanced Mode Features:
  • Advanced pattern detection (EXEC SQL, CDS Views, BAPI, Authority Checks)
  • Context-aware taint tracking with scope analysis
  • Performance optimizations (caching, sliding window)
  • Inter-procedural flow analysis
  • Field symbol and reference tracking
        """
    )

    parser.add_argument(
        'command',
        choices=['analyze', 'test', 'report', 'benchmark'],
        help='Command to execute'
    )

    parser.add_argument(
        'input_file',
        nargs='?',
        help='Input CSV file (optional, uses default if not specified)'
    )

    parser.add_argument(
        '--enhanced', '-e',
        action='store_true',
        help='Use enhanced analyzer with advanced patterns and optimizations'
    )

    parser.add_argument(
        '--window', '-w',
        type=int,
        default=50,
        help='Sliding window size for enhanced mode (default: 50)'
    )

    parser.add_argument(
        '--no-cache',
        action='store_true',
        help='Disable pattern caching in enhanced mode'
    )

    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose output'
    )

    args = parser.parse_args()

    # Create tracker instance
    tracker = ABAPTracker(
        verbose=args.verbose,
        enhanced=args.enhanced,
        window_size=args.window,
        enable_cache=not args.no_cache
    )

    # Execute command
    success = False

    if args.command == 'analyze':
        success = tracker.analyze_csv(args.input_file)

    elif args.command == 'test':
        success = tracker.run_tests()

    elif args.command == 'benchmark':
        success = tracker.benchmark()

    elif args.command == 'report':
        # Generate report from latest results
        output_dir = Path("output")
        json_files = sorted(output_dir.glob("analysis_*.json"))

        if json_files:
            latest = json_files[-1]
            print(f"📊 Generating report from: {latest}")

            with open(latest, 'r', encoding='utf-8') as f:
                data = json.load(f)

            report_file = output_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"

            with open(report_file, 'w', encoding='utf-8') as f:
                f.write("ABAP SY-UNAME ANALYSIS REPORT\n")
                f.write("="*80 + "\n\n")
                f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write(f"Source: {latest}\n")
                f.write(f"Total entries: {len(data)}\n\n")

                # Add detailed analysis
                if data and isinstance(data[0], dict):
                    if 'operation' in data[0]:  # Enhanced format
                        f.write("ENHANCED ANALYSIS RESULTS\n")
                        f.write("-"*40 + "\n")
                        for item in data:
                            f.write(f"\nFile: {item.get('file_path', 'N/A')}\n")
                            f.write(f"Line: {item.get('line_number', 'N/A')}\n")
                            f.write(f"Operation: {item.get('operation', 'N/A')}\n")
                            f.write(f"Table: {item.get('table', 'N/A')}\n")
                            f.write(f"Confidence: {item.get('confidence', 0):.2f}\n")
                            if item.get('tainted_variables'):
                                f.write(f"Tainted: {', '.join(item['tainted_variables'])}\n")
                    else:  # Standard format
                        f.write("STANDARD ANALYSIS RESULTS\n")
                        f.write("-"*40 + "\n")
                        for item in data:
                            f.write(f"\nID: {item.get('id', 'N/A')}\n")
                            f.write(f"File: {item.get('source_file', 'N/A')}\n")
                            f.write(f"Line: {item.get('line_number', 'N/A')}\n")
                            f.write(f"Status: {item.get('status', 'N/A')}\n")

            print(f"✅ Report saved to: {report_file}")
            success = True
        else:
            print("❌ No analysis results found in output directory")
            success = False

    # Exit with appropriate code
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()