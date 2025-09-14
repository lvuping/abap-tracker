#!/usr/bin/env python3
"""
Enhanced ABAP Analyzer with Verbose Logging Extension
"""

import re
import time
import io
import csv
from typing import List, Dict, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from pathlib import Path


def analyze_csv_verbose(analyzer, csv_path: str, output_path: str = None) -> Tuple[List, Dict]:
    """
    Analyze CSV file with detailed verbose logging

    This function extends the EnhancedABAPAnalyzer with verbose logging
    """
    results = []

    print(f"\n{'='*80}")
    print(f"ENHANCED ANALYZER - VERBOSE DEBUG MODE")
    print(f"{'='*80}")
    print(f"\n📂 Loading CSV: {csv_path}")

    # Import encoding utils
    from utils.encoding_utils import safe_file_read

    # Read CSV with encoding detection
    csv_lines, encoding = safe_file_read(csv_path)
    print(f"   ✓ CSV encoding: {encoding}")
    print(f"   ✓ CSV lines read: {len(csv_lines)}")

    # Parse CSV from lines
    csv_content = io.StringIO(''.join(csv_lines))
    reader = csv.DictReader(csv_content)
    rows = list(reader)
    print(f"   ✓ Total rows in CSV: {len(rows)}")

    if rows:
        print(f"   ✓ CSV headers: {list(rows[0].keys())}")
        print(f"\n   First 3 rows:")
        for i, row in enumerate(rows[:3], 1):
            print(f"     Row {i}: {dict(row)}")

    # Group by file for efficiency
    files_to_analyze = {}
    skipped_rows = []
    processed_rows = 0

    print(f"\n📊 Processing CSV rows...")
    for idx, row in enumerate(rows, 1):
        if idx % 50 == 0:
            print(f"   ... processed {idx}/{len(rows)} rows")

        file_path = row.get('file_path', '').strip()
        line_number_str = row.get('line_number', '').strip()

        # Skip rows with missing data
        if not file_path:
            skipped_rows.append((idx, "missing file_path", row))
            continue

        if not line_number_str:
            skipped_rows.append((idx, "missing line_number", row))
            continue

        try:
            line_num = int(line_number_str) - 1  # Convert to 0-indexed
            processed_rows += 1
        except ValueError:
            skipped_rows.append((idx, f"invalid line_number '{line_number_str}'", row))
            continue

        if file_path not in files_to_analyze:
            files_to_analyze[file_path] = []
        files_to_analyze[file_path].append((idx, line_num))

    print(f"\n📊 CSV Processing Summary:")
    print(f"   • Total rows: {len(rows)}")
    print(f"   • Processed rows: {processed_rows}")
    print(f"   • Skipped rows: {len(skipped_rows)}")
    print(f"   • Unique files to analyze: {len(files_to_analyze)}")

    if skipped_rows:
        print(f"\n   ⚠️ Skipped row details (first 5):")
        for row_idx, reason, row_data in skipped_rows[:5]:
            print(f"     Row {row_idx}: {reason}")
            print(f"       Data: {row_data}")

    # Show files to analyze
    print(f"\n📁 Files to analyze (first 10):")
    for i, (file_path, line_data) in enumerate(list(files_to_analyze.items())[:10], 1):
        print(f"   {i}. {file_path}: {len(line_data)} lines")
    if len(files_to_analyze) > 10:
        print(f"   ... and {len(files_to_analyze) - 10} more files")

    # Analyze each file
    print(f"\n🔍 Starting file analysis...")
    print(f"{'='*60}")

    files_found = 0
    files_not_found = []
    total_results_by_file = {}
    errors = []

    for file_idx, (file_path, line_data) in enumerate(files_to_analyze.items(), 1):
        # Extract just line numbers for analysis
        line_nums = [ln for _, ln in line_data]
        row_indices = [idx for idx, _ in line_data]

        # Show progress for first 20 files and every 10th file after
        show_details = file_idx <= 20 or file_idx % 10 == 0

        if show_details:
            print(f"\n[{file_idx}/{len(files_to_analyze)}] Analyzing: {file_path}")
            print(f"   Lines to analyze: {len(line_nums)}")
            if len(line_nums) <= 10:
                print(f"   Line numbers (1-based): {[ln+1 for ln in line_nums]}")

        # Try file path as is, or relative to input directory
        file_to_check = Path(file_path)
        if not file_to_check.exists():
            # Try relative to input directory
            file_to_check = Path('input') / file_path
            if show_details:
                print(f"   Trying relative path: {file_to_check}")

        if file_to_check.exists():
            files_found += 1
            if show_details:
                print(f"   ✓ File found at: {file_to_check}")
                # Show file size
                file_size = file_to_check.stat().st_size
                print(f"   File size: {file_size:,} bytes")

            try:
                # Count lines in file
                with open(file_to_check, 'r', encoding='utf-8', errors='ignore') as f:
                    total_lines = len(f.readlines())
                if show_details:
                    print(f"   Total lines in file: {total_lines}")

                # Check if requested lines are within file bounds
                invalid_lines = [ln+1 for ln in line_nums if ln >= total_lines]
                if invalid_lines and show_details:
                    print(f"   ⚠️ Lines exceeding file length: {invalid_lines[:10]}")

                # Analyze file
                if show_details:
                    print(f"   Calling analyze_file...")

                file_results = analyzer.analyze_file(str(file_to_check), line_nums)
                total_results_by_file[file_path] = len(file_results)

                if show_details:
                    if file_results:
                        print(f"   ✓ Generated {len(file_results)} results")
                        # Show first result
                        if file_results:
                            first = file_results[0]
                            print(f"     Sample result:")
                            print(f"       Line: {first.line_number}")
                            print(f"       Operation: {first.operation}")
                            print(f"       Table: {first.table}")
                            print(f"       Confidence: {first.confidence:.2f}")
                    else:
                        print(f"   ⚠️ No results generated")
                        print(f"     Possible reasons:")
                        print(f"     - No DB operations at specified lines")
                        print(f"     - Pattern detection didn't match")
                        print(f"     - Taint tracking didn't propagate")

                results.extend(file_results)

            except Exception as e:
                errors.append(f"File {file_path}: {e}")
                if show_details:
                    print(f"   ✗ ERROR analyzing file: {e}")
                    import traceback
                    if file_idx <= 5:  # Show traceback for first 5 errors
                        traceback.print_exc()
        else:
            files_not_found.append((file_path, len(line_nums)))
            if show_details:
                print(f"   ✗ File not found")
                print(f"     Checked paths:")
                print(f"     - {Path(file_path).absolute()}")
                print(f"     - {(Path('input') / file_path).absolute()}")

    print(f"\n{'='*80}")
    print(f"📈 FINAL ANALYSIS SUMMARY")
    print(f"{'='*80}")
    print(f"   • CSV rows processed: {processed_rows}/{len(rows)}")
    print(f"   • Files found: {files_found}/{len(files_to_analyze)}")
    print(f"   • Files not found: {len(files_not_found)}")
    print(f"   • Total results generated: {len(results)}")
    print(f"   • Errors encountered: {len(errors)}")

    if files_not_found:
        print(f"\n   Files not found (first 10):")
        for file_path, line_count in files_not_found[:10]:
            print(f"     - {file_path} ({line_count} lines)")
        if len(files_not_found) > 10:
            print(f"     ... and {len(files_not_found) - 10} more")

    # Show results distribution
    if total_results_by_file:
        files_with_results = {k: v for k, v in total_results_by_file.items() if v > 0}
        if files_with_results:
            print(f"\n   Files with results ({len(files_with_results)}):")
            for file_path, count in list(files_with_results.items())[:10]:
                print(f"     - {file_path}: {count} results")
            if len(files_with_results) > 10:
                print(f"     ... and {len(files_with_results) - 10} more")

    if errors:
        print(f"\n   ⚠️ Errors encountered:")
        for error in errors[:5]:
            print(f"     - {error}")
        if len(errors) > 5:
            print(f"     ... and {len(errors) - 5} more")

    # Diagnostic check
    print(f"\n🔬 DIAGNOSTIC CHECK:")
    if len(results) == 0:
        print(f"   ⚠️ NO RESULTS GENERATED! Possible issues:")
        print(f"   1. File paths in CSV don't match actual file locations")
        print(f"   2. The analyze_file method isn't detecting patterns")
        print(f"   3. Lines specified don't contain database operations")
        print(f"   4. Taint tracking or pattern matching is too restrictive")
        print(f"\n   Recommendations:")
        print(f"   - Verify file paths are correct")
        print(f"   - Check if specified lines contain INSERT/UPDATE/MODIFY")
        print(f"   - Review pattern detection logic in analyze_file")
    elif len(results) < processed_rows / 10:
        print(f"   ⚠️ Very few results ({len(results)}) for {processed_rows} rows")
        print(f"   This might indicate:")
        print(f"   1. Pattern detection is too restrictive")
        print(f"   2. Many lines don't contain actual DB operations")
        print(f"   3. Taint tracking is not propagating correctly")
    else:
        print(f"   ✓ Result generation appears normal")
        print(f"   Ratio: {len(results)}/{processed_rows} = {len(results)/processed_rows*100:.1f}%")

    # Save results if output path provided
    if output_path:
        analyzer._save_results(results, output_path)
        print(f"\n💾 Results saved to: {output_path}")

    print(f"\n🏁 Verbose analysis complete!")
    print(f"{'='*80}\n")

    return results, analyzer.get_performance_metrics()