# ABAP Tracker - Command Guide

## 🚀 Quick Start

After the reorganization, you can use the simplified command runner:

```bash
python3 run.py [command] [options]
```

## 📋 Available Commands

### 1. **analyze** - Analyze CSV files

Analyzes a CSV file containing ABAP file references and line numbers to track SY-UNAME usage.

```bash
# Analyze default CSV (input/default.csv)
python3 run.py analyze

# Analyze specific CSV file
python3 run.py analyze input/test.csv

# Using main.py directly
python3 main.py analyze input/your_file.csv
```

**Input CSV Format:**
```csv
id,file_path,line_number
1,program1.abap,45
2,program2.abap,123
```

**Output:**
- CSV file: `output/analysis_TIMESTAMP.csv`
- JSON file: `output/analysis_TIMESTAMP.json`

### 2. **report** - Generate Analysis Report

Generates a summary report from the latest analysis results.

```bash
# Generate report from latest analysis
python3 run.py report

# Using main.py directly
python3 main.py report
```

**Output:**
- Report file: `output/report_TIMESTAMP.txt`

### 3. **test** - Run Test Suite

Runs various test suites to validate pattern detection.

```bash
# Run all tests
python3 run.py test

# Test specific pattern (e.g., INSERT pattern 01)
python3 run.py test I01

# Run specific test suite directly
python3 tests/test_ultimate_handler.py
python3 tests/test_all_db_operations.py
```

### 4. **help** - Show Help

```bash
python3 run.py help
```

## 📂 Directory Structure After Reorganization

```
abap-tracker/
├── main.py                 # Main entry point
├── run.py                  # Simplified command runner
├── COMMANDS.md            # This file
├── src/
│   ├── core/              # Core components
│   │   ├── db_handler.py
│   │   ├── analyzer.py
│   │   └── csv_analyzer.py
│   ├── patterns/          # Pattern handlers
│   │   ├── enhanced_insert_handler.py
│   │   ├── enhanced_update_handler.py
│   │   ├── enhanced_modify_handler.py
│   │   └── ...
│   └── utils/             # Utilities
│       ├── encoding_utils.py
│       ├── multiline_handler.py
│       ├── system_variable_extractor.py
│       └── taint_propagation.py
├── tests/                 # Test files
│   ├── test_all_db_operations.py
│   ├── test_ultimate_handler.py
│   └── ...
├── input/                 # Input ABAP and CSV files
├── output/               # Generated output files
├── docs/                 # Documentation
└── archive/              # Legacy/backup files
```

## 🔍 Pattern Coverage

The tool supports 70+ ABAP patterns including:

- **INSERT Patterns**: 25+ patterns (I01-I25)
- **UPDATE Patterns**: 24+ patterns (U01-U24)
- **MODIFY Patterns**: 24+ patterns (M01-M24)
- **Special Patterns**: RFC calls, PERFORM calls, Audit fields

## 💡 Examples

### Example 1: Analyze Multiple ABAP Files

1. Create input CSV file `input/analysis.csv`:
```csv
id,file_path,line_number
1,program1.abap,100
2,program2.abap,250
3,program3.abap,50
```

2. Run analysis:
```bash
python3 run.py analyze input/analysis.csv
```

3. Generate report:
```bash
python3 run.py report
```

### Example 2: Test Specific Pattern

To test if INSERT pattern 10 (I10) is working correctly:
```bash
python3 run.py test I10
```

### Example 3: Direct File Analysis

Analyze a single ABAP file directly:
```bash
python3 main.py path/to/file.abap
```

## 🛠️ Troubleshooting

### Import Errors
If you encounter import errors after reorganization, the imports have been updated to:
- `from core.xxx import` for core modules
- `from patterns.xxx import` for pattern handlers
- `from utils.xxx import` for utilities
- `from archive.xxx import` for archived modules

### Test File Locations
Test ABAP files should be placed in:
- `input/` directory for analysis
- `tests/data/` directory for test data

### Encoding Issues
The tool automatically detects file encoding (UTF-8, CP949). If you encounter encoding errors, check that your ABAP files are properly encoded.

## 📊 Output Formats

### CSV Output Columns
- **ID**: Unique identifier
- **Source_File**: ABAP file path
- **Line_Number**: Line where SY-UNAME is found
- **Status**: Analysis status
- **Final_Table**: Database table name
- **Final_Fields**: Database fields
- **DB_Operations**: Database operations detected
- **RFC_Functions**: RFC function calls
- **PERFORM_Calls**: PERFORM statements
- **Confidence**: Confidence score (0-1)
- **Tainted_Variables**: Variables containing SY-UNAME
- **Description**: Analysis description

### JSON Output Structure
```json
{
  "id": "1",
  "file": "program1.abap",
  "line": 100,
  "result": {
    "status": "Complete",
    "database_operations": [...],
    "tainted_variables": [...],
    "confidence": 0.85
  }
}
```

## 🔧 Advanced Usage

### Custom Pattern Testing
For testing custom patterns, modify test files in `tests/` directory and run:
```bash
python3 tests/test_ultimate_handler.py [PATTERN_ID]
```

### Batch Processing
For large-scale analysis, create a comprehensive CSV file and use:
```bash
python3 main.py analyze input/batch.csv --verbose
```

## 📝 Notes

- The tool traces SY-UNAME through variable assignments
- Supports multi-line ABAP statements
- Handles complex patterns like VALUE constructors, CORRESPONDING, etc.
- Confidence scoring helps prioritize results
- All outputs are timestamped for tracking

## 🆘 Need Help?

For issues or questions:
1. Check test files in `tests/` for examples
2. Review pattern definitions in `src/patterns/`
3. Check archived implementations in `archive/` for reference