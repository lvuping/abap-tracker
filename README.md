# ABAP Tracker - Enhanced SY-UNAME Analysis Tool

A comprehensive tool for tracking and analyzing SY-UNAME usage in ABAP code, focusing on direct database impacts, RFC calls, and PERFORM subroutines.

## ⚡ Quick Command Reference

```bash
# Basic analysis (uses input/sy_uname_locations.csv)
python3 main.py analyze

# Analyze specific CSV file
python3 main.py analyze input/your_file.csv

# Run tests
python3 main.py test

# Generate report
python3 main.py report
```

## 🚀 Key Features

- **Direct Impact Focus**: Only shows tables, fields, and operations that directly affect data
- **Smart Extraction**: Identifies CREATED_BY, CHANGED_BY, and other audit fields
- **Clean Output**: No unnecessary keywords (IF, LOOP) - only what matters
- **Excel-Compatible CSV**: Ready for immediate analysis in Excel
- **Comprehensive Tracking**: Database operations, RFC calls, PERFORM subroutines

## 📁 Project Structure

```
abap-tracker/
├── src/                     # Source code modules
│   ├── unified_analyzer.py  # Core analysis engine
│   ├── improved_patterns.py # Pattern matching system
│   ├── csv_analyzer.py      # Enhanced CSV processor
│   ├── analyzer.py          # Basic analysis functions
│   ├── patterns.py          # Pattern definitions
│   └── encoding_utils.py    # Encoding utilities
├── input/                   # Input files and test data
│   ├── sy_uname_locations.csv  # Default input CSV
│   └── *.abap              # ABAP source files
├── output/                  # Analysis results
│   ├── analysis_*.csv      # CSV results
│   └── analysis_*.json     # JSON results
├── test/                    # Test files
│   └── *.abap              # Test ABAP files
└── main.py                  # Main entry point
```

## 🎯 Quick Start

### Prerequisites
- Python 3.7 or higher
- No additional packages required (uses standard library only)

### Step 1: Prepare Your CSV File

Create `input/sy_uname_locations.csv` with the following format:
```csv
id,file_path,line_number
1,program1.abap,45
2,program2.abap,123
3,report_xyz.abap,567
```

### Step 2: Run Analysis

```bash
# Analyze default CSV file (input/sy_uname_locations.csv)
python3 main.py analyze

# Analyze your own CSV file
python3 main.py analyze input/your_file.csv

# Analyze with verbose output
python3 main.py analyze -v
```

### Step 3: Check Results

Results are saved in the `output/` folder:
- **CSV File**: `output/analysis_YYYYMMDD_HHMMSS.csv` - Open with Excel
- **JSON File**: `output/analysis_YYYYMMDD_HHMMSS.json` - Detailed data

### Additional Commands

```bash
# Run test suite
python3 main.py test

# Generate summary report from latest analysis
python3 main.py report
```

## 📊 CSV Format

### Input CSV Format

The input CSV should have the following format:

```csv
id,file_path,line_number
1,example.abap,45
2,another_file.abap,123
3,user_module.abap,567
```

- **id**: Optional unique identifier
- **file_path**: Path to ABAP file (relative to input/ or absolute)
- **line_number**: Line number where SY-UNAME appears

### Output CSV Format (Improved - Focus on Direct Impacts)

The enhanced output CSV now focuses on what really matters:

| Column | Description | Example |
|--------|-------------|---------|
| **ID** | Unique identifier | 1, 2, 3... |
| **Source_File** | ABAP source file | input/program.abap |
| **Line_Number** | Line where SY-UNAME appears | 45 |
| **Status** | Analysis status | Success, Partial, Not Found |
| **Final_Table** | 🎯 **Database tables affected** | ZTABLE, ZAUDIT_LOG |
| **Final_Fields** | 🎯 **Fields where SY-UNAME is stored** | CREATED_BY, CHANGED_BY |
| **DB_Operations** | 🎯 **Database operations performed** | INSERT, UPDATE, DELETE |
| **RFC_Functions** | 🎯 **RFC functions called** | Z_REMOTE_FUNCTION |
| **PERFORM_Calls** | 🎯 **Subroutines executed** | update_audit_log |
| **Confidence** | Analysis confidence score | 0.85 (0.0-1.0) |
| **Tainted_Variables** | Top 5 affected variables | LV_USER, GV_ADMIN |
| **Description** | Brief summary | Direct impacts summary |

## 🔍 What Gets Analyzed

### Primary Focus - Direct Database Impacts
✅ **Database Operations**
- INSERT, UPDATE, DELETE, MODIFY operations
- Tables where SY-UNAME data is stored
- Specific fields affected (CREATED_BY, CHANGED_BY, etc.)

✅ **Function Calls**
- RFC function calls with parameters
- PERFORM subroutine calls
- Data passed to external systems

✅ **Audit Trail**
- User tracking fields
- Timestamp fields
- Authorization logs

### What's NOT Included (Clean Output)
❌ Control flow keywords (IF, LOOP, CASE)
❌ Variable declarations (DATA, TYPES)
❌ System fields that don't store user data
❌ Comments and formatting

## 📈 Commands

### `analyze` - Analyze CSV File

```bash
python main.py analyze [csv_file] [-v]
```

Options:
- `csv_file`: Path to CSV file (optional, defaults to input/sy_uname_locations.csv)
- `-v, --verbose`: Enable verbose output

### `test` - Run Test Suite

```bash
python main.py test [-v]
```

Runs automated tests on sample ABAP files in the test directory.

### `report` - Generate Report

```bash
python main.py report
```

Generates a summary report from the latest analysis results.

## 💡 Real-World Examples

### Example 1: Complete Analysis Workflow

#### 1️⃣ Prepare your CSV file
```csv
# input/my_programs.csv
id,file_path,line_number
1,z_user_report.abap,156
2,z_audit_log.abap,89
3,z_rfc_handler.abap,234
```

#### 2️⃣ Run the analysis
```bash
python3 main.py analyze input/my_programs.csv
```

#### 3️⃣ Check the output
```
🔍 Starting Enhanced SY-UNAME Analysis
📁 Input file: input/my_programs.csv
================================================================================
📊 Found 3 entries to analyze

📍 [1/3] z_user_report.abap (line 156)
   ✅ Success
      Tables: ZUSER_DATA, ZAUDIT_LOG
      RFC: Z_UPDATE_USER
      PERFORM: write_audit_log
      Confidence: 0.95

✅ Analysis complete!
📁 Results saved:
   CSV: output/analysis_20250110_143022.csv
```

#### 4️⃣ Open CSV in Excel
The CSV will show:
| Final_Table | Final_Fields | DB_Operations | RFC_Functions | PERFORM_Calls |
|-------------|--------------|---------------|---------------|---------------|
| ZUSER_DATA, ZAUDIT_LOG | CREATED_BY, CHANGED_BY | INSERT, UPDATE | Z_UPDATE_USER | write_audit_log |

### Example 2: Test Suite

```bash
python main.py test
```

Output:
```
🧪 Running Test Suite
================================================================================

Testing: 01_database_insert.abap
  ✅ Pass - 5 variables tracked

Testing: 02_database_update.abap
  ✅ Pass - 8 variables tracked
...

================================================================================
Test Results: 18 passed, 2 failed
```

### Example 3: Report Generation

```bash
python main.py report
```

Output:
```
📈 Generating Report
================================================================================
Using: output/analysis_20250110_143022.csv

ABAP SY-UNAME TRACKER - ANALYSIS REPORT
================================================================================
Generated: 2025-01-10T14:35:00

SUMMARY
----------------------------------------
Total analyzed: 50
Successful: 45 (90.0%)

TOP TABLES
----------------------------------------
  ZTABLE1: 15 occurrences
  USR02: 12 occurrences
  CDHDR: 8 occurrences
...

TOP RFC FUNCTIONS
----------------------------------------
  RFC_READ_TABLE: 5 calls
  Z_CUSTOM_RFC: 3 calls
...
```

## 🛠️ Supported ABAP Patterns

### Database Operations
- `INSERT` - Table insertion with field mapping
- `UPDATE` - Table updates with WHERE conditions
- `MODIFY` - Table modifications
- `DELETE` - Record deletion
- `SELECT` - Data retrieval with SY-UNAME in WHERE

### Variable Assignments
- Direct assignment (`=`)
- `MOVE` statements
- `MOVE-CORRESPONDING`
- String templates with embedded variables
- Structure field assignments
- Internal table operations

### System Calls
- RFC function calls with parameter tracking
- PERFORM/FORM subroutines
- Method calls
- BDC field mapping

### Advanced Patterns
- Multi-line statements with colons
- Nested structures
- Field symbols
- Dynamic SQL generation
- Loop processing

## 🔧 Configuration

### Directory Configuration

- **Input Directory**: `input/` - Place your ABAP files and CSV here
- **Output Directory**: `output/` - Results are saved here
- **Test Directory**: `test/` - Test ABAP files
- **Source Directory**: `src/` - Core Python modules

### File Extensions

- ABAP files: `.abap` extension is automatically added if missing
- CSV files: UTF-8 with BOM for Excel compatibility
- JSON files: UTF-8 encoding with proper formatting

## 🎯 Use Cases

### Security Audit
- Identify all tables storing user information
- Track user data flow through the system
- Find potential GDPR compliance issues

### Code Review
- Quickly identify database impacts
- Review RFC calls and external interfaces
- Validate audit trail implementation

### Documentation
- Generate data flow documentation
- Create security impact assessments
- Build compliance reports

## 🐛 Troubleshooting

### Common Issues

1. **File Not Found**: 
   - Ensure ABAP files are in the `input/` directory
   - Check file paths in CSV are correct

2. **No Results**: 
   - Verify SY-UNAME exists at the specified line
   - Check that line numbers are correct (1-based)

3. **CSV Format Error**: 
   - Headers must be: `id,file_path,line_number`
   - No spaces after commas

### Debug Mode

```bash
# Enable verbose output for debugging
python3 main.py analyze -v

# Check specific file
python3 main.py analyze input/test.csv -v
```

## 📊 Performance

- **Accuracy**: High precision pattern matching
- **Speed**: Fast processing of large codebases
- **Context**: Analyzes 200 lines before and 1000 lines after target
- **Depth**: Tracks up to 20 levels of variable propagation

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## 📄 License

MIT License - Use freely for your ABAP security analysis needs.

---

**For questions or support, please open an issue on GitHub!** 🙏