# ABAP SY-UNAME Tracker - Execution Guide

## 📋 Overview
This tool analyzes ABAP code to track the flow of `SY-UNAME` variable and identify where it's used (database operations, RFC calls, etc.).

## 🚀 Quick Start

### Method 1: Using test.csv (Recommended)
```bash
# Create your test.csv file with locations to analyze
python run_test_csv.py
```

### Method 2: Using sy_uname_locations.csv
```bash
# Uses input/sy_uname_locations.csv
python main.py --csv
```

## 📁 Input File Format

### test.csv Structure
The input CSV file should have three columns:
- `id`: Unique identifier for each test case
- `file_path`: Name of the ABAP file (without path and extension)
- `line_number`: Line number where SY-UNAME appears

Example `input/test.csv`:
```csv
id,file_path,line_number
1,test_basic,10
2,user_example,5
3,01_database_insert,50
```

## 🎯 How It Works

1. **Read Input**: Reads test.csv to get locations to analyze
2. **Load ABAP Files**: Loads the specified ABAP files from `input/` folder
3. **Track SY-UNAME**: Traces the flow of SY-UNAME from the specified line
4. **Identify Sinks**: Finds where the tracked value is used:
   - Database operations (INSERT, UPDATE, MODIFY, DELETE, SELECT)
   - RFC calls
   - Audit fields
   - Transaction calls
5. **Generate Output**: Creates CSV and JSON results in `output/` folder

## 📊 Output Files

### CSV Output
Generated in `output/test_results_YYYYMMDD_HHMMSS.csv` with columns:
- `ID`: Test case identifier
- `Source_File`: ABAP file path
- `Line_Number`: SY-UNAME line number
- `Status`: Analysis result status
  - `Found`: Successfully tracked to a sink
  - `Scope Boundary Reached`: Tracking stopped at scope boundary
  - `Not Found`: SY-UNAME not found at specified line
- `Type`: Type of sink found (DATABASE_INSERT_FIELD, RFC, etc.)
- `Table`: Database table name (if applicable)
- `Fields`: Database fields affected
- `RFC_Name`: RFC function name (if applicable)
- `Trace_Steps`: Number of tracking steps
- `Trace_Step_1-5`: Detailed tracking path

### JSON Output
Generated in `output/test_results_YYYYMMDD_HHMMSS.json` with detailed analysis results.

## 🔧 Command Line Options

### run_test_csv.py
```bash
# Default: Generate both JSON and CSV
python run_test_csv.py

# Generate JSON only
python run_test_csv.py --json-only

# Generate CSV only  
python run_test_csv.py --csv-only

# Verbose output (show detailed tracking)
python run_test_csv.py --verbose

# Create sample test.csv
python run_test_csv.py --create-sample
```

### main.py
```bash
# Default: JSON output
python main.py

# Add CSV output
python main.py --csv

# CSV only
python main.py --format csv

# Verbose mode
python main.py --verbose
```

## 📝 Example Workflow

1. **Prepare Input File**:
```bash
# Create sample test.csv
python run_test_csv.py --create-sample
```

2. **Edit test.csv**:
```csv
id,file_path,line_number
1,test_complex_flow,25
2,test_perform_using,15
3,05_database_select,100
```

3. **Run Analysis**:
```bash
python run_test_csv.py --verbose
```

4. **Check Results**:
- Open `output/test_results_*.csv` in Excel
- View `output/test_results_*.json` for detailed analysis

## 🎯 Detection Capabilities

### Database Operations
- INSERT INTO table VALUES structure
- UPDATE table SET field = value
- MODIFY table FROM structure
- DELETE FROM table
- SELECT ... WHERE field = value

### RFC Calls
- CALL FUNCTION 'RFC_NAME'
- Tracks EXPORTING parameters

### Audit Fields
- Common audit field patterns (CREATED_BY, MODIFIED_BY, etc.)
- Structure-field combinations

### Scope Boundaries
- PERFORM subroutine calls
- FORM definitions
- FUNCTION/METHOD definitions
- CLASS definitions
- INCLUDE statements

## 📊 Result Interpretation

### Success Cases (✅)
- **Database Found**: SY-UNAME tracked to database operation
- **RFC Found**: SY-UNAME passed to RFC function
- **Audit Field Found**: SY-UNAME assigned to audit field

### Boundary Cases (⛔)
- **PERFORM Call**: Tracking stopped at subroutine call
- **FORM Definition**: Entered new form definition
- **Dynamic Calls**: Dynamic PERFORM/CALL FUNCTION

### Failure Cases (❌)
- **Not at Line**: SY-UNAME not found at specified line
- **No Sink Found**: Tracked but no valid sink found
- **File Not Found**: ABAP file doesn't exist

## 🛠️ Troubleshooting

### Common Issues

1. **"File not found" error**:
   - Ensure ABAP files are in `input/` folder
   - File names in CSV should not include path or extension

2. **"SY-UNAME not at specified line"**:
   - Verify the line number is correct
   - Check if SY-UNAME exists at that line in the file

3. **"Scope Boundary Reached"**:
   - Normal behavior when tracking crosses subroutine boundaries
   - Consider analyzing the called subroutine separately

## 📈 Performance Tips

- For large-scale analysis, use batch processing with test.csv
- Use `--verbose` flag to debug tracking issues
- JSON output provides more detailed tracking information
- CSV output is better for Excel analysis and reporting

## 🔍 Advanced Usage

### Custom Analysis Patterns
Edit `patterns.py` to add custom detection patterns for:
- New database operations
- Custom RFC patterns
- Organization-specific audit fields

### Batch Processing
Create multiple test.csv files for different analysis scenarios:
```bash
python run_test_csv.py --input input/test_batch1.csv
python run_test_csv.py --input input/test_batch2.csv
```

## 📞 Support

For issues or questions:
1. Check the verbose output for detailed error messages
2. Verify input file format matches examples
3. Ensure all ABAP files are in the `input/` directory