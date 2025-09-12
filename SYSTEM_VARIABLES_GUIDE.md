# System Variables Extraction Guide

## 📚 Overview

This guide explains how to extract and analyze ABAP system variables (SY-* fields) from your code. System variables are crucial for tracking user actions, timestamps, program flow, and system state.

## 🎯 What are System Variables?

System variables in ABAP are predefined variables that contain runtime information. They all start with `SY-` or `SYST-` prefix.

### Common System Variables

#### User & System Information
- `SY-UNAME` - Current user name
- `SY-MANDT` - Client number
- `SY-LANGU` - Logon language
- `SY-SYSID` - System ID
- `SY-HOST` - Host name

#### Date & Time
- `SY-DATUM` - Current date (YYYYMMDD)
- `SY-UZEIT` - Current time (HHMMSS)
- `SY-TIMLO` - Local time
- `SY-DATLO` - Local date
- `SY-ZONLO` - Time zone

#### Program Flow Control
- `SY-SUBRC` - Return code (0 = success)
- `SY-INDEX` - Loop counter
- `SY-TABIX` - Current line of internal table
- `SY-TFILL` - Number of table entries
- `SY-DBCNT` - Number of processed database lines

#### Program Information
- `SY-REPID` - Current program name
- `SY-CPROG` - Calling program
- `SY-TCODE` - Transaction code
- `SY-DYNNR` - Screen number

## 🛠️ How to Extract System Variables

### Method 1: Using the Command-Line Tool

```bash
# Extract all system variables from an ABAP file
python extract_system_variables.py program.abap

# Extract with verbose output
python extract_system_variables.py program.abap -v

# Extract specific variables only
python extract_system_variables.py program.abap --vars SY-UNAME,SY-DATUM

# Save results to CSV
python extract_system_variables.py program.abap -o results.csv

# Save results to JSON
python extract_system_variables.py program.abap -o results.json
```

### Method 2: Using Python API

```python
from src.system_variable_extractor import SystemVariableExtractor

# Initialize extractor
extractor = SystemVariableExtractor()

# Read your ABAP file
with open('program.abap', 'r') as f:
    lines = f.readlines()

# Extract all system variables
all_vars = extractor.extract_all(lines)

# Extract specific variables
specific_vars = extractor.extract_specific(lines, ['SY-UNAME', 'SY-DATUM'])

# Get usage summary
summary = extractor.get_usage_summary(lines)

# Find database impacts
db_impacts = extractor.find_database_impacts(lines)

# Track variable flow
flow = extractor.track_variable_flow(lines, 'SY-UNAME')
```

## 📊 Understanding the Output

### 1. Variable Extraction Results

Each extracted variable includes:
- **Name**: The system variable name (e.g., SY-UNAME)
- **Line Number**: Where it appears in the code
- **Context**: How it's being used
- **Statement**: The actual ABAP statement
- **Target**: Target variable/field (for assignments)
- **Confidence**: Detection confidence score

### 2. Usage Contexts

The tool identifies HOW system variables are used:

| Context | Description | Example |
|---------|-------------|---------|
| **Assignment** | Variable receives SY value | `lv_user = sy-uname` |
| **Database Insert** | INSERT operation | `INSERT ztable FROM VALUE #( created_by = sy-uname )` |
| **Database Update** | UPDATE operation | `UPDATE ztable SET changed_by = sy-uname` |
| **Database Modify** | MODIFY operation | `MODIFY ztable FROM ls_rec` |
| **Database Select** | SELECT WHERE clause | `SELECT * WHERE user = sy-uname` |
| **Condition** | IF/WHILE/CHECK statements | `IF sy-subrc = 0` |
| **Function Parameter** | CALL FUNCTION | `CALL FUNCTION 'Z_FUNC' EXPORTING user = sy-uname` |
| **String Template** | String interpolation | `\|User: { sy-uname }\|` |
| **Output** | WRITE statement | `WRITE sy-uname` |

### 3. Usage Summary

Shows aggregated statistics:
```
SY-UNAME        :  15 occurrences
  Description: Current user name
  Contexts: Assignment, Database Insert, Database Update
  Lines: 45, 67, 89, 123, 156...
```

### 4. Database Impacts

Lists all database operations using system variables:
```
Line  156: SY-UNAME     - Database Insert
  Statement: INSERT ztable FROM VALUE #( created_by = sy-uname )

Line  189: SY-DATUM     - Database Update
  Statement: UPDATE ztable SET changed_date = sy-datum
```

### 5. Variable Flow Analysis

Tracks how system variables propagate through code:
```
SY-UNAME:
  → Assignments: 3 (variables that receive the value)
  → Database ops: 5 (INSERT/UPDATE/MODIFY operations)
  → Function calls: 2 (RFC or function calls)
  → Other usage: 7 (other contexts)
```

## 💡 Practical Examples

### Example 1: Security Audit - Find All User Tracking

```bash
# Extract all SY-UNAME usage
python extract_system_variables.py program.abap --vars SY-UNAME -o user_tracking.csv
```

This helps identify:
- Where user information is stored
- Which tables contain user data
- How user actions are logged

### Example 2: Timestamp Analysis

```bash
# Extract date/time variables
python extract_system_variables.py program.abap --vars SY-DATUM,SY-UZEIT -o timestamps.csv
```

Useful for:
- Finding all timestamp recordings
- Identifying audit trail implementations
- Checking time-based logic

### Example 3: Error Handling Review

```bash
# Extract return code checks
python extract_system_variables.py program.abap --vars SY-SUBRC -v
```

Helps locate:
- Error handling after database operations
- Function call error checks
- Missing error handling

### Example 4: Complete System Variable Audit

```python
from src.system_variable_extractor import SystemVariableExtractor
import glob

extractor = SystemVariableExtractor()
all_results = {}

# Analyze all ABAP files
for filepath in glob.glob('*.abap'):
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    # Get summary for each file
    summary = extractor.get_usage_summary(lines)
    db_impacts = extractor.find_database_impacts(lines)
    
    all_results[filepath] = {
        'summary': summary,
        'db_impacts': len(db_impacts)
    }

# Print consolidated report
print("System Variable Usage Report")
print("=" * 60)

for filepath, result in all_results.items():
    print(f"\n{filepath}:")
    for var_name, info in result['summary'].items():
        print(f"  {var_name}: {info['count']} times")
    print(f"  Database impacts: {result['db_impacts']}")
```

## 🔍 Advanced Features

### 1. Variable Flow Tracking

Track how a system variable flows through the code:

```python
flow = extractor.track_variable_flow(lines, 'SY-UNAME')

# Shows:
# - Which variables receive SY-UNAME value
# - Where those variables are used
# - Database operations using the value
# - Function calls passing the value
```

### 2. Database Impact Analysis

Find all system variables affecting database:

```python
db_impacts = extractor.find_database_impacts(lines)

for impact in db_impacts:
    print(f"Table operation at line {impact.line_number}")
    print(f"  Variable: {impact.name}")
    print(f"  Operation: {impact.context.value}")
```

### 3. Custom Variable Detection

Add your own system variables:

```python
extractor = SystemVariableExtractor()

# Add custom system variable
extractor.system_variables['SY-CUSTOM'] = 'Custom system variable'

# Now it will be detected
vars = extractor.extract_all(lines)
```

## 📈 Use Cases

### 1. Security & Compliance
- Track user data storage (GDPR compliance)
- Audit trail verification
- Authorization checks review

### 2. Performance Analysis
- Find unnecessary SY-SUBRC checks
- Identify redundant timestamp captures
- Loop optimization (SY-INDEX, SY-TABIX)

### 3. Code Quality
- Ensure proper error handling
- Verify consistent timestamp usage
- Check client handling (SY-MANDT)

### 4. Documentation
- Generate variable usage reports
- Create data flow diagrams
- Document system dependencies

## 🎯 Best Practices

### 1. Regular Audits
Run system variable extraction regularly to:
- Track changes in user data handling
- Ensure consistent timestamp usage
- Verify error handling coverage

### 2. Focus Areas
Priority variables to monitor:
- `SY-UNAME` - User tracking
- `SY-DATUM/UZEIT` - Timestamps
- `SY-SUBRC` - Error handling
- `SY-MANDT` - Client handling
- `SY-TCODE` - Transaction tracking

### 3. Integration
Integrate extraction into:
- Code review process
- CI/CD pipelines
- Security audits
- Documentation generation

## 📝 Output Formats

### CSV Format
```csv
File,Line,Variable,Context,Target,Statement,Confidence
program.abap,45,SY-UNAME,Assignment,lv_user,lv_user = sy-uname,0.95
program.abap,67,SY-DATUM,Database Insert,,INSERT ztable FROM VALUE #( created_date = sy-datum ),0.95
```

### JSON Format
```json
{
  "file": "program.abap",
  "summary": {
    "SY-UNAME": {
      "count": 5,
      "contexts": ["Assignment", "Database Insert"],
      "lines": [45, 67, 89],
      "description": "Current user name"
    }
  },
  "database_impacts": [
    {
      "name": "SY-UNAME",
      "line": 67,
      "context": "Database Insert",
      "statement": "INSERT ztable FROM VALUE #( created_by = sy-uname )"
    }
  ]
}
```

## 🚀 Quick Start Checklist

1. ✅ Install the tool
2. ✅ Run basic extraction: `python extract_system_variables.py your_program.abap`
3. ✅ Review the summary output
4. ✅ Check database impacts
5. ✅ Export to CSV for detailed analysis
6. ✅ Focus on high-usage variables
7. ✅ Track variable flows for critical data

## 🤝 Support

For questions or issues:
1. Check this guide
2. Run with `-v` for verbose output
3. Review the example code
4. Open an issue on GitHub

---

**Remember**: System variables are the key to understanding program behavior, user tracking, and data flow in ABAP applications!