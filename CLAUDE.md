# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ABAP Database Operations Tracker - A Python tool for analyzing ABAP code to detect and track database operations (INSERT, UPDATE, MODIFY) with special focus on system variable usage like `sy-uname`. The tool traces variable flow through ABAP code to identify where system variables end up in database tables.

## Key Commands

### Running Analysis
```bash
# Basic analysis using default CSV
python3 main.py analyze

# Analyze specific CSV file
python3 main.py analyze input/your_file.csv

# Analyze single ABAP file directly
python3 main.py path/to/file.abap

# Generate report from latest analysis
python3 main.py report
```

### Testing
```bash
# Run comprehensive test suite
python3 tests/test_ultimate_handler.py

# Test specific pattern (e.g., INSERT pattern 01)
python3 tests/test_ultimate_handler.py I01

# Run all database operation tests
python3 tests/test_all_db_operations.py
```

### Development Tasks
```bash
# No linting or type checking commands are configured
# Tests are the primary validation method
```

## Architecture Overview

### Core Components

1. **Handler System** (`src/complete_db_handler.py`)
   - Primary handler supporting 70+ ABAP patterns
   - Handles INSERT, UPDATE, MODIFY operations
   - Multi-line statement parsing
   - Context-aware variable tracking

2. **Pattern Handlers**
   - `src/enhanced_insert_handler.py` - INSERT pattern processing
   - `src/enhanced_update_handler.py` - UPDATE pattern processing
   - `src/enhanced_modify_handler.py` - MODIFY pattern processing
   - `src/multiline_statement_handler.py` - Multi-line ABAP statements

3. **Analysis Engine**
   - `src/unified_analyzer.py` - Legacy analysis engine
   - `src/csv_analyzer.py` - CSV export functionality
   - `src/system_variable_extractor.py` - System variable detection

### Data Flow

1. **Input Processing**: CSV file with ABAP file references and line numbers → Load ABAP files
2. **Variable Tracking**: Track sy-uname and tainted variables through assignments
3. **Pattern Matching**: Apply 70+ patterns to detect database operations
4. **Output Generation**: Generate CSV/JSON results with table names, fields, and operations

### Key Concepts

- **Tainted Variables**: Variables that contain sy-uname data, tracked through assignments
- **Sink Detection**: Database operations where tainted data ends up (INSERT/UPDATE/MODIFY)
- **Pattern Priority**: DATABASE operations > RFC calls > PERFORM calls > Audit fields
- **Multi-line Handling**: Statements spanning multiple lines with proper continuation

## Input/Output Format

### Input CSV
```csv
id,file_path,line_number
1,program1.abap,45
2,program2.abap,123
```

### Output CSV Columns
- ID, Source_File, Line_Number, Status
- Final_Table, Final_Fields, DB_Operations
- RFC_Functions, PERFORM_Calls
- Confidence, Tainted_Variables, Description

## Testing Strategy

The project uses pattern-based testing with real ABAP code samples in `test/` directory. Test files like `all_abap_patterns.abap` contain 70+ test patterns with expected results defined in the test runner.