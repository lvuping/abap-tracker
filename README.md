# ABAP SY-UNAME Tracker 🔍

Advanced ABAP code analyzer for tracking SY-UNAME (system username) propagation through database operations with enhanced pattern detection and context-aware taint tracking.

## 🌟 Features

### Standard Mode
- **Database Operation Detection**: INSERT, UPDATE, MODIFY patterns
- **System Variable Tracking**: SY-UNAME, SY-DATUM, SY-UZEIT, SY-MANDT
- **Multi-line Statement Support**: Handles ABAP statements spanning multiple lines
- **CSV Export**: Analysis results in CSV and JSON formats
- **70+ ABAP Patterns**: Comprehensive pattern library

### Enhanced Mode (New! 🚀)
- **Advanced Pattern Detection**:
  - EXEC SQL blocks (Native SQL)
  - CDS View operations
  - AMDP (ABAP Managed Database Procedures)
  - Authority checks with system variables
  - Enhanced BAPI/RFC tracking
  - BDC data handling
  - Complex JOIN operations
  - Dynamic SQL patterns
  - Field symbol operations

- **Context-Aware Taint Tracking**:
  - Scope analysis (FORM, METHOD, LOOP, IF blocks)
  - Inter-procedural flow tracking
  - Field symbol and reference tracking
  - Confidence-based propagation
  - MOVE-CORRESPONDING support

- **Performance Optimizations**:
  - Pattern compilation caching
  - Sliding window analysis
  - Reduced memory footprint
  - Parallel processing ready

## 📦 Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/abap-tracker.git
cd abap-tracker

# No additional dependencies required - uses Python 3 standard library
python3 --version  # Ensure Python 3.6+ is installed
```

## 🚀 Quick Start

### Basic Usage (Standard Mode)

```bash
# Analyze default CSV file (input/sy_uname_locations.csv)
python3 main.py analyze

# Analyze specific CSV file
python3 main.py analyze input/your_file.csv
```

### Enhanced Mode Usage

```bash
# Use enhanced analyzer with all optimizations
python3 main.py analyze --enhanced

# Analyze specific file with enhanced mode
python3 main.py analyze input/test.csv --enhanced

# Custom window size for large files
python3 main.py analyze --enhanced --window 100

# Disable caching (useful for debugging)
python3 main.py analyze --enhanced --no-cache

# Verbose output
python3 main.py analyze --enhanced --verbose
```

## 📋 CSV Input Format

Create a CSV file with the following format:

```csv
id,file_path,line_number
1,program1.abap,45
2,program2.abap,123
3,/full/path/to/file.abap,789
```

- **id**: Unique identifier for each entry
- **file_path**: Path to ABAP file (relative or absolute)
- **line_number**: Line number to analyze in the ABAP file

## 🎯 Command Reference

### analyze
Analyze ABAP files for SY-UNAME usage

```bash
# Standard analysis
python3 main.py analyze

# Enhanced analysis with advanced patterns
python3 main.py analyze --enhanced

# Custom settings
python3 main.py analyze --enhanced --window 75 --verbose
```

**Options:**
- `--enhanced, -e`: Enable enhanced analyzer with advanced patterns
- `--window, -w`: Sliding window size (default: 50 lines)
- `--no-cache`: Disable pattern caching
- `--verbose, -v`: Show detailed progress

### benchmark
Compare performance between standard and enhanced modes

```bash
python3 main.py benchmark
```

### test
Run the test suite

```bash
python3 main.py test
```

### report
Generate a detailed report from the latest analysis

```bash
python3 main.py report
```

## 📊 Output Format

### Enhanced Mode Output

**JSON Format** (`output/analysis_YYYYMMDD_HHMMSS.json`):
```json
{
  "file_path": "program.abap",
  "line_number": 123,
  "status": "Match with SY-UNAME",
  "operation": "INSERT",
  "table": "ZTABLE",
  "confidence": 0.95,
  "tainted_variables": ["LV_USER", "LS_REC-CREATED_BY"],
  "advanced_patterns": [
    {
      "pattern_id": "EXEC_SQL_01",
      "operation_type": "EXEC_SQL",
      "confidence": 0.95
    }
  ],
  "context_info": {
    "scope": "FORM:UPDATE_USER",
    "inter_procedural_flows": ["PERFORM_UPDATE"]
  }
}
```

**CSV Format** (`output/analysis_YYYYMMDD_HHMMSS.csv`):
- file_path
- line_number
- status
- operation
- table
- confidence
- tainted_variables
- advanced_patterns (count)
- context_scope

## 🔬 Advanced Pattern Examples

### EXEC SQL Detection
```abap
EXEC SQL.
  INSERT INTO ztable (user_id, created_by)
  VALUES (:lv_user, :sy-uname)
ENDEXEC.
```

### CDS View Operations
```abap
SELECT * FROM Z_USER_CDS( )
  WHERE created_by = @sy-uname
  INTO TABLE @DATA(lt_data).
```

### Authority Checks
```abap
AUTHORITY-CHECK OBJECT 'S_USER_GRP'
  ID 'ACTVT' FIELD '03'
  ID 'CLASS' FIELD sy-uname.
```

### Enhanced BAPI Tracking
```abap
CALL FUNCTION 'BAPI_USER_CREATE'
  EXPORTING
    username = sy-uname
    password = lv_password.
```

## 📈 Performance Metrics

Enhanced mode provides detailed performance metrics:

```
⚡ Performance Metrics:
  • total_time: 0.0025s
  • pattern_time: 0.0019s (74.48%)
  • taint_time: 0.0004s (14.39%)
  • cache_hit_rate: 85.30%
  • cache_size: 45
```

## 🧪 Testing

### Run Complete Test Suite
```bash
python3 main.py test
```

### Test Specific Patterns
```bash
# Test enhanced analyzer independently
python3 test_enhanced_analyzer.py

# Test specific pattern types
python3 tests/test_ultimate_handler.py I01  # Test INSERT pattern 01
```

## 📁 Project Structure

```
abap-tracker/
├── main.py                    # Main entry point with integrated analyzers
├── README.md                  # This file
├── CLAUDE.md                 # AI assistant instructions
├── src/
│   ├── core/
│   │   ├── enhanced_analyzer.py    # Enhanced analyzer with optimizations
│   │   ├── final_handler.py        # Core DB operation handler
│   │   ├── csv_analyzer.py         # CSV processing
│   │   └── unified_analyzer.py     # Legacy analyzer
│   ├── patterns/
│   │   ├── advanced_patterns.py    # Advanced ABAP patterns
│   │   ├── enhanced_insert_handler.py
│   │   ├── enhanced_update_handler.py
│   │   └── enhanced_modify_handler.py
│   └── utils/
│       ├── context_aware_taint.py  # Context-aware taint tracking
│       ├── taint_propagation.py    # Taint propagation optimizer
│       └── encoding_utils.py       # File encoding utilities
├── input/                    # Input CSV and ABAP files
│   └── sy_uname_locations.csv
├── output/                   # Analysis results
└── tests/                    # Test files and test suite
```

## 🎯 Use Cases

1. **Security Auditing**: Track user data flow in database operations
2. **Compliance Checking**: Ensure proper user tracking in audit tables
3. **Code Review**: Identify potential security vulnerabilities
4. **Data Lineage**: Trace system variable propagation
5. **Legacy Code Analysis**: Understand complex ABAP codebases

## 🔧 Configuration

### Window Size Optimization

For different file sizes, adjust the window parameter:

- Small files (< 1000 lines): `--window 30`
- Medium files (1000-5000 lines): `--window 50` (default)
- Large files (> 5000 lines): `--window 100`
- Very large files: `--window 200 --no-cache`

### Memory vs Speed Trade-off

- **Maximum Speed**: `--enhanced --window 30`
- **Balanced**: `--enhanced` (default settings)
- **Maximum Accuracy**: `--enhanced --window 200 --no-cache`

## 🐛 Troubleshooting

### Enhanced mode not available
```bash
⚠️  Enhanced mode modules not found. Using standard analyzer.
```
**Solution**: Ensure all files in `src/` directory are present

### File encoding issues
```bash
❌ ERROR: 'utf-8' codec can't decode byte...
```
**Solution**: The tool automatically handles multiple encodings (UTF-8, CP1252, Latin-1)

### Memory issues with large files
```bash
python3 main.py analyze --enhanced --window 30 --no-cache
```

## 📊 Comparison: Standard vs Enhanced Mode

| Feature | Standard | Enhanced |
|---------|----------|----------|
| Basic DB Operations | ✅ | ✅ |
| EXEC SQL | ❌ | ✅ |
| CDS Views | ❌ | ✅ |
| Authority Checks | ❌ | ✅ |
| Context Tracking | Basic | Advanced |
| Performance | Good | Optimized |
| Memory Usage | Higher | Lower |
| Pattern Count | 70+ | 100+ |
| Taint Tracking | Basic | Context-aware |
| Caching | ❌ | ✅ |

## 🤝 Contributing

Contributions are welcome! Areas for improvement:

1. Additional ABAP patterns
2. Machine learning integration
3. GUI interface
4. IDE plugins
5. Cloud deployment options

## 📝 License

MIT License - See LICENSE file for details

## 🙏 Acknowledgments

- SAP ABAP documentation
- Open source ABAP community
- Pattern detection research papers

## 📧 Support

For issues, questions, or suggestions:
- Create an issue on GitHub
- Check existing documentation in `/docs`
- Review test cases in `/tests`

---

**Version**: 2.0.0 (Enhanced Edition)
**Last Updated**: 2024
**Python Required**: 3.6+