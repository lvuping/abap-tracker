"""
Encoding detection and handling utilities for ABAP files
Supports CP949 (Korean) and UTF-8 encoding
"""

import chardet
import codecs
import os
from typing import Optional, Tuple, List


def detect_encoding(file_path: str, sample_size: int = 10240) -> str:
    """
    Detect the encoding of a file by analyzing its content.
    
    Args:
        file_path: Path to the file
        sample_size: Number of bytes to sample for detection
        
    Returns:
        Detected encoding name ('utf-8' or 'cp949')
    """
    try:
        # First, try reading a sample of the file
        with open(file_path, 'rb') as f:
            raw_data = f.read(sample_size)
        
        # Use chardet to detect encoding
        result = chardet.detect(raw_data)
        detected_encoding = result.get('encoding', '').lower()
        confidence = result.get('confidence', 0)
        
        # Map common Korean encodings to cp949
        korean_encodings = ['euc-kr', 'cp949', 'ms949', 'ks_c_5601-1987']
        if any(enc in detected_encoding for enc in korean_encodings):
            return 'cp949'
        
        # If UTF-8 is detected with high confidence
        if 'utf-8' in detected_encoding and confidence > 0.7:
            return 'utf-8'
        
        # If ASCII is detected, treat as UTF-8 (ASCII is subset of UTF-8)
        if 'ascii' in detected_encoding:
            return 'utf-8'
        
        # Try decoding with UTF-8 first
        try:
            raw_data.decode('utf-8')
            return 'utf-8'
        except UnicodeDecodeError:
            pass
        
        # Try decoding with CP949
        try:
            raw_data.decode('cp949')
            return 'cp949'
        except UnicodeDecodeError:
            pass
        
        # Default to UTF-8 with error handling
        return 'utf-8'
        
    except Exception as e:
        # If detection fails, default to UTF-8
        print(f"Warning: Could not detect encoding for {file_path}: {e}")
        return 'utf-8'


def read_file_with_encoding(file_path: str, encoding: Optional[str] = None) -> Tuple[List[str], str]:
    """
    Read a file with automatic encoding detection.
    
    Args:
        file_path: Path to the file
        encoding: Optional encoding to use (if None, will auto-detect)
        
    Returns:
        Tuple of (list of lines, encoding used)
    """
    # Auto-detect encoding if not provided
    if encoding is None:
        encoding = detect_encoding(file_path)
    
    try:
        # Try reading with detected encoding
        with open(file_path, 'r', encoding=encoding, errors='replace') as f:
            lines = f.readlines()
        return lines, encoding
    except (UnicodeDecodeError, LookupError) as e:
        # If the detected encoding fails, try the other one
        alternate_encoding = 'cp949' if encoding == 'utf-8' else 'utf-8'
        print(f"Warning: Failed to read {file_path} with {encoding}, trying {alternate_encoding}")
        
        try:
            with open(file_path, 'r', encoding=alternate_encoding, errors='replace') as f:
                lines = f.readlines()
            return lines, alternate_encoding
        except Exception:
            # Last resort: read with UTF-8 and replace errors
            with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
                lines = f.readlines()
            return lines, 'utf-8'


def safe_file_read(file_path: str) -> Tuple[List[str], str]:
    """
    Safely read a file with automatic encoding detection and fallback.
    
    Args:
        file_path: Path to the file
        
    Returns:
        Tuple of (list of lines, encoding used)
    """
    return read_file_with_encoding(file_path)


def convert_file_encoding(input_path: str, output_path: str, 
                         from_encoding: Optional[str] = None, 
                         to_encoding: str = 'utf-8') -> bool:
    """
    Convert a file from one encoding to another.
    
    Args:
        input_path: Path to input file
        output_path: Path to output file
        from_encoding: Source encoding (auto-detect if None)
        to_encoding: Target encoding
        
    Returns:
        True if successful, False otherwise
    """
    try:
        # Read with source encoding
        lines, used_encoding = read_file_with_encoding(input_path, from_encoding)
        
        # Write with target encoding
        with open(output_path, 'w', encoding=to_encoding) as f:
            f.writelines(lines)
        
        print(f"Converted {input_path} from {used_encoding} to {to_encoding}")
        return True
        
    except Exception as e:
        print(f"Error converting {input_path}: {e}")
        return False


def batch_detect_encodings(directory: str, extension: str = '.abap') -> dict:
    """
    Detect encodings for all files with given extension in a directory.
    
    Args:
        directory: Directory to scan
        extension: File extension to check
        
    Returns:
        Dictionary mapping file paths to their detected encodings
    """
    encodings = {}
    
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(extension):
                file_path = os.path.join(root, file)
                encoding = detect_encoding(file_path)
                encodings[file_path] = encoding
    
    return encodings


# Install chardet if not available
def ensure_chardet():
    """Ensure chardet is installed"""
    try:
        import chardet
    except ImportError:
        import subprocess
        import sys
        print("Installing chardet for encoding detection...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "chardet"])


# Run installation check when module is imported
ensure_chardet()