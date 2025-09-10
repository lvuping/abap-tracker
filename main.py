#!/usr/bin/env python3
"""
SAP ABAP SY-UNAME 추적기 - 메인 실행 파일
실제 CSV 파일을 대상으로 SY-UNAME 추적 분석을 수행합니다.

사용법:
    python main.py                    # 기본 실행 (JSON + CSV 출력)
    python main.py --csv-only        # CSV만 출력
    python main.py --json-only       # JSON만 출력
    python main.py -i my_data.csv    # 특정 CSV 파일 사용
    python main.py --help            # 도움말 표시
"""

import os
import sys
import json
import csv
import argparse
from datetime import datetime
from typing import List, Dict, Any

# 프로젝트 모듈
from analyzer import trace_sy_uname_in_snippet
from encoding_utils import safe_file_read


class SYUNameTracker:
    """SY-UNAME 추적 및 분석 클래스"""
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.results = []
        
    def analyze_csv(self, input_file: str) -> bool:
        """
        CSV 파일을 읽고 SY-UNAME 추적 분석 수행
        
        CSV 형식:
            file_path,line_number
            example.abap,45
            another.abap,123
        """
        if not os.path.exists(input_file):
            self._print_error(f"입력 파일을 찾을 수 없습니다: {input_file}")
            self._print_csv_format_help()
            return False
        
        print(f"\n🔍 SY-UNAME 추적 분석 시작")
        print(f"📁 입력 파일: {input_file}")
        print("=" * 80)
        
        # CSV 파일 읽기
        try:
            lines, encoding = safe_file_read(input_file)
            if self.verbose:
                print(f"📝 파일 인코딩: {encoding}")
            
            # CSV 파싱
            import io
            csv_content = io.StringIO(''.join(lines))
            reader = csv.DictReader(csv_content)
            
            # 헤더 검증
            if not self._validate_csv_headers(reader.fieldnames):
                return False
            
            # 각 행 처리
            rows = list(reader)
            total = len(rows)
            
            if total == 0:
                self._print_warning("처리할 데이터가 없습니다.")
                return False
            
            print(f"📊 총 {total}개 항목 발견\n")
            
            # 분석 수행
            for idx, row in enumerate(rows, 1):
                result = self._analyze_entry(row, idx, total)
                self.results.append(result)
            
            return True
            
        except Exception as e:
            self._print_error(f"CSV 파일 처리 중 오류: {str(e)}")
            return False
    
    def _analyze_entry(self, row: Dict, idx: int, total: int) -> Dict:
        """단일 CSV 엔트리 분석"""
        try:
            # 파일 경로 및 라인 번호 추출
            file_path = row['file_path'].strip()
            line_number = int(row['line_number'].strip())
            
            # 경로 정규화
            if not file_path.endswith('.abap'):
                file_path += '.abap'
            if not os.path.isabs(file_path) and not file_path.startswith('input/'):
                file_path = 'input/' + file_path
            
            print(f"📍 [{idx}/{total}] {os.path.basename(file_path)} (라인 {line_number})")
            
            # ABAP 파일 확인 및 읽기
            if not os.path.exists(file_path):
                self._print_error(f"   파일을 찾을 수 없음: {file_path}", indent=True)
                return {
                    'file': file_path,
                    'line': line_number,
                    'status': 'error',
                    'error': 'File not found'
                }
            
            # 파일 내용 읽기
            lines, _ = safe_file_read(file_path)
            
            # SY-UNAME 추적 분석
            analysis = trace_sy_uname_in_snippet(lines, line_number - 1)
            
            # 결과 출력
            self._print_analysis_result(analysis)
            
            return {
                'file': file_path,
                'line': line_number,
                'status': 'success',
                'analysis': analysis
            }
            
        except Exception as e:
            self._print_error(f"   분석 중 오류: {str(e)}", indent=True)
            return {
                'file': row.get('file_path', 'unknown'),
                'line': row.get('line_number', 0),
                'status': 'error',
                'error': str(e)
            }
    
    def save_results(self, output_format: str = "both"):
        """분석 결과를 파일로 저장"""
        os.makedirs("output", exist_ok=True)
        
        saved_files = []
        
        # JSON 출력
        if output_format in ["json", "both"]:
            json_file = "output/analysis_results.json"
            self._save_json(json_file)
            saved_files.append(json_file)
        
        # CSV 출력
        if output_format in ["csv", "both"]:
            csv_file = "output/sy_uname_analysis_results.csv"
            self._save_csv(csv_file)
            saved_files.append(csv_file)
        
        # 결과 파일 안내
        print("\n" + "=" * 80)
        print("✅ 분석 완료!")
        print("📁 결과 파일:")
        for file in saved_files:
            print(f"   - {file}")
        
        return saved_files
    
    def _save_json(self, filename: str):
        """JSON 형식으로 저장"""
        output = {
            'timestamp': datetime.now().isoformat(),
            'total_analyzed': len(self.results),
            'results': self.results
        }
        
        with open(filename, 'w', encoding='utf-8') as f:
            json.dump(output, f, ensure_ascii=False, indent=2)
    
    def _save_csv(self, filename: str):
        """CSV 형식으로 저장 (Excel 호환)"""
        with open(filename, 'w', newline='', encoding='utf-8-sig') as f:
            fieldnames = [
                'ID', 'Source_File', 'SY_UNAME_Line', 'Status',
                'Final_Table', 'Final_Fields', 'Operation', 
                'RFC_Name', 'RFC_Parameter', 'Trace_Path'
            ]
            
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for idx, result in enumerate(self.results, 1):
                row = {
                    'ID': idx,
                    'Source_File': os.path.basename(result['file']),
                    'SY_UNAME_Line': result['line'],
                    'Status': result['status']
                }
                
                if result['status'] == 'success' and 'analysis' in result:
                    analysis = result['analysis']
                    row.update({
                        'Final_Table': analysis.get('table', ''),
                        'Final_Fields': ', '.join(analysis.get('fields', [])) if analysis.get('fields') else '',
                        'Operation': analysis.get('operation', ''),
                        'RFC_Name': analysis.get('rfc_function', ''),
                        'RFC_Parameter': analysis.get('rfc_parameter', ''),
                        'Trace_Path': ' → '.join(analysis.get('trace', [])) if analysis.get('trace') else ''
                    })
                elif result['status'] == 'error':
                    row['Status'] = f"Error: {result.get('error', 'Unknown')}"
                
                writer.writerow(row)
    
    def _print_analysis_result(self, analysis: Dict):
        """분석 결과를 콘솔에 출력"""
        if analysis.get('status') == 'Found':
            if analysis.get('type') == 'RFC_PARAMETER':
                print(f"   🎯 RFC 호출 감지: {analysis.get('rfc_function')}")
                print(f"      파라미터: {analysis.get('rfc_parameter')}")
            elif analysis.get('table'):
                print(f"   🎯 데이터베이스 감지: {analysis.get('table')}")
                print(f"      필드: {', '.join(analysis.get('fields', []))}")
                print(f"      작업: {analysis.get('operation')}")
        elif analysis.get('status') == 'Scope boundary':
            print(f"   ⚠️  스코프 경계 도달")
        else:
            print(f"   ℹ️  Sink 없음")
    
    def _validate_csv_headers(self, headers: List[str]) -> bool:
        """CSV 헤더 검증"""
        if not headers:
            self._print_error("CSV 파일에 헤더가 없습니다.")
            return False
        
        required = ['file_path', 'line_number']
        missing = [h for h in required if h not in headers]
        
        if missing:
            self._print_error(f"필수 컬럼이 없습니다: {', '.join(missing)}")
            self._print_csv_format_help()
            return False
        
        return True
    
    def _print_csv_format_help(self):
        """CSV 형식 도움말"""
        print("\n📋 올바른 CSV 파일 형식:")
        print("   file_path,line_number")
        print("   example_file.abap,45")
        print("   another_file.abap,123")
        print("\n💡 팁:")
        print("   - .abap 확장자는 자동으로 추가됩니다")
        print("   - input/ 경로도 자동으로 추가됩니다")
    
    def _print_error(self, msg: str, indent: bool = False):
        """에러 메시지 출력"""
        prefix = "   " if indent else ""
        print(f"{prefix}❌ {msg}")
    
    def _print_warning(self, msg: str, indent: bool = False):
        """경고 메시지 출력"""
        prefix = "   " if indent else ""
        print(f"{prefix}⚠️  {msg}")


def main():
    """메인 실행 함수"""
    parser = argparse.ArgumentParser(
        description='SAP ABAP SY-UNAME 추적기',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
📋 CSV 파일 형식:
  file_path,line_number
  example.abap,45
  another.abap,123

🎯 사용 예시:
  python main.py                              # 기본 실행 (JSON + CSV)
  python main.py --csv-only                   # CSV만 출력
  python main.py --json-only                  # JSON만 출력
  python main.py -i my_data.csv               # 다른 CSV 파일 사용
  python main.py -v                           # 상세 출력
        """
    )
    
    # 명령행 인자 정의
    parser.add_argument('-i', '--input',
                        default='input/sy_uname_locations.csv',
                        help='입력 CSV 파일 경로 (기본: input/sy_uname_locations.csv)')
    
    # 출력 형식 옵션 (상호 배타적)
    format_group = parser.add_mutually_exclusive_group()
    format_group.add_argument('--csv-only',
                              action='store_const',
                              const='csv',
                              dest='format',
                              help='CSV 형식으로만 출력')
    format_group.add_argument('--json-only',
                              action='store_const',
                              const='json',
                              dest='format',
                              help='JSON 형식으로만 출력')
    
    parser.add_argument('-v', '--verbose',
                        action='store_true',
                        help='상세 출력 모드')
    
    # 기본값 설정
    parser.set_defaults(format='both')
    
    # 인자 파싱
    args = parser.parse_args()
    
    # 추적기 실행
    tracker = SYUNameTracker(verbose=args.verbose)
    
    # CSV 분석
    if tracker.analyze_csv(args.input):
        # 결과 저장
        tracker.save_results(args.format)
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == '__main__':
    main()