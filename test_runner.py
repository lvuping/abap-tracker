#!/usr/bin/env python3
"""
ABAP SY-UNAME Tracker - 통합 테스트 실행기
모든 테스트 관련 기능을 통합한 단일 실행 파일
"""

import os
import sys
import json
import csv
from datetime import datetime
from typing import List, Dict, Any, Optional

# 프로젝트 모듈 임포트
from analyzer import trace_sy_uname_in_snippet
from encoding_utils import safe_file_read


class TestRunner:
    """테스트 실행 및 결과 관리 클래스"""
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.results = []
        self.summary = {
            'total_tests': 0,
            'successful': 0,
            'failed': 0,
            'warnings': 0
        }
    
    def run_csv_analysis(self, csv_file: str = "input/sy_uname_locations.csv") -> Dict[str, Any]:
        """
        CSV 파일 기반 SY-UNAME 분석 실행
        
        Args:
            csv_file: 입력 CSV 파일 경로 (형식: file_path,line_number)
        
        Returns:
            분석 결과 딕셔너리
        """
        if not os.path.exists(csv_file):
            print(f"❌ 오류: {csv_file} 파일을 찾을 수 없습니다.")
            self._print_csv_format_help()
            return None
        
        print(f"🔍 CSV 기반 SY-UNAME 추적 분석 시작")
        print(f"📁 입력 파일: {csv_file}")
        print("=" * 80)
        
        # CSV 파일 읽기
        lines, encoding = safe_file_read(csv_file)
        
        if self.verbose:
            print(f"📝 파일 인코딩: {encoding}")
        
        # CSV 파싱
        import io
        csv_content = io.StringIO(''.join(lines))
        reader = csv.DictReader(csv_content)
        
        # 결과 처리
        for idx, row in enumerate(reader, 1):
            self.summary['total_tests'] += 1
            result = self._analyze_single_entry(row, idx)
            self.results.append(result)
            
            if result['status'] == 'success':
                self.summary['successful'] += 1
            elif result['status'] == 'error':
                self.summary['failed'] += 1
            else:
                self.summary['warnings'] += 1
        
        return {
            'results': self.results,
            'summary': self.summary,
            'timestamp': datetime.now().isoformat()
        }
    
    def _analyze_single_entry(self, row: Dict[str, str], idx: int) -> Dict[str, Any]:
        """단일 CSV 엔트리 분석"""
        try:
            file_path = row['file_path'].strip()
            line_number = int(row['line_number'].strip())
            
            # 파일 경로 정규화
            if not file_path.endswith('.abap'):
                file_path += '.abap'
            if not file_path.startswith('input/'):
                file_path = 'input/' + file_path
            
            print(f"📍 [{idx}/{self.summary['total_tests']}] 분석 중: {os.path.basename(file_path)} (라인 {line_number})")
            
            # ABAP 파일 읽기
            if not os.path.exists(file_path):
                print(f"   ❌ 파일 없음: {file_path}")
                return {
                    'file': file_path,
                    'line': line_number,
                    'status': 'error',
                    'message': 'File not found'
                }
            
            # 파일 내용 읽기 및 분석
            lines, _ = safe_file_read(file_path)
            
            # SY-UNAME 추적 분석
            result = trace_sy_uname_in_snippet(lines, line_number - 1)
            
            # 결과 정리
            return {
                'file': file_path,
                'line': line_number,
                'status': 'success',
                'analysis': result
            }
            
        except Exception as e:
            print(f"   ❌ 오류 발생: {str(e)}")
            return {
                'file': row.get('file_path', 'unknown'),
                'line': row.get('line_number', 0),
                'status': 'error',
                'message': str(e)
            }
    
    def save_results(self, output_format: str = "both"):
        """
        분석 결과를 파일로 저장
        
        Args:
            output_format: "json", "csv", "both" 중 하나
        """
        os.makedirs("output", exist_ok=True)
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        if output_format in ["json", "both"]:
            json_file = f"output/analysis_results_{timestamp}.json"
            with open(json_file, 'w', encoding='utf-8') as f:
                json.dump({
                    'results': self.results,
                    'summary': self.summary,
                    'timestamp': datetime.now().isoformat()
                }, f, ensure_ascii=False, indent=2)
            print(f"✅ JSON 결과 저장: {json_file}")
        
        if output_format in ["csv", "both"]:
            csv_file = f"output/analysis_results_{timestamp}.csv"
            self._save_csv_results(csv_file)
            print(f"✅ CSV 결과 저장: {csv_file}")
    
    def _save_csv_results(self, csv_file: str):
        """CSV 형식으로 결과 저장"""
        with open(csv_file, 'w', newline='', encoding='utf-8-sig') as f:
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
                        'Final_Fields': ', '.join(analysis.get('fields', [])),
                        'Operation': analysis.get('operation', ''),
                        'RFC_Name': analysis.get('rfc_function', ''),
                        'RFC_Parameter': analysis.get('rfc_parameter', ''),
                        'Trace_Path': ' → '.join(analysis.get('trace', []))
                    })
                
                writer.writerow(row)
    
    def print_summary(self):
        """분석 요약 출력"""
        print("\n" + "=" * 80)
        print("📊 분석 요약")
        print("=" * 80)
        print(f"총 테스트: {self.summary['total_tests']}")
        print(f"✅ 성공: {self.summary['successful']}")
        print(f"❌ 실패: {self.summary['failed']}")
        print(f"⚠️  경고: {self.summary['warnings']}")
        
        success_rate = (self.summary['successful'] / self.summary['total_tests'] * 100) if self.summary['total_tests'] > 0 else 0
        print(f"성공률: {success_rate:.1f}%")
    
    def _print_csv_format_help(self):
        """CSV 형식 도움말 출력"""
        print("\n📋 CSV 파일 형식:")
        print("   file_path,line_number")
        print("   example_file.abap,45")
        print("   another_file.abap,123")
        print("\n💡 팁: 파일명에 .abap 확장자는 자동으로 추가됩니다.")
        print("   input/ 경로도 자동으로 추가됩니다.")


def main():
    """메인 실행 함수"""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='ABAP SY-UNAME Tracker - 테스트 실행기',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
사용 예시:
  python test_runner.py                              # 기본 CSV 파일로 실행
  python test_runner.py -i input/my_tests.csv        # 특정 CSV 파일 사용
  python test_runner.py --format csv                 # CSV만 출력
  python test_runner.py --format json                # JSON만 출력
  python test_runner.py --verbose                    # 상세 출력
        """
    )
    
    parser.add_argument('-i', '--input', 
                        default='input/sy_uname_locations.csv',
                        help='입력 CSV 파일 경로')
    parser.add_argument('--format', 
                        choices=['json', 'csv', 'both'],
                        default='both',
                        help='출력 형식 (기본: both)')
    parser.add_argument('-v', '--verbose', 
                        action='store_true',
                        help='상세 출력 모드')
    
    args = parser.parse_args()
    
    # 테스트 실행
    runner = TestRunner(verbose=args.verbose)
    results = runner.run_csv_analysis(args.input)
    
    if results:
        runner.save_results(args.format)
        runner.print_summary()
        sys.exit(0 if runner.summary['failed'] == 0 else 1)
    else:
        sys.exit(1)


if __name__ == '__main__':
    main()