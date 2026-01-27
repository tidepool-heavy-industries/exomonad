#!/usr/bin/env python3
"""
Generate clean skeleton with unique rewrite tokens.
Each example gets unique IDs for criteria and selected symbols.
"""

import json
import sys

def main():
    input_file = 'skeleton-clean.jsonl'
    output_file = 'skeleton-v3-1080-rewrite.jsonl'

    example_id = 1

    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            line = line.strip()
            if not line or not line.startswith('{'):
                continue

            skeleton = json.loads(line)

            # Generate 4 variations per skeleton
            variations = [
                ('positive_structure', True),
                ('positive_semantic', True),
                ('negative_1', False),
                ('negative_2', False)
            ]

            for variation_type, is_positive in variations:
                example = {
                    'example_id': f'{example_id:04d}',
                    'variation': variation_type,
                    'code': skeleton['code'],
                    'criteria': f'CRITERIA_{example_id:04d}',
                    'selected': f'SELECTED_{example_id:04d}',  # LLM will replace with array
                    'file': skeleton['file'],
                    'name': skeleton['name'],
                    'range': skeleton['range']
                }

                outfile.write(json.dumps(example) + '\n')
                example_id += 1

    print(f'Generated {example_id - 1} skeleton examples with rewrite tokens', file=sys.stderr)
    print(f'Output: {output_file}', file=sys.stderr)

if __name__ == '__main__':
    main()
