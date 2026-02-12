import json
import jsonschema
import sys
import os

def validate_json(json_file, schema_file):
    try:
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        with open(schema_file, 'r') as f:
            schema = json.load(f)

        jsonschema.validate(instance=data, schema=schema)
        print(f"✅ Validation successful: {json_file} matches schema.")
        return 0
    except json.JSONDecodeError as e:
        print(f"❌ Invalid JSON in {json_file}: {e}")
        return 1
    except jsonschema.exceptions.ValidationError as e:
        print(f"❌ Validation failed for {json_file}:")
        print(f"  Message: {e.message}")
        print(f"  Path: {list(e.path)}")
        return 1
    except FileNotFoundError as e:
        print(f"❌ File not found: {e.filename}")
        return 1
    except Exception as e:
        print(f"❌ An error occurred: {e}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python validate_json.py <json_file> <schema_file>")
        sys.exit(1)
    
    sys.exit(validate_json(sys.argv[1], sys.argv[2]))
