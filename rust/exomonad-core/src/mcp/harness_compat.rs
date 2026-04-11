//! JSON coercion layer for MCP tool arguments.
//!
//! Some MCP harnesses serialize every tool argument as a JSON String,
//! regardless of the declared type in the tool's schema. This module
//! detects such encoded values and converts them back to their native
//! JSON types before the arguments reach the WASM plugin.

use serde_json::Value;

/// Recursively walks a JSON value and coerces any String that looks like
/// a JSON-encoded non-string value (number, bool, array, object) back to
/// its native type. Nested structures are coerced after decoding so
/// encoded values inside encoded arrays are also normalized.
///
/// If `coerce_scalars` is true, strings matching Number or Bool shapes
/// are also coerced. If false, only Array and Object shapes are coerced.
pub fn coerce_harness_value(value: Value, coerce_scalars: bool) -> Value {
    match value {
        Value::Object(obj) => Value::Object(
            obj.into_iter()
                .map(|(k, v)| (k, coerce_harness_value(v, coerce_scalars)))
                .collect(),
        ),
        Value::Array(arr) => Value::Array(
            arr.into_iter()
                .map(|v| coerce_harness_value(v, coerce_scalars))
                .collect(),
        ),
        Value::String(s) => {
            decode_if_harness_encoded(&s, coerce_scalars).unwrap_or(Value::String(s))
        }
        other => other,
    }
}

/// If `s` looks like a JSON-encoded non-string value (number, bool, array,
/// object), parse it and return the decoded value recursively coerced.
/// Returns `None` if the string doesn't match any encoded shape or if
/// decoding fails, preserving the original string.
fn decode_if_harness_encoded(s: &str, coerce_scalars: bool) -> Option<Value> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return None;
    }

    let shape = HarnessShape::detect(trimmed)?;

    // Gate scalar coercion if disabled.
    if !coerce_scalars && matches!(shape, HarnessShape::Number | HarnessShape::Bool) {
        return None;
    }

    let decoded: Value = serde_json::from_str(trimmed).ok()?;

    match (shape, &decoded) {
        (HarnessShape::Number, Value::Number(_))
        | (HarnessShape::Bool, Value::Bool(_))
        | (HarnessShape::Array, Value::Array(_))
        | (HarnessShape::Object, Value::Object(_)) => {
            Some(coerce_harness_value(decoded, coerce_scalars))
        }
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HarnessShape {
    Number,
    Bool,
    Array,
    Object,
}

impl HarnessShape {
    fn detect(s: &str) -> Option<Self> {
        if s == "true" || s == "false" {
            return Some(Self::Bool);
        }
        if s.starts_with('[') {
            return Some(Self::Array);
        }
        if s.starts_with('{') {
            return Some(Self::Object);
        }
        if Self::is_json_number(s) {
            return Some(Self::Number);
        }
        None
    }

    fn is_json_number(s: &str) -> bool {
        let bytes = s.as_bytes();
        if bytes.is_empty() {
            return false;
        }

        let mut i = 0;

        if bytes[i] == b'-' {
            i += 1;
            if i == bytes.len() {
                return false;
            }
        }

        match bytes[i] {
            b'0' => {
                i += 1;
            }
            b'1'..=b'9' => {
                i += 1;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
            }
            _ => return false,
        }

        if i < bytes.len() && bytes[i] == b'.' {
            i += 1;
            let frac_start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i == frac_start {
                return false;
            }
        }

        if i < bytes.len() && matches!(bytes[i], b'e' | b'E') {
            i += 1;
            if i < bytes.len() && matches!(bytes[i], b'+' | b'-') {
                i += 1;
            }
            let exp_start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i == exp_start {
                return false;
            }
        }

        i == bytes.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_detect_all_shapes() {
        // Bool
        assert_eq!(HarnessShape::detect("true"), Some(HarnessShape::Bool));
        assert_eq!(HarnessShape::detect("false"), Some(HarnessShape::Bool));
        assert_eq!(HarnessShape::detect("True"), None);
        assert_eq!(HarnessShape::detect("TRUE"), None);
        assert_eq!(HarnessShape::detect("False"), None);
        assert_eq!(HarnessShape::detect("FALSE"), None);

        // Number
        assert_eq!(HarnessShape::detect("1"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("0"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("-1"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("123456"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("1.5"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("-1.5"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("1e10"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("1.5e-3"), Some(HarnessShape::Number));
        assert_eq!(HarnessShape::detect("1E10"), Some(HarnessShape::Number));

        // Array
        assert_eq!(HarnessShape::detect("[]"), Some(HarnessShape::Array));
        assert_eq!(HarnessShape::detect("[1,2]"), Some(HarnessShape::Array));
        assert_eq!(HarnessShape::detect("[ 1 , 2 ]"), Some(HarnessShape::Array));

        // Object
        assert_eq!(HarnessShape::detect("{}"), Some(HarnessShape::Object));
        assert_eq!(
            HarnessShape::detect("{\"k\":1}"),
            Some(HarnessShape::Object)
        );

        // None
        assert_eq!(HarnessShape::detect(""), None);
        assert_eq!(HarnessShape::detect("abc"), None);
        assert_eq!(HarnessShape::detect("1abc"), None);
        assert_eq!(HarnessShape::detect("hello world"), None);
        assert_eq!(HarnessShape::detect("truthy"), None);
        assert_eq!(HarnessShape::detect("falsely"), None);
        assert_eq!(HarnessShape::detect("null"), None);
        assert_eq!(HarnessShape::detect("2026-04-10"), None);
        assert_eq!(HarnessShape::detect("-"), None);
        assert_eq!(HarnessShape::detect("+"), None);
        assert_eq!(HarnessShape::detect("."), None);

        // Prefix/Suffix only detection (decoding may still fail)
        assert_eq!(HarnessShape::detect("["), Some(HarnessShape::Array));
        assert_eq!(HarnessShape::detect("]"), None); // doesn't start with [
        assert_eq!(
            HarnessShape::detect("[incomplete"),
            Some(HarnessShape::Array)
        );
        assert_eq!(HarnessShape::detect("{broken"), Some(HarnessShape::Object));
    }

    #[test]
    fn test_decode_numbers() {
        assert_eq!(decode_if_harness_encoded("841", true), Some(json!(841)));
        assert_eq!(decode_if_harness_encoded("-5", true), Some(json!(-5)));
        assert_eq!(decode_if_harness_encoded("1.5", true), Some(json!(1.5)));
        assert_eq!(decode_if_harness_encoded("0", true), Some(json!(0)));
    }

    #[test]
    fn test_decode_bools() {
        assert_eq!(decode_if_harness_encoded("true", true), Some(json!(true)));
        assert_eq!(decode_if_harness_encoded("false", true), Some(json!(false)));
    }

    #[test]
    fn test_decode_arrays() {
        assert_eq!(
            decode_if_harness_encoded("[1,2]", true),
            Some(json!([1, 2]))
        );
        assert_eq!(decode_if_harness_encoded("[]", true), Some(json!([])));
        assert_eq!(
            decode_if_harness_encoded("[\"a\",\"b\"]", true),
            Some(json!(["a", "b"]))
        );
    }

    #[test]
    fn test_decode_objects() {
        assert_eq!(
            decode_if_harness_encoded("{\"k\":1}", true),
            Some(json!({"k": 1}))
        );
        assert_eq!(decode_if_harness_encoded("{}", true), Some(json!({})));
    }

    #[test]
    fn test_decode_nested_coercion() {
        assert_eq!(
            decode_if_harness_encoded("[\"1\",\"2\"]", true),
            Some(json!([1, 2]))
        );
        assert_eq!(
            decode_if_harness_encoded("[1,\"true\"]", true),
            Some(json!([1, true]))
        );
        assert_eq!(
            decode_if_harness_encoded("[[\"1\"]]", true),
            Some(json!([[1]]))
        );
        assert_eq!(
            decode_if_harness_encoded("{\"n\":\"42\"}", true),
            Some(json!({"n": 42}))
        );
    }

    #[test]
    fn test_decode_rejects_unshaped() {
        assert_eq!(decode_if_harness_encoded("hello", true), None);
        assert_eq!(decode_if_harness_encoded("my-slug", true), None);
        // "2026-04-10" matches Number shape (starts with digit, only contains digits/dashes)
        // but it should fail to decode as a JSON number.
        assert_eq!(decode_if_harness_encoded("2026-04-10", true), None);
        assert_eq!(decode_if_harness_encoded("", true), None);
    }

    #[test]
    fn test_decode_rejects_malformed() {
        assert_eq!(decode_if_harness_encoded("[incomplete", true), None);
        assert_eq!(decode_if_harness_encoded("{broken", true), None);
    }

    #[test]
    fn test_decode_whitespace_tolerant() {
        assert_eq!(decode_if_harness_encoded(" 841 ", true), Some(json!(841)));
    }

    #[test]
    fn test_decode_null_passthrough() {
        assert_eq!(decode_if_harness_encoded("null", true), None);
    }

    #[test]
    fn test_coerce_primitive_passthrough() {
        assert_eq!(coerce_harness_value(json!(5), true), json!(5));
        assert_eq!(coerce_harness_value(json!(true), true), json!(true));
        assert_eq!(coerce_harness_value(Value::Null, true), Value::Null);
    }

    #[test]
    fn test_coerce_plain_string_passthrough() {
        assert_eq!(coerce_harness_value(json!("hello"), true), json!("hello"));
        assert_eq!(
            coerce_harness_value(json!("my-agent-name"), true),
            json!("my-agent-name")
        );
    }

    #[test]
    fn test_coerce_encoded_scalar_in_string() {
        assert_eq!(coerce_harness_value(json!("841"), true), json!(841));
        assert_eq!(coerce_harness_value(json!("true"), true), json!(true));
    }

    #[test]
    fn test_coerce_object_with_mixed_fields() {
        let input = json!({
            "pr_number": "841",
            "force": "true",
            "strategy": "squash",
            "task": "do the thing"
        });
        let expected = json!({
            "pr_number": 841,
            "force": true,
            "strategy": "squash",
            "task": "do the thing"
        });
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_coerce_nested_object() {
        let input = json!({"outer": {"inner": "[1,2]"}});
        let expected = json!({"outer": {"inner": [1, 2]}});
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_coerce_array_of_encoded_strings() {
        let input = json!(["1", "2", "3"]);
        let expected = json!([1, 2, 3]);
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_coerce_mixed_array() {
        let input = json!([1, "2", true, "false", "[1,2]"]);
        let expected = json!([1, 2, true, false, [1, 2]]);
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_coerce_empty_containers() {
        assert_eq!(coerce_harness_value(json!({}), true), json!({}));
        assert_eq!(coerce_harness_value(json!([]), true), json!([]));
    }

    #[test]
    fn test_coerce_null_preserved() {
        assert_eq!(
            coerce_harness_value(json!({"k": null}), true),
            json!({"k": null})
        );
        assert_eq!(coerce_harness_value(json!("null"), true), json!("null"));
    }

    #[test]
    fn test_coerce_deeply_nested() {
        let input = json!({"a": {"b": {"c": "[{\"k\":\"5\"}]"}}});
        let expected = json!({"a": {"b": {"c": [{"k": 5}]}}});
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_coerce_preserves_existing_native_types() {
        let input = json!({"n": 841, "b": true, "arr": [1, 2]});
        assert_eq!(coerce_harness_value(input.clone(), true), input);
    }

    #[test]
    fn test_merge_pr_args_from_harness() {
        let input = json!({"pr_number": "841", "force": "true"});
        let expected = json!({"pr_number": 841, "force": true});
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_merge_pr_args_already_native() {
        let input = json!({"pr_number": 841, "force": true});
        assert_eq!(coerce_harness_value(input.clone(), true), input);
    }

    #[test]
    fn test_fork_wave_children_array_from_harness() {
        let input = json!({"children": "[{\"slug\":\"a\",\"task\":\"t1\"},{\"slug\":\"b\",\"task\":\"t2\"}]"});
        let expected = json!({
            "children": [
                {"slug": "a", "task": "t1"},
                {"slug": "b", "task": "t2"}
            ]
        });
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_spawn_gemini_steps_array_from_harness() {
        let input = json!({"name": "foo", "steps": "[\"step1\",\"step2\"]", "task": "do X"});
        let expected = json!({
            "name": "foo",
            "steps": ["step1", "step2"],
            "task": "do X"
        });
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_known_false_positive_string_field_with_digit_value() {
        // Known limitation: the coercer is not schema-directed so a String field
        // whose value is all digits will be turned into a Number.
        let input = json!({"slug": "123"});
        let expected = json!({"slug": 123});
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_known_false_positive_string_field_with_bool_value() {
        // Known limitation: same as above for booleans.
        let input = json!({"task": "true"});
        let expected = json!({"task": true});
        assert_eq!(coerce_harness_value(input, true), expected);
    }

    #[test]
    fn test_coerce_is_idempotent() {
        let input = json!({"a": {"b": {"c": "[{\"k\":\"5\"}]"}}});
        let first = coerce_harness_value(input, true);
        let second = coerce_harness_value(first.clone(), true);
        assert_eq!(first, second);
    }

    #[test]
    fn test_coerce_preserves_value_for_purely_native_input() {
        let input = json!({
            "a": "hello",
            "b": "my-agent-123z",
            "c": 123,
            "d": true,
            "e": null,
            "f": [1, 2, 3],
            "g": {"h": "i"}
        });
        assert_eq!(coerce_harness_value(input.clone(), true), input);
    }

    #[test]
    fn test_coerce_gated_scalars() {
        let input = json!({
            "pr_number": "841",
            "force": "true",
            "children": "[{\"id\":\"123\"}]"
        });
        // With coerce_scalars=false, pr_number and force stay as strings.
        // But the children array is still coerced, AND its inner strings are coerced
        // because we passed coerce_scalars down. Wait, should we?
        // If we want to be safe, inner strings should ALSO be preserved.
        let expected = json!({
            "pr_number": "841",
            "force": "true",
            "children": [{"id": "123"}]
        });
        assert_eq!(coerce_harness_value(input, false), expected);
    }
}
