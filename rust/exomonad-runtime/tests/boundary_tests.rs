//! WASM Boundary Tests
//!
//! These tests capture the exact byte-level behavior of Extism's
//! memory_set_val and memory_get_val to serve as ground truth
//! for the Haskell PDK implementation.

use exomonad_runtime::common::HostResult;
use extism::{CurrentPlugin, Function, Manifest, Plugin, UserData, Val, ValType, Wasm};
use extism_convert::Json;
use serde_json;

#[test]
fn test_memory_set_val_byte_layout() {
    // Goal: When host calls memory_set_val(Json(value)), what bytes appear in memory?

    let wasm = r#"
        (module
          (import "env" "get_data" (func $get_data (result i64)))
          (import "env" "probe_data" (func $probe_data (param i64)))
          (func (export "test")
            (call $probe_data (call $get_data))
          )
        )
    "#;

    let host_fn = Function::new(
        "get_data",
        [],
        [ValType::I64],
        UserData::new(()),
        |plugin: &mut CurrentPlugin,
         _inputs: &[Val],
         outputs: &mut [Val],
         _user_data: UserData<()>| {
            let value = "boundary-test";
            // This is what our host functions do
            plugin.memory_set_val(&mut outputs[0], Json(value))?;
            Ok(())
        },
    )
    .with_namespace("env");

    let probe_fn = Function::new(
        "probe_data",
        [ValType::I64],
        [],
        UserData::new(()),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         _outputs: &mut [Val],
         _user_data: UserData<()>| {
            let handle = inputs[0].unwrap_i64();
            let mem = plugin
                .memory_from_val(&Val::I64(handle))
                .expect("Invalid handle in probe");
            let bytes = plugin
                .memory_bytes(mem)
                .expect("Failed to get memory bytes");
            println!("Probed bytes: {:?}", bytes);
            println!("Probed string: {}", String::from_utf8_lossy(bytes));

            // Ground truth assertions
            assert_eq!(bytes, b"\"boundary-test\"");
            Ok(())
        },
    )
    .with_namespace("env");

    let manifest = Manifest::new([Wasm::data(wasm)]);
    let mut plugin =
        Plugin::new(&manifest, vec![host_fn, probe_fn], true).expect("Failed to build plugin");

    // Call WASM function which calls our host functions
    plugin.call::<(), ()>("test", ()).expect("WASM call failed");
}

#[test]
fn test_memory_get_val_roundtrip() {
    // Goal: Confirm that host's memory_get_val can read data allocated by host's memory_set_val.

    let wasm = r#"
        (module
          (import "env" "get_data" (func $get_data (result i64)))
          (import "env" "check_data" (func $check_data (param i64) (result i64)))
          (import "env" "report_result" (func $report_result (param i64)))
          (func (export "test")
            (call $report_result (call $check_data (call $get_data)))
          )
        )
    "#;

    let get_data_fn = Function::new(
        "get_data",
        [],
        [ValType::I64],
        UserData::new(()),
        |plugin: &mut CurrentPlugin,
         _inputs: &[Val],
         outputs: &mut [Val],
         _user_data: UserData<()>| {
            plugin.memory_set_val(&mut outputs[0], Json("guest-data".to_string()))?;
            Ok(())
        },
    )
    .with_namespace("env");

    let check_data_fn = Function::new(
        "check_data",
        [ValType::I64],
        [ValType::I64],
        UserData::new(()),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         _user_data: UserData<()>| {
            let Json(input): Json<String> = plugin.memory_get_val(&inputs[0])?;
            if input == "guest-data" {
                outputs[0] = Val::I64(1);
            } else {
                outputs[0] = Val::I64(0);
            }
            Ok(())
        },
    )
    .with_namespace("env");

    let report_fn = Function::new(
        "report_result",
        [ValType::I64],
        [],
        UserData::new(()),
        |_plugin: &mut CurrentPlugin,
         inputs: &[Val],
         _outputs: &mut [Val],
         _user_data: UserData<()>| {
            let result = inputs[0].unwrap_i64();
            println!("Reported result: {}", result);
            assert_eq!(result, 1);
            Ok(())
        },
    )
    .with_namespace("env");

    let manifest = Manifest::new([Wasm::data(wasm)]);
    let mut plugin = Plugin::new(&manifest, vec![get_data_fn, check_data_fn, report_fn], true)
        .expect("Failed to build plugin");

    plugin.call::<(), ()>("test", ()).expect("WASM call failed");
}

#[test]
fn test_host_result_success_serialization() {
    // Verify HostResult::Success("main") produces exact JSON expected by Haskell
    let result: HostResult<String> = HostResult::Success("main".to_string());
    let json_bytes = serde_json::to_vec(&result).expect("Serialization failed");
    let json_str = String::from_utf8(json_bytes).expect("Invalid UTF-8");

    println!("HostResult::Success JSON: {}", json_str);

    // Exact layout match for Haskell's Aeson instance
    assert_eq!(json_str, r#"{"kind":"Success","payload":"main"}"#);
}

#[test]
fn test_host_result_error_serialization() {
    use exomonad_runtime::common::{ErrorCode, FFIError};

    let err = FFIError {
        message: "file not found".to_string(),
        code: ErrorCode::NotFound,
        context: None,
        suggestion: Some("check path".to_string()),
    };
    let result: HostResult<String> = HostResult::Error(err);
    let json_str = serde_json::to_string(&result).expect("Serialization failed");

    println!("HostResult::Error JSON: {}", json_str);

    // Exact layout match
    assert!(json_str.contains(r#""kind":"Error""#));
    assert!(json_str.contains(r#""message":"file not found""#));
    assert!(json_str.contains(r#""code":"not_found""#));
    assert!(json_str.contains(r#""suggestion":"check path""#));
}

#[test]
fn test_guest_sees_raw_json_no_prefix() {
    // Goal: When guest receives a handle from host, what does it see at that offset?
    // Does it see raw JSON or some length prefix?

    let wasm = r#"
        (module
          (import "env" "get_data" (func $get_data (result i64)))
          (import "env" "report_first_8_bytes" (func $report_first_8_bytes (param i64)))
          (func (export "test")
            (call $report_first_8_bytes (call $get_data))
          )
        )
    "#;

    let get_data_fn = Function::new(
        "get_data",
        [],
        [ValType::I64],
        UserData::new(()),
        |plugin: &mut CurrentPlugin,
         _inputs: &[Val],
         outputs: &mut [Val],
         _user_data: UserData<()>| {
            let value = "1234567890"; // JSON: "1234567890" (12 bytes)
            plugin.memory_set_val(&mut outputs[0], Json(value))?;
            Ok(())
        },
    )
    .with_namespace("env");

    let report_fn = Function::new(
        "report_first_8_bytes",
        [ValType::I64],
        [],
        UserData::new(()),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         _outputs: &mut [Val],
         _user_data: UserData<()>| {
            let handle = inputs[0].unwrap_i64();
            let mem = plugin.memory_from_val(&Val::I64(handle)).unwrap();
            let bytes = plugin.memory_bytes(mem).unwrap();

            let first_8 = &bytes[0..8.min(bytes.len())];
            println!("First 8 bytes at handle: {:?}", first_8);
            println!("First 8 as string: {}", String::from_utf8_lossy(first_8));

            assert_eq!(bytes[0], 34); // '"'
            Ok(())
        },
    )
    .with_namespace("env");

    let manifest = Manifest::new([Wasm::data(wasm)]);
    let mut plugin =
        Plugin::new(&manifest, vec![get_data_fn, report_fn], true).expect("Failed to build plugin");

    plugin.call::<(), ()>("test", ()).expect("WASM call failed");
}
