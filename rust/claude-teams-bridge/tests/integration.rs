use claude_teams_bridge::{
    config_path, inbox_path, is_message_read, list_inboxes, list_teams, read_inbox,
    write_team_config, write_to_inbox, TeamConfig, TeamInfo, TeamMember, TeamRegistry, TeamsMessage,
};
use serial_test::serial;
use std::env;
use std::fs;
use std::path::Path;
use tempfile::tempdir;

/// Sets HOME for the duration of a test. Must be used with #[serial].
struct ScopedHome {
    old_home: Option<String>,
}

impl ScopedHome {
    fn new(new_home: &Path) -> Self {
        let old_home = env::var("HOME").ok();
        unsafe { env::set_var("HOME", new_home) };
        Self { old_home }
    }
}

impl Drop for ScopedHome {
    fn drop(&mut self) {
        if let Some(ref old) = self.old_home {
            unsafe { env::set_var("HOME", old) };
        } else {
            unsafe { env::remove_var("HOME") };
        }
    }
}

// --- Format contract tests (no HOME needed) ---

#[test]
fn json_format_matches_cc_contract() {
    let msg = TeamsMessage {
        from: "agent".into(),
        text: "hello".into(),
        summary: "greeting".into(),
        timestamp: "2024-03-21T12:00:00Z".into(),
        read: false,
    };

    let json = serde_json::to_string(&msg).unwrap();
    let obj: serde_json::Value = serde_json::from_str(&json).unwrap();

    let keys: Vec<String> = obj.as_object().unwrap().keys().cloned().collect();
    assert_eq!(keys.len(), 5);
    assert!(keys.contains(&"from".to_string()));
    assert!(keys.contains(&"text".to_string()));
    assert!(keys.contains(&"summary".to_string()));
    assert!(keys.contains(&"timestamp".to_string()));
    assert!(keys.contains(&"read".to_string()));

    assert_eq!(obj["from"], "agent");
    assert_eq!(obj["text"], "hello");
    assert_eq!(obj["read"], false);
}

#[test]
fn config_json_matches_cc_format() {
    let config = TeamConfig {
        name: "test-team".into(),
        description: "A test team".into(),
        created_at: 1711022400,
        lead_agent_id: "lead-1".into(),
        lead_session_id: "session-1".into(),
        members: vec![TeamMember {
            agent_id: "agent-1".into(),
            name: "worker".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022401,
            cwd: "/tmp".into(),
        }],
    };

    let json = serde_json::to_string(&config).unwrap();
    let obj: serde_json::Value = serde_json::from_str(&json).unwrap();

    assert!(obj.as_object().unwrap().contains_key("createdAt"));
    assert!(obj.as_object().unwrap().contains_key("leadAgentId"));
    assert!(obj.as_object().unwrap().contains_key("leadSessionId"));

    let member = &obj["members"][0];
    assert!(member.as_object().unwrap().contains_key("agentId"));
    assert!(member.as_object().unwrap().contains_key("agentType"));
    assert!(member.as_object().unwrap().contains_key("joinedAt"));

    assert!(!obj.as_object().unwrap().contains_key("created_at"));
}

#[test]
fn config_deserializes_real_cc_json() {
    let cc_json = r#"{
  "name": "exomonad",
  "description": "ExoMonad Dev Team",
  "createdAt": 1711022400,
  "leadAgentId": "agent-lead",
  "leadSessionId": "sess-123",
  "members": [
    {
      "agentId": "agent-1",
      "name": "worker-1",
      "agentType": "claude",
      "model": "claude-3-sonnet-20240229",
      "joinedAt": 1711022405,
      "cwd": "/home/user/project"
    }
  ]
}"#;

    let config: TeamConfig = serde_json::from_str(cc_json).unwrap();
    assert_eq!(config.name, "exomonad");
    assert_eq!(config.created_at, 1711022400);
    assert_eq!(config.lead_agent_id, "agent-lead");
    assert_eq!(config.members[0].agent_id, "agent-1");
    assert_eq!(config.members[0].joined_at, 1711022405);
}

// --- Filesystem tests (need HOME override, must be #[serial]) ---

#[test]
#[serial]
fn inbox_append_preserves_order() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "order-team";
    let recipient = "lead";

    for i in 1..=5 {
        write_to_inbox(team, recipient, "sender", &format!("msg {i}"), "sum").unwrap();
    }

    let messages = read_inbox(team, recipient).unwrap();
    assert_eq!(messages.len(), 5);
    for i in 1..=5 {
        assert_eq!(messages[i - 1].text, format!("msg {i}"));
    }
}

#[test]
#[serial]
fn inbox_returns_valid_timestamp() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let ts = write_to_inbox("ts-team", "r", "s", "m", "s").unwrap();
    let parsed = chrono::DateTime::parse_from_rfc3339(&ts);
    assert!(parsed.is_ok(), "Timestamp {ts} is not valid RFC3339");
}

#[test]
#[serial]
fn is_message_read_detects_cc_marking() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "mark-team";
    let recipient = "worker";

    let ts = write_to_inbox(team, recipient, "s", "m", "s").unwrap();
    assert!(!is_message_read(team, recipient, &ts));

    // Simulate CC's InboxPoller marking the message as read
    let path = inbox_path(team, recipient).unwrap();
    let content = fs::read_to_string(&path).unwrap();
    let mut messages: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();
    messages[0].read = true;
    fs::write(&path, serde_json::to_string_pretty(&messages).unwrap()).unwrap();

    assert!(is_message_read(team, recipient, &ts));
}

#[test]
#[serial]
fn multiple_recipients_isolated() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "iso-team";

    write_to_inbox(team, "alice", "boss", "msg to alice", "sum").unwrap();
    write_to_inbox(team, "bob", "boss", "msg to bob", "sum").unwrap();

    let alice_msgs = read_inbox(team, "alice").unwrap();
    let bob_msgs = read_inbox(team, "bob").unwrap();

    assert_eq!(alice_msgs.len(), 1);
    assert_eq!(alice_msgs[0].text, "msg to alice");
    assert_eq!(bob_msgs.len(), 1);
    assert_eq!(bob_msgs[0].text, "msg to bob");
}

#[test]
#[serial]
fn atomic_write_no_partial() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "atomic-team";
    let recipient = "r";

    write_to_inbox(team, recipient, "s", "m", "s").unwrap();

    let inbox_dir = tmp
        .path()
        .join(".claude")
        .join("teams")
        .join(team)
        .join("inboxes");
    for entry in fs::read_dir(inbox_dir).unwrap() {
        let entry = entry.unwrap();
        let name = entry.file_name().into_string().unwrap();
        assert!(!name.ends_with(".tmp"), "Found leftover tmp file: {name}");
    }
}

#[test]
#[serial]
fn empty_inbox_returns_empty_vec() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let messages = read_inbox("nonexistent", "nobody").unwrap();
    assert!(messages.is_empty());
}

#[test]
#[serial]
fn discovery_list_teams() {
    let tmp = tempdir().unwrap();
    let base = tmp.path().join(".claude").join("teams");
    fs::create_dir_all(&base).unwrap();
    fs::create_dir(base.join("team-a")).unwrap();
    fs::create_dir(base.join("team-b")).unwrap();

    let _home = ScopedHome::new(tmp.path());
    let teams = list_teams().unwrap();
    assert_eq!(teams, vec!["team-a", "team-b"]);
}

#[test]
#[serial]
fn discovery_list_inboxes() {
    let tmp = tempdir().unwrap();
    let inbox_dir = tmp
        .path()
        .join(".claude")
        .join("teams")
        .join("team-x")
        .join("inboxes");
    fs::create_dir_all(&inbox_dir).unwrap();
    fs::write(inbox_dir.join("alice.json"), "[]").unwrap();
    fs::write(inbox_dir.join("bob.json"), "[]").unwrap();

    let _home = ScopedHome::new(tmp.path());
    let inboxes = list_inboxes("team-x").unwrap();
    assert_eq!(inboxes, vec!["alice", "bob"]);
}

#[test]
#[serial]
fn paths_structure() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let c_path = config_path("my-team").unwrap();
    let i_path = inbox_path("my-team", "recipient").unwrap();

    assert!(c_path.ends_with(".claude/teams/my-team/config.json"));
    assert!(i_path.ends_with(".claude/teams/my-team/inboxes/recipient.json"));
}

// --- Two-tier resolve integration tests ---

/// E2E: exomonad agent sends to CC-native teammate via Tier 2 config.json resolve.
///
/// Simulates the real flow:
/// 1. CC creates a team (TeamCreate writes config.json with members)
/// 2. Exomonad agent registers itself in-memory (SessionStart hook)
/// 3. Exomonad agent calls send_message targeting a CC-native teammate
/// 4. resolve() finds the CC-native teammate via Tier 2 (config.json)
/// 5. write_to_inbox delivers the message to the correct inbox file
/// 6. The inbox file contains the message (CC's InboxPoller would read this)
#[tokio::test]
#[serial]
async fn resolve_tier2_e2e_inbox_delivery() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "e2e-resolve-team";

    // Step 1: Simulate CC's TeamCreate — write config.json with members
    let config = TeamConfig {
        name: team.into(),
        description: "E2E resolve test".into(),
        created_at: 1711022400,
        lead_agent_id: "lead-uuid".into(),
        lead_session_id: "session-uuid".into(),
        members: vec![
            TeamMember {
                agent_id: "exo-agent-uuid".into(),
                name: "exo-worker".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022401,
                cwd: "/tmp/exo".into(),
            },
            TeamMember {
                agent_id: "cc-supervisor-uuid".into(),
                name: "supervisor".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022402,
                cwd: "/tmp/cc".into(),
            },
        ],
    };
    write_team_config(team, &config).unwrap();

    // Step 2: Exomonad agent registers itself in-memory (only exo-worker, NOT supervisor)
    let registry = TeamRegistry::new();
    registry
        .register(
            "exo-worker",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "exo-worker".into(),
            },
        )
        .await;

    // Step 3: Tier 1 — exo-worker is in memory
    let exo_result = registry.resolve("exo-worker", None).await;
    assert!(exo_result.is_some());
    assert_eq!(exo_result.unwrap().team_name, team);

    // Step 4: Tier 2 — supervisor NOT in memory, resolved from config.json
    let sender_team = registry
        .get("exo-worker")
        .await
        .map(|info| info.team_name);
    let supervisor_result = registry
        .resolve("supervisor", sender_team.as_deref())
        .await;
    assert!(supervisor_result.is_some());
    let supervisor_info = supervisor_result.unwrap();
    assert_eq!(supervisor_info.team_name, team);
    assert_eq!(supervisor_info.inbox_name, "supervisor");

    // Step 5: Write to the resolved inbox (what deliver_to_agent does)
    let ts = write_to_inbox(
        &supervisor_info.team_name,
        &supervisor_info.inbox_name,
        "exo-worker",
        "[from: exo-worker] Task completed successfully",
        "Agent update: exo-worker",
    )
    .unwrap();

    // Step 6: Verify the inbox file exists and contains the message
    let messages = read_inbox(team, "supervisor").unwrap();
    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0].from, "exo-worker");
    assert_eq!(
        messages[0].text,
        "[from: exo-worker] Task completed successfully"
    );
    assert_eq!(messages[0].timestamp, ts);
    assert!(!messages[0].read);

    // Verify the message is unread (CC's InboxPoller hasn't touched it)
    assert!(!is_message_read(team, "supervisor", &ts));
}

/// Verify resolve_from_config correctly deserializes CC's actual JSON format.
///
/// CC writes camelCase JSON (createdAt, leadAgentId, agentType, joinedAt).
/// If our serde mapping drifts, this test catches it.
#[test]
#[serial]
fn resolve_from_config_real_cc_format() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "cc-format-test";

    // Write raw JSON matching CC's exact output format (not our Rust struct)
    let cc_json = r#"{
  "name": "cc-format-test",
  "description": "Real CC format",
  "createdAt": 1711022400,
  "leadAgentId": "01JQRS-lead",
  "leadSessionId": "sess-abc123",
  "members": [
    {
      "agentId": "01JQRS-worker",
      "name": "my-researcher",
      "agentType": "general-purpose",
      "model": "claude-opus-4-6",
      "joinedAt": 1711022405,
      "cwd": "/home/user/project"
    },
    {
      "agentId": "01JQRS-supervisor",
      "name": "supervisor",
      "agentType": "general-purpose",
      "model": "claude-opus-4-6",
      "joinedAt": 1711022410,
      "cwd": "/home/user/project"
    }
  ]
}"#;

    let config_dir = tmp
        .path()
        .join(".claude")
        .join("teams")
        .join(team);
    fs::create_dir_all(&config_dir).unwrap();
    fs::write(config_dir.join("config.json"), cc_json).unwrap();

    // resolve_from_config should parse CC's camelCase JSON correctly
    let result = TeamRegistry::resolve_from_config(team, "supervisor");
    assert!(result.is_some(), "Failed to resolve 'supervisor' from CC-format config.json");
    let info = result.unwrap();
    assert_eq!(info.team_name, team);
    assert_eq!(info.inbox_name, "supervisor");

    // Non-existent member returns None
    assert!(TeamRegistry::resolve_from_config(team, "nonexistent").is_none());
}

/// Live E2E: the test runner creates a CC team, spawns a real CC-native teammate,
/// resolves the teammate via Tier 2 config.json, and writes to their inbox.
///
/// The teammate ("test-receiver") is a real Claude Code agent — it joined the team
/// via TaskCreate, is listed in config.json, but is NOT in the in-memory TeamRegistry.
/// This is the exact scenario two-tier resolve was built for.
///
/// Run: `cargo test -p claude-teams-bridge --test integration -- live_team_resolve --ignored --nocapture`
///
/// Prerequisites: the calling Claude session must have already:
/// 1. Called TeamCreate for "resolve-e2e"
/// 2. Spawned "test-receiver" as a CC-native teammate in that team
#[tokio::test]
#[ignore]
async fn live_team_resolve_and_deliver() {
    let team = "resolve-e2e";
    let recipient = "test-receiver";

    // Verify the team exists on disk with the expected member
    let config_path = claude_teams_bridge::config_path(team);
    assert!(
        config_path
            .as_ref()
            .map(|p| p.exists())
            .unwrap_or(false),
        "Team '{}' not found — run TeamCreate first",
        team
    );

    let config = claude_teams_bridge::read_team_config(team).unwrap();
    let member_names: Vec<&str> = config.members.iter().map(|m| m.name.as_str()).collect();
    println!("[live] Team '{}' members: {:?}", team, member_names);
    assert!(
        member_names.contains(&recipient),
        "'{}' not in team — spawn the teammate first",
        recipient
    );

    // Tier 2: resolve test-receiver from config.json (NOT in memory)
    let info = TeamRegistry::resolve_from_config(team, recipient)
        .expect("Tier 2 should resolve test-receiver from config.json");
    println!(
        "[live] Tier 2 resolved '{}': team={}, inbox={}",
        recipient, info.team_name, info.inbox_name
    );

    // Full registry flow: register sender in memory, resolve recipient via Tier 2
    let registry = TeamRegistry::new();
    registry
        .register(
            "test-harness",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "test-harness".into(),
            },
        )
        .await;
    let sender_team = registry.get("test-harness").await.map(|i| i.team_name);
    let resolved = registry
        .resolve(recipient, sender_team.as_deref())
        .await
        .expect("resolve() should find test-receiver via Tier 2");
    assert_eq!(resolved.team_name, team);
    assert_eq!(resolved.inbox_name, recipient);

    // Write to the teammate's inbox — this is what deliver_to_agent does
    let ts = write_to_inbox(
        &resolved.team_name,
        &resolved.inbox_name,
        "test-harness",
        "TWO_TIER_RESOLVE_TEST: message from test harness via Tier 2 config.json resolve",
        "E2E resolve test",
    )
    .unwrap();
    println!("[live] Wrote to {}'s inbox at {}", recipient, ts);

    // Read back and verify
    let messages = read_inbox(team, recipient).unwrap();
    let last = messages.last().expect("inbox should have the test message");
    assert_eq!(last.from, "test-harness");
    assert!(last.text.contains("TWO_TIER_RESOLVE_TEST"));
    assert_eq!(last.timestamp, ts);
    println!("[live] Verified message in inbox: from={}, text={}", last.from, last.text);
    println!(
        "[live] Message is live at ~/.claude/teams/{}/inboxes/{}.json",
        team, recipient
    );
    println!("[live] CC's InboxPoller will deliver it to the test-receiver agent");

    // DON'T clean up — let the InboxPoller deliver it to the real agent.
    // The test-receiver should see it and respond.
}
