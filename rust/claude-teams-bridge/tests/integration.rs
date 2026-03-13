use claude_teams_bridge::{
    config_path, inbox_path, is_message_read, list_inboxes, list_teams, read_inbox,
    write_to_inbox, TeamConfig, TeamMember, TeamsMessage,
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
