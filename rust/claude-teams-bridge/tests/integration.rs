use claude_teams_bridge::{
    config_path, inbox_path, is_message_read, list_inboxes, list_teams, read_inbox,
    read_team_config, write_team_config, write_to_inbox, TeamConfig, TeamInfo, TeamMember,
    TeamRegistry, TeamsMessage,
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
            backend_type: None,
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
                backend_type: None,
            },
            TeamMember {
                agent_id: "cc-supervisor-uuid".into(),
                name: "supervisor".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022402,
                cwd: "/tmp/cc".into(),
                backend_type: None,
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
    let sender_team = registry.get("exo-worker").await.map(|info| info.team_name);
    let supervisor_result = registry.resolve("supervisor", sender_team.as_deref()).await;
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

    let config_dir = tmp.path().join(".claude").join("teams").join(team);
    fs::create_dir_all(&config_dir).unwrap();
    fs::write(config_dir.join("config.json"), cc_json).unwrap();

    // resolve_from_config should parse CC's camelCase JSON correctly
    let result = TeamRegistry::resolve_from_config(team, "supervisor");
    assert!(
        result.is_some(),
        "Failed to resolve 'supervisor' from CC-format config.json"
    );
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
        config_path.as_ref().map(|p| p.exists()).unwrap_or(false),
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
    println!(
        "[live] Verified message in inbox: from={}, text={}",
        last.from, last.text
    );
    println!(
        "[live] Message is live at ~/.claude/teams/{}/inboxes/{}.json",
        team, recipient
    );
    println!("[live] CC's InboxPoller will deliver it to the test-receiver agent");

    // DON'T clean up — let the InboxPoller deliver it to the real agent.
    // The test-receiver should see it and respond.
}

// --- Edge case / failure scenario tests ---

/// 1. Lead leaves team: deregister from in-memory, config.json still names old lead.
/// Verifies config outlives in-memory state for resolve_lead.
#[tokio::test]
#[serial]
async fn lead_deregistered_still_resolves_via_config() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "lead-leaves-team";
    let config = TeamConfig {
        name: team.into(),
        description: "Test lead departure".into(),
        created_at: 1711022400,
        lead_agent_id: "tl-uuid".into(),
        lead_session_id: "session-1".into(),
        members: vec![
            TeamMember {
                agent_id: "tl-uuid".into(),
                name: "team-lead".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022400,
                cwd: "/tmp".into(),
                backend_type: None,
            },
            TeamMember {
                agent_id: "w1-uuid".into(),
                name: "worker-1".into(),
                agent_type: "claude".into(),
                model: "haiku".into(),
                joined_at: 1711022401,
                cwd: "/tmp".into(),
                backend_type: None,
            },
            TeamMember {
                agent_id: "w2-uuid".into(),
                name: "worker-2".into(),
                agent_type: "claude".into(),
                model: "haiku".into(),
                joined_at: 1711022402,
                cwd: "/tmp".into(),
                backend_type: None,
            },
        ],
    };
    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();
    registry
        .register(
            "team-lead",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "team-lead".into(),
            },
        )
        .await;
    registry
        .register(
            "worker-1",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "worker-1".into(),
            },
        )
        .await;

    // TL shuts down — deregister from in-memory
    registry.deregister("team-lead").await;
    assert!(registry.get("team-lead").await.is_none());

    // resolve_lead still finds the lead via config.json
    let lead = registry.resolve_lead(team).await;
    assert_eq!(lead, Some("team-lead".to_string()));
}

/// 2. Lead replaced mid-session: config.json is authoritative over in-memory newcomers.
#[tokio::test]
#[serial]
async fn lead_replaced_config_authoritative() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "lead-replace-team";
    let config = TeamConfig {
        name: team.into(),
        description: "Test lead replacement".into(),
        created_at: 1711022400,
        lead_agent_id: "uuid-old-lead".into(),
        lead_session_id: "session-old".into(),
        members: vec![
            TeamMember {
                agent_id: "uuid-old-lead".into(),
                name: "old-lead".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022400,
                cwd: "/tmp".into(),
                backend_type: None,
            },
            TeamMember {
                agent_id: "uuid-new-lead".into(),
                name: "new-lead".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022401,
                cwd: "/tmp".into(),
                backend_type: None,
            },
        ],
    };
    write_team_config(team, &config).unwrap();

    // Register only new-lead in memory (old-lead is gone)
    let registry = TeamRegistry::new();
    registry
        .register(
            "new-lead",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "new-lead".into(),
            },
        )
        .await;

    // Config says old-lead is the lead — config wins
    let lead = registry.resolve_lead(team).await;
    assert_eq!(lead, Some("old-lead".to_string()));
}

/// 3. Empty team: config exists with no members.
#[tokio::test]
#[serial]
async fn empty_team_no_panic() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "empty-team";
    let config = TeamConfig {
        name: team.into(),
        description: "Ghost town".into(),
        created_at: 1711022400,
        lead_agent_id: "nobody".into(),
        lead_session_id: "session".into(),
        members: vec![],
    };
    write_team_config(team, &config).unwrap();

    // resolve_from_config returns None for any name
    assert!(TeamRegistry::resolve_from_config(team, "anyone").is_none());

    // resolve_lead returns None (no members in config, no in-memory entries)
    let registry = TeamRegistry::new();
    let lead = registry.resolve_lead(team).await;
    assert!(lead.is_none());
}

/// 4. Cross-team name collision: same name in two teams resolved by hint.
#[tokio::test]
#[serial]
async fn cross_team_name_collision() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let make_config = |team: &str, worker_uuid: &str| TeamConfig {
        name: team.into(),
        description: "collision test".into(),
        created_at: 1711022400,
        lead_agent_id: "lead".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: worker_uuid.into(),
            name: "worker".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022401,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };

    write_team_config("alpha", &make_config("alpha", "uuid-alpha")).unwrap();
    write_team_config("beta", &make_config("beta", "uuid-beta")).unwrap();

    let registry = TeamRegistry::new();
    // Register sender in alpha
    registry
        .register(
            "sender",
            TeamInfo {
                team_name: "alpha".into(),
                inbox_name: "sender".into(),
            },
        )
        .await;

    // Tier 2 resolve with hint scopes to correct team
    let alpha_worker = registry.resolve("worker", Some("alpha")).await.unwrap();
    assert_eq!(alpha_worker.team_name, "alpha");

    let beta_worker = registry.resolve("worker", Some("beta")).await.unwrap();
    assert_eq!(beta_worker.team_name, "beta");

    // Register "worker" in-memory for alpha — Tier 1 wins regardless of hint
    registry
        .register(
            "worker",
            TeamInfo {
                team_name: "alpha".into(),
                inbox_name: "worker".into(),
            },
        )
        .await;
    let result = registry.resolve("worker", Some("beta")).await.unwrap();
    assert_eq!(
        result.team_name, "alpha",
        "Tier 1 (in-memory) must win over Tier 2 hint"
    );
}

/// 5. Orphaned agent: not in any team, graceful None.
#[tokio::test]
#[serial]
async fn orphaned_agent_returns_none() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "some-team";
    let config = TeamConfig {
        name: team.into(),
        description: "Has members".into(),
        created_at: 1711022400,
        lead_agent_id: "lead-uuid".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: "lead-uuid".into(),
            name: "lead".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022401,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };

    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();
    registry
        .register(
            "lead",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "lead".into(),
            },
        )
        .await;

    // "ghost" is nowhere
    assert!(registry.resolve("ghost", None).await.is_none());
    assert!(registry.resolve("ghost", Some(team)).await.is_none());
}

/// 6. resolve_lead name-match fallback (exomonad-style leadAgentId = member name).
#[tokio::test]
#[serial]
async fn resolve_lead_name_match_fallback() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "name-match-team";

    // Case A: leadAgentId is a member name (not UUID) — exomonad convention
    let config = TeamConfig {
        name: team.into(),
        description: "Name match test".into(),
        created_at: 1711022400,
        lead_agent_id: "root".into(), // name, not UUID
        lead_session_id: "session".into(),
        members: vec![
            TeamMember {
                agent_id: "uuid-root".into(),
                name: "root".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022400,
                cwd: "/tmp".into(),
                backend_type: None,
            },
            TeamMember {
                agent_id: "uuid-worker".into(),
                name: "helper".into(),
                agent_type: "claude".into(),
                model: "haiku".into(),
                joined_at: 1711022401,
                cwd: "/tmp".into(),
                backend_type: None,
            },
        ],
    };
    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();
    let lead = registry.resolve_lead(team).await;
    // "root" doesn't match any agent_id, but matches member name → name fallback
    assert_eq!(lead, Some("root".to_string()));

    // Case B: leadAgentId matches neither UUID nor name → falls back to first in-memory entry
    let team2 = "no-match-team";
    let config2 = TeamConfig {
        name: team2.into(),
        description: "No match test".into(),
        created_at: 1711022400,
        lead_agent_id: "nonexistent-lead".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: "uuid-someone".into(),
            name: "someone".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022401,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };

    write_team_config(team2, &config2).unwrap();

    // No in-memory entries → None (config doesn't match by UUID or name)
    let lead2 = registry.resolve_lead(team2).await;
    assert!(lead2.is_none());

    // Add in-memory entry → fallback picks it
    registry
        .register(
            "fallback-agent",
            TeamInfo {
                team_name: team2.into(),
                inbox_name: "fallback-agent".into(),
            },
        )
        .await;
    let lead3 = registry.resolve_lead(team2).await;
    assert_eq!(lead3, Some("fallback-agent".to_string()));
}

/// 7. Inbox delivery survives team restructuring (config overwrite).
#[tokio::test]
#[serial]
async fn inbox_survives_config_restructuring() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "restructure-team";
    let config_v1 = TeamConfig {
        name: team.into(),
        description: "v1".into(),
        created_at: 1711022400,
        lead_agent_id: "lead-uuid".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: "w-uuid".into(),
            name: "worker".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022401,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };
    write_team_config(team, &config_v1).unwrap();

    // Deliver message via Tier 2 resolve
    let info = TeamRegistry::resolve_from_config(team, "worker").unwrap();
    write_to_inbox(
        &info.team_name,
        &info.inbox_name,
        "sender",
        "hello worker",
        "sum",
    )
    .unwrap();

    // Overwrite config: rename "worker" to "senior-worker"
    let config_v2 = TeamConfig {
        name: team.into(),
        description: "v2".into(),
        created_at: 1711022400,
        lead_agent_id: "lead-uuid".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: "w-uuid".into(),
            name: "senior-worker".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022401,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };
    write_team_config(team, &config_v2).unwrap();

    // Old name misses in new config
    assert!(TeamRegistry::resolve_from_config(team, "worker").is_none());
    // New name resolves
    assert!(TeamRegistry::resolve_from_config(team, "senior-worker").is_some());

    // Old inbox file still readable (filesystem isn't config-coupled)
    let old_messages = read_inbox(team, "worker").unwrap();
    assert_eq!(old_messages.len(), 1);
    assert_eq!(old_messages[0].text, "hello worker");
}

/// 8. Concurrent register + resolve: no panics, no data loss under contention.
#[tokio::test]
async fn concurrent_register_resolve_no_panic() {
    let registry = std::sync::Arc::new(TeamRegistry::new());
    let mut handles = vec![];

    for i in 0..20 {
        let reg = registry.clone();
        handles.push(tokio::spawn(async move {
            let name = format!("agent-{i}");
            reg.register(
                &name,
                TeamInfo {
                    team_name: "concurrent-team".into(),
                    inbox_name: name.clone(),
                },
            )
            .await;
            // Immediately try to resolve self and others
            let self_result = reg.resolve(&name, None).await;
            assert!(
                self_result.is_some(),
                "agent should resolve itself after register"
            );
            // Resolve a potentially-not-yet-registered peer (should return Some or None, not panic)
            let _ = reg.resolve(&format!("agent-{}", (i + 1) % 20), None).await;
        }));
    }

    for h in handles {
        h.await.unwrap();
    }

    // After all tasks complete, every agent should be resolvable
    for i in 0..20 {
        let name = format!("agent-{i}");
        assert!(
            registry.get(&name).await.is_some(),
            "agent-{i} should be registered after all tasks complete"
        );
    }
}

/// 9. Supervisor → lead routing chain: the full path notify_parent traverses.
/// Uses TeamRegistry only (SupervisorRegistry is in exomonad-core).
#[tokio::test]
#[serial]
async fn supervisor_to_lead_routing_chain() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "routing-chain-team";
    let config = TeamConfig {
        name: team.into(),
        description: "Routing chain test".into(),
        created_at: 1711022400,
        lead_agent_id: "tl-uuid".into(),
        lead_session_id: "session".into(),
        members: vec![
            TeamMember {
                agent_id: "tl-uuid".into(),
                name: "team-lead".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 1711022400,
                cwd: "/tmp".into(),
                backend_type: None,
            },
            TeamMember {
                agent_id: "child-uuid".into(),
                name: "child-worker".into(),
                agent_type: "claude".into(),
                model: "haiku".into(),
                joined_at: 1711022401,
                cwd: "/tmp".into(),
                backend_type: None,
            },
        ],
    };
    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();
    // TL registered in memory (via SessionStart hook)
    registry
        .register(
            "team-lead",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "team-lead".into(),
            },
        )
        .await;

    // Simulate the chain: child knows supervisor name → resolve supervisor → get team → resolve_lead
    let supervisor_name = "team-lead"; // from SupervisorRegistry.lookup()
    let supervisor_info = registry.resolve(supervisor_name, None).await.unwrap();
    assert_eq!(supervisor_info.team_name, team);

    let lead = registry
        .resolve_lead(&supervisor_info.team_name)
        .await
        .unwrap();
    assert_eq!(lead, "team-lead");

    // Deliver the notification to the lead's inbox
    let ts = write_to_inbox(
        &supervisor_info.team_name,
        &supervisor_info.inbox_name,
        "child-worker",
        "[from: child-worker] Task completed",
        "child done",
    )
    .unwrap();

    let messages = read_inbox(team, "team-lead").unwrap();
    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0].from, "child-worker");
    assert_eq!(messages[0].timestamp, ts);
}

/// 10. Inbox accumulation: multiple senders, ordering, read tracking.
#[tokio::test]
#[serial]
async fn inbox_accumulation_multi_sender() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "multi-sender-team";
    let recipient = "lead";

    // 3 agents each write a message (sleep to ensure distinct timestamps)
    let ts1 = write_to_inbox(team, recipient, "worker-1", "report from w1", "w1 done").unwrap();
    tokio::time::sleep(std::time::Duration::from_millis(2)).await;
    let ts2 = write_to_inbox(team, recipient, "worker-2", "report from w2", "w2 done").unwrap();
    tokio::time::sleep(std::time::Duration::from_millis(2)).await;
    let ts3 = write_to_inbox(team, recipient, "worker-3", "report from w3", "w3 done").unwrap();
    // Verify timestamps are distinct (millisecond precision can collide without sleep)
    assert_ne!(ts1, ts2, "timestamps must be distinct for read tracking");
    assert_ne!(ts2, ts3, "timestamps must be distinct for read tracking");

    // All 3 present, ordered by timestamp (insertion order)
    let messages = read_inbox(team, recipient).unwrap();
    assert_eq!(messages.len(), 3);
    assert_eq!(messages[0].from, "worker-1");
    assert_eq!(messages[1].from, "worker-2");
    assert_eq!(messages[2].from, "worker-3");
    assert_eq!(messages[0].text, "report from w1");
    assert_eq!(messages[1].text, "report from w2");
    assert_eq!(messages[2].text, "report from w3");

    // All unread initially
    assert!(!is_message_read(team, recipient, &ts1));
    assert!(!is_message_read(team, recipient, &ts2));
    assert!(!is_message_read(team, recipient, &ts3));

    // Simulate CC's InboxPoller marking first two as read
    let path = inbox_path(team, recipient).unwrap();
    let content = fs::read_to_string(&path).unwrap();
    let mut msgs: Vec<TeamsMessage> = serde_json::from_str(&content).unwrap();
    msgs[0].read = true;
    msgs[1].read = true;
    fs::write(&path, serde_json::to_string_pretty(&msgs).unwrap()).unwrap();

    // Per-timestamp read status
    assert!(is_message_read(team, recipient, &ts1));
    assert!(is_message_read(team, recipient, &ts2));
    assert!(!is_message_read(team, recipient, &ts3));
}

// --- Live E2E test (requires real CC team) ---

/// Live E2E: exercises 6 messaging scenarios against a real CC team.
///
/// The calling Claude session must have already called TeamCreate for "teams-e2e".
/// This test reads the live config.json, constructs registries, sends messages through
/// various routing paths, and verifies inbox delivery.
///
/// Run: `cargo test -p claude-teams-bridge --test integration -- live_teams_e2e --ignored --nocapture`
#[tokio::test]
#[ignore]
async fn live_teams_e2e() {
    let team = "teams-e2e";

    // --- Preflight: verify team exists ---
    let config_file = claude_teams_bridge::config_path(team);
    assert!(
        config_file.as_ref().map(|p| p.exists()).unwrap_or(false),
        "Team '{}' not found — call TeamCreate first",
        team
    );
    let config = claude_teams_bridge::read_team_config(team).unwrap();
    let member_names: Vec<&str> = config.members.iter().map(|m| m.name.as_str()).collect();
    println!(
        "[e2e] Team '{}' has {} members: {:?}",
        team,
        config.members.len(),
        member_names
    );
    assert!(
        !config.members.is_empty(),
        "Team has no members — spawn at least one teammate"
    );

    let lead_name = {
        // Find lead by matching leadAgentId to member agentId, then fall back to name match
        config
            .members
            .iter()
            .find(|m| m.agent_id == config.lead_agent_id)
            .or_else(|| {
                config
                    .members
                    .iter()
                    .find(|m| m.name == config.lead_agent_id)
            })
            .map(|m| m.name.clone())
            .unwrap_or_else(|| config.members[0].name.clone())
    };
    println!("[e2e] Lead resolved to: '{}'", lead_name);

    // ====== SCENARIO 1: Direct inbox write to lead ======
    println!("\n[e2e] === Scenario 1: Direct inbox write to lead ===");
    let ts1 = write_to_inbox(
        team,
        &lead_name,
        "e2e-test-runner",
        "[E2E Scenario 1] Direct inbox write — basic delivery pipeline test",
        "E2E scenario 1",
    )
    .unwrap();
    let messages = read_inbox(team, &lead_name).unwrap();
    let s1_msg = messages
        .iter()
        .find(|m| m.timestamp == ts1)
        .expect("Scenario 1 message not found in inbox");
    assert_eq!(s1_msg.from, "e2e-test-runner");
    assert!(s1_msg.text.contains("Scenario 1"));
    assert!(!s1_msg.read);
    println!("[e2e] PASS: Message written and read back from lead inbox");

    // ====== SCENARIO 2: Tier 2 resolve for every config member ======
    println!("\n[e2e] === Scenario 2: Tier 2 resolve for every config member ===");
    for member in &config.members {
        let resolved = TeamRegistry::resolve_from_config(team, &member.name);
        assert!(
            resolved.is_some(),
            "Tier 2 failed to resolve member '{}' from config.json",
            member.name
        );
        let info = resolved.unwrap();
        assert_eq!(info.team_name, team);
        assert_eq!(info.inbox_name, member.name);
        println!(
            "[e2e]   Resolved '{}': team={}, inbox={}",
            member.name, info.team_name, info.inbox_name
        );
    }
    println!(
        "[e2e] PASS: All {} members resolved via Tier 2",
        config.members.len()
    );

    // ====== SCENARIO 3: Full registry flow (register sender, resolve recipient) ======
    println!("\n[e2e] === Scenario 3: Full registry flow ===");
    let registry = TeamRegistry::new();
    registry
        .register(
            "e2e-test-runner",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "e2e-test-runner".into(),
            },
        )
        .await;
    let sender_team = registry.get("e2e-test-runner").await.map(|i| i.team_name);
    assert_eq!(sender_team.as_deref(), Some(team));

    let resolved = registry
        .resolve(&lead_name, sender_team.as_deref())
        .await
        .expect("resolve() should find lead via Tier 2");
    assert_eq!(resolved.team_name, team);
    assert_eq!(resolved.inbox_name, lead_name);

    let ts3 = write_to_inbox(
        &resolved.team_name,
        &resolved.inbox_name,
        "e2e-test-runner",
        "[E2E Scenario 3] Full registry flow — register + resolve + deliver",
        "E2E scenario 3",
    )
    .unwrap();
    let messages = read_inbox(team, &lead_name).unwrap();
    assert!(
        messages.iter().any(|m| m.timestamp == ts3),
        "Scenario 3 message not in inbox"
    );
    println!("[e2e] PASS: Full registry flow works end-to-end");

    // ====== SCENARIO 4: Multi-sender accumulation ======
    println!("\n[e2e] === Scenario 4: Multi-sender accumulation (3 senders) ===");
    let senders = ["e2e-worker-alpha", "e2e-worker-beta", "e2e-worker-gamma"];
    let mut sender_timestamps = Vec::new();
    for sender in &senders {
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        let ts = write_to_inbox(
            team,
            &lead_name,
            sender,
            &format!("[E2E Scenario 4] Report from {}", sender),
            &format!("E2E from {}", sender),
        )
        .unwrap();
        sender_timestamps.push((sender.to_string(), ts));
    }

    let messages = read_inbox(team, &lead_name).unwrap();
    for (sender, ts) in &sender_timestamps {
        let found = messages.iter().find(|m| m.timestamp == *ts);
        assert!(found.is_some(), "Message from {} not found", sender);
        assert_eq!(found.unwrap().from, *sender);
        println!("[e2e]   Found message from '{}' at {}", sender, ts);
    }
    // Verify ordering: alpha before beta before gamma
    let alpha_idx = messages
        .iter()
        .position(|m| m.timestamp == sender_timestamps[0].1)
        .unwrap();
    let beta_idx = messages
        .iter()
        .position(|m| m.timestamp == sender_timestamps[1].1)
        .unwrap();
    let gamma_idx = messages
        .iter()
        .position(|m| m.timestamp == sender_timestamps[2].1)
        .unwrap();
    assert!(alpha_idx < beta_idx, "alpha should come before beta");
    assert!(beta_idx < gamma_idx, "beta should come before gamma");
    println!("[e2e] PASS: 3 senders accumulated in order, no message loss");

    // ====== SCENARIO 5: Read tracking on unread messages ======
    println!("\n[e2e] === Scenario 5: Read tracking ===");
    // All scenario 4 messages should be unread
    for (sender, ts) in &sender_timestamps {
        assert!(
            !is_message_read(team, &lead_name, ts),
            "Message from {} should be unread before InboxPoller",
            sender
        );
    }
    // Scenario 1 message should also be unread (we just wrote it)
    assert!(
        !is_message_read(team, &lead_name, &ts1),
        "Scenario 1 message should be unread"
    );
    println!("[e2e] PASS: All test messages correctly marked as unread");

    // ====== SCENARIO 6: Cross-member delivery (to non-lead peer) ======
    println!("\n[e2e] === Scenario 6: Cross-member delivery ===");
    // Find a non-lead member to deliver to
    let peer = config.members.iter().find(|m| m.name != lead_name);
    if let Some(peer_member) = peer {
        let peer_info = TeamRegistry::resolve_from_config(team, &peer_member.name)
            .expect("Should resolve non-lead peer from config");
        let ts6 = write_to_inbox(
            &peer_info.team_name,
            &peer_info.inbox_name,
            "e2e-test-runner",
            &format!(
                "[E2E Scenario 6] Cross-member delivery to '{}'",
                peer_member.name
            ),
            "E2E scenario 6",
        )
        .unwrap();
        let peer_messages = read_inbox(team, &peer_member.name).unwrap();
        assert!(
            peer_messages.iter().any(|m| m.timestamp == ts6),
            "Scenario 6 message not found in {}'s inbox",
            peer_member.name
        );
        println!(
            "[e2e] PASS: Message delivered to non-lead peer '{}'",
            peer_member.name
        );
    } else {
        println!("[e2e] SKIP: Only one member in team, cannot test cross-member delivery");
    }

    // ====== SUMMARY ======
    println!("\n[e2e] ====== ALL SCENARIOS PASSED ======");
    println!(
        "[e2e] Messages are live at ~/.claude/teams/{}/inboxes/",
        team
    );
    println!("[e2e] CC's InboxPoller will deliver them to the agents");
    println!("[e2e] Lead inbox: check for messages from 'e2e-test-runner', 'e2e-worker-alpha', 'e2e-worker-beta', 'e2e-worker-gamma'");
}

// --- Sync persistence tests ---

#[tokio::test]
#[serial]
async fn test_register_syncs_to_disk() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "drift-team";
    let config = TeamConfig {
        name: team.into(),
        description: "Drift test".into(),
        created_at: 1711022400,
        lead_agent_id: "lead".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: "lead".into(),
            name: "lead".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 1711022400,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };
    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();

    // Register a new member in-memory
    registry
        .register(
            "worker-1",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "worker-1".into(),
            },
        )
        .await;

    // Verify it's ON disk (fix: register now syncs to disk)
    let disk_config = read_team_config(team).unwrap();
    let on_disk = disk_config.members.iter().any(|m| m.name == "worker-1");
    assert!(on_disk, "Fix: member in-memory should also be on disk");

    let member = disk_config
        .members
        .iter()
        .find(|m| m.name == "worker-1")
        .unwrap();
    assert_eq!(member.backend_type.as_deref(), Some("exomonad"));
}

#[tokio::test]
#[serial]
async fn test_remove_member_persists_to_disk() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "remove-sync-team";
    let config = TeamConfig {
        name: team.into(),
        description: "test".into(),
        created_at: 0,
        lead_agent_id: "lead".into(),
        lead_session_id: "session".into(),
        members: vec![TeamMember {
            agent_id: "lead".into(),
            name: "lead".into(),
            agent_type: "claude".into(),
            model: "opus".into(),
            joined_at: 0,
            cwd: "/tmp".into(),
            backend_type: None,
        }],
    };
    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();
    registry
        .register(
            "worker-1",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "worker-1".into(),
            },
        )
        .await;

    // Verify it's on disk
    assert!(read_team_config(team)
        .unwrap()
        .members
        .iter()
        .any(|m| m.name == "worker-1"));

    // Remove it
    registry.deregister("worker-1").await;

    // Verify it's GONE from disk
    let disk_config = read_team_config(team).unwrap();
    assert!(!disk_config.members.iter().any(|m| m.name == "worker-1"));
    // Lead should still be there
    assert!(disk_config.members.iter().any(|m| m.name == "lead"));
}

#[tokio::test]
#[serial]
async fn test_cc_native_members_preserved() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team = "preserve-team";
    let config = TeamConfig {
        name: team.into(),
        description: "test".into(),
        created_at: 0,
        lead_agent_id: "lead".into(),
        lead_session_id: "session".into(),
        members: vec![
            TeamMember {
                agent_id: "lead".into(),
                name: "lead".into(),
                agent_type: "haiku".into(),
                model: "opus".into(),
                joined_at: 0,
                cwd: "/tmp".into(),
                backend_type: None, // CC-native
            },
            TeamMember {
                agent_id: "native-1".into(),
                name: "native-1".into(),
                agent_type: "haiku".into(),
                model: "opus".into(),
                joined_at: 0,
                cwd: "/tmp".into(),
                backend_type: None, // CC-native
            },
        ],
    };
    write_team_config(team, &config).unwrap();

    let registry = TeamRegistry::new();
    registry
        .register(
            "worker-1",
            TeamInfo {
                team_name: team.into(),
                inbox_name: "worker-1".into(),
            },
        )
        .await;

    let disk_config = read_team_config(team).unwrap();
    assert_eq!(disk_config.members.len(), 3);
    assert!(disk_config.members.iter().any(|m| m.name == "lead"));
    assert!(disk_config.members.iter().any(|m| m.name == "native-1"));
    assert!(disk_config.members.iter().any(|m| m.name == "worker-1"));
}

#[tokio::test]
#[serial]
async fn test_register_member_moves_between_teams() {
    let tmp = tempdir().unwrap();
    let _home = ScopedHome::new(tmp.path());

    let team1 = "team-1";
    let team2 = "team-2";

    for team in [team1, team2] {
        let config = TeamConfig {
            name: team.into(),
            description: "test".into(),
            created_at: 0,
            lead_agent_id: "lead".into(),
            lead_session_id: "session".into(),
            members: vec![TeamMember {
                agent_id: "lead".into(),
                name: "lead".into(),
                agent_type: "claude".into(),
                model: "opus".into(),
                joined_at: 0,
                cwd: "/tmp".into(),
                backend_type: None,
            }],
        };
        write_team_config(team, &config).unwrap();
    }

    let registry = TeamRegistry::new();

    // Register in team 1
    registry
        .register_member(
            "agent-x",
            TeamInfo {
                team_name: team1.into(),
                inbox_name: "agent-x".into(),
            },
        )
        .await
        .unwrap();

    assert!(read_team_config(team1)
        .unwrap()
        .members
        .iter()
        .any(|m| m.name == "agent-x"));

    // Move to team 2
    registry
        .register_member(
            "agent-x",
            TeamInfo {
                team_name: team2.into(),
                inbox_name: "agent-x".into(),
            },
        )
        .await
        .unwrap();

    // Should be in team 2
    assert!(read_team_config(team2)
        .unwrap()
        .members
        .iter()
        .any(|m| m.name == "agent-x"));
    // Should be GONE from team 1
    assert!(!read_team_config(team1)
        .unwrap()
        .members
        .iter()
        .any(|m| m.name == "agent-x"));
}
