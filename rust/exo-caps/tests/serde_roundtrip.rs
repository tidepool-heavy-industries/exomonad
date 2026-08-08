use chrono::Utc;
use exo_caps::*;
use serde_json::json;

// Mock RoleKind for testing NodePapers and RoleRecord
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
enum TestRole {
    Root,
    Dev,
}

impl RoleKind for TestRole {
    fn all() -> &'static [Self] {
        &[TestRole::Root, TestRole::Dev]
    }
    fn agent_type(&self) -> AgentType {
        match self {
            TestRole::Root | TestRole::Dev => AgentType::Claude,
        }
    }
    fn role_str(&self) -> &'static str {
        match self {
            TestRole::Root => "root",
            TestRole::Dev => "dev",
        }
    }
}

fn assert_roundtrip<T>(val: &T)
where
    T: serde::Serialize + for<'de> serde::Deserialize<'de> + std::fmt::Debug + PartialEq,
{
    let json = serde_json::to_string(val).expect("failed to serialize");
    let back: T = serde_json::from_str(&json).expect("failed to deserialize");
    assert_eq!(val, &back, "Roundtrip failed for JSON: {}", json);
}

#[test]
fn test_node_path_roundtrip() {
    let np = NodePath::new(vec![
        AgentName::new("root".into()).unwrap(),
        AgentName::new("dev".into()).unwrap(),
    ])
    .unwrap();
    assert_roundtrip(&np);
    // Wire form check
    let json = serde_json::to_string(&np).unwrap();
    assert_eq!(json, r#"["root","dev"]"#);
}

#[test]
fn test_branch_roundtrip() {
    let b = Branch::new("root.dev".into()).unwrap();
    assert_roundtrip(&b);
    let json = serde_json::to_string(&b).unwrap();
    assert_eq!(json, r#""root.dev""#);
}

#[test]
fn test_pane_id_roundtrip() {
    let p = PaneId::new("%317".into()).unwrap();
    assert_roundtrip(&p);
    let json = serde_json::to_string(&p).unwrap();
    assert_eq!(json, r#""%317""#);
}

#[test]
fn test_inbox_path_roundtrip() {
    let p = InboxPath::new("/tmp/pane-1.jsonl".into());
    assert_roundtrip(&p);
}

#[test]
fn test_agent_name_roundtrip() {
    let n = AgentName::new("my-agent".into()).unwrap();
    assert_roundtrip(&n);
}

#[test]
fn test_synthetic_name_roundtrip() {
    let n = SyntheticName::new("github".into()).unwrap();
    assert_roundtrip(&n);
}

#[test]
fn test_message_body_roundtrip() {
    let b = MessageBody::new("hello world".into()).unwrap();
    assert_roundtrip(&b);
}

#[test]
fn test_summary_roundtrip() {
    let s = Summary::new("short summary".into()).unwrap();
    assert_roundtrip(&s);
}

#[test]
fn test_agent_type_roundtrip() {
    for variant in [AgentType::Claude, AgentType::Shoal] {
        assert_roundtrip(&variant);
    }
}

#[test]
fn test_child_kind_roundtrip() {
    for variant in [ChildKind::Inline, ChildKind::Worktree] {
        assert_roundtrip(&variant);
    }
}

#[test]
fn test_persona_roundtrip() {
    let p1 = Persona::Agent(AgentName::new("dev".into()).unwrap());
    let p2 = Persona::Synthetic(SyntheticName::new("github".into()).unwrap());
    assert_roundtrip(&p1);
    assert_roundtrip(&p2);

    // Pin tag names
    assert_eq!(serde_json::to_value(&p1).unwrap(), json!({"agent": "dev"}));
    assert_eq!(
        serde_json::to_value(&p2).unwrap(),
        json!({"synthetic": "github"})
    );
}

#[test]
fn test_message_roundtrip() {
    let m = Message {
        text: MessageBody::new("body".into()).unwrap(),
        summary: Summary::new("summary".into()).unwrap(),
        kind: MessageKind::Chat,
        reply_to: None,
    };
    assert_roundtrip(&m);
    // `reply_to` is omitted from the wire when unset — an ordinary message is byte-identical
    // to what a pre-field node wrote.
    assert!(!serde_json::to_string(&m).unwrap().contains("reply_to"));
}

#[test]
fn test_ingestion_entry_roundtrip() {
    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        from: Persona::Agent(AgentName::new("root".into()).unwrap()),
        id: Some("6f1c9b0e-0000-4000-8000-000000000001".into()),
        spill: None,
        msg: Message {
            text: MessageBody::new("hello".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
            reply_to: Some("6f1c9b0e-0000-4000-8000-000000000000".into()),
        },
    };
    assert_roundtrip(&entry);

    // Verify flattening — `reply_to` rides at the same top level as the rest of the Message.
    let json = serde_json::to_value(&entry).unwrap();
    assert!(json.get("text").is_some());
    assert!(json.get("msg").is_none());
    assert!(json.get("id").is_some());
    assert!(json.get("reply_to").is_some());
}

#[test]
fn test_ingestion_entry_without_id_still_parses() {
    // A line written before the envelope carried an id (or by a sender that didn't stamp one).
    let raw = r#"{"v":1,"ts":"2026-05-31T22:00:00Z","from":{"agent":"root"},"kind":"chat","summary":"hi","text":"hello"}"#;
    let entry: IngestionEntry = serde_json::from_str(raw).unwrap();
    assert!(entry.id.is_none());
    assert!(entry.msg.reply_to.is_none());
}

#[test]
fn test_message_kind_roundtrip() {
    let variants = [
        MessageKind::Chat,
        MessageKind::Event,
        MessageKind::Control(ControlKind::Shutdown {
            grace_ms: 100,
            force: true,
        }),
        MessageKind::Lifecycle(Lifecycle::Exiting {
            reason: "done".into(),
        }),
        MessageKind::Lifecycle(Lifecycle::Submitted {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "deadbeef".into(),
            reviewed: true,
        }),
    ];
    for v in variants {
        assert_roundtrip(&v);
    }
}

#[test]
fn test_message_kind_wire_pinning() {
    let chat: MessageKind = serde_json::from_str(r#""chat""#).unwrap();
    assert_eq!(chat, MessageKind::Chat);

    let event: MessageKind = serde_json::from_str(r#""event""#).unwrap();
    assert_eq!(event, MessageKind::Event);

    let control: MessageKind =
        serde_json::from_str(r#"{"control":{"shutdown":{"grace_ms":100}}}"#).unwrap();
    if let MessageKind::Control(ControlKind::Shutdown { grace_ms, force }) = control {
        assert_eq!(grace_ms, 100);
        assert!(!force); // default
    } else {
        panic!("not control");
    }
}

#[test]
fn test_control_kind_roundtrip() {
    let c = ControlKind::Shutdown {
        grace_ms: 500,
        force: false,
    };
    assert_roundtrip(&c);
}

#[test]
fn test_shutdown_status_roundtrip() {
    assert_roundtrip(&ShutdownStatus::Accepted);
    assert_roundtrip(&ShutdownStatus::Deferred);
}

#[test]
fn test_lifecycle_roundtrip() {
    let variants = [
        Lifecycle::Exiting {
            reason: "done".into(),
        },
        Lifecycle::ShutdownResponse {
            status: ShutdownStatus::Accepted,
            live_children: vec![AgentName::new("a".into()).unwrap()],
            busy: false,
            reason: "ok".into(),
        },
        Lifecycle::Submitted {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "deadbeef".into(),
            reviewed: false,
        },
    ];
    for v in variants {
        assert_roundtrip(&v);
    }
}

#[test]
fn test_lifecycle_wire_pinning() {
    let exiting: Lifecycle = serde_json::from_str(r#"{"type":"exiting","reason":"bye"}"#).unwrap();
    assert!(matches!(exiting, Lifecycle::Exiting { .. }));

    let resp: Lifecycle =
        serde_json::from_str(r#"{"type":"shutdown_response","status":"accepted"}"#).unwrap();
    if let Lifecycle::ShutdownResponse { status, .. } = resp {
        assert_eq!(status, ShutdownStatus::Accepted);
    } else {
        panic!("not shutdown_response");
    }

    // `reviewed` defaults to false when the sender omits it.
    let submitted: Lifecycle =
        serde_json::from_str(r#"{"type":"submitted","branch":"root.dev-0","sha":"abc"}"#).unwrap();
    if let Lifecycle::Submitted {
        branch,
        sha,
        reviewed,
    } = submitted
    {
        assert_eq!(branch.as_str(), "root.dev-0");
        assert_eq!(sha, "abc");
        assert!(!reviewed);
    } else {
        panic!("not submitted");
    }
}

#[test]
fn test_node_status_roundtrip() {
    let s = NodeStatus {
        node: NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
        kind: "dev".into(),
        branch: "main".into(),
        shutdown_pending: false,
        children: vec![ChildStatus {
            name: "c1".into(),
            busy: true,
        }],
        ts: Utc::now(),
    };
    let json = serde_json::to_string(&s).expect("failed to serialize");
    let back: NodeStatus = serde_json::from_str(&json).expect("failed to deserialize");

    // Manual comparison since NodeStatus doesn't impl PartialEq
    assert_eq!(s.node, back.node);
    assert_eq!(s.kind, back.kind);
    assert_eq!(s.branch, back.branch);
    assert_eq!(s.shutdown_pending, back.shutdown_pending);
    assert_eq!(s.children.len(), back.children.len());
    assert_eq!(s.children[0].name, back.children[0].name);
    assert_eq!(s.children[0].busy, back.children[0].busy);
    assert_eq!(s.ts, back.ts);
}

#[test]
fn test_role_record_roundtrip() {
    let rr = RoleRecord::new(&TestRole::Dev).unwrap();
    assert_roundtrip(&rr);
}

#[test]
fn test_node_papers_roundtrip() {
    let papers = NodePapers::new(
        NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
        Branch::new("main".into()).unwrap(),
        TestRole::Dev,
        PaneId::new("%1".into()).unwrap(),
        Some(InboxPath::new("/tmp/inbox".into())),
        true,
        false,
        false,
        Some(Branch::new("main".into()).unwrap()),
    )
    .unwrap();
    assert_roundtrip(&papers);
}

#[test]
fn test_child_record_roundtrip() {
    let r1 = ChildRecord::Spawned {
        child: AgentName::new("a".into()).unwrap(),
        kind: ChildKind::Worktree,
        pane: PaneId::new("%2".into()).unwrap(),
        inbox: InboxPath::new("/tmp/i".into()),
        model_label: None,
        model: None,
        directives_hash: None,
    };
    assert_roundtrip(&r1);

    // Tag pinning
    assert!(serde_json::to_string(&r1)
        .unwrap()
        .contains(r#""record":"spawned""#));

    // Existing children.jsonl lines must keep parsing: raw wire format for `Spawned`.
    let raw = r#"{"record":"spawned","child":"a","kind":"worktree","pane":"%2","inbox":"/tmp/i"}"#;
    let parsed: ChildRecord = serde_json::from_str(raw).unwrap();
    assert_eq!(parsed, r1);
}

#[test]
fn test_child_record_lifecycle_variants_roundtrip() {
    let child = AgentName::new("a".into()).unwrap();
    let at = Some(Utc::now());
    let variants = [
        ChildRecord::Reaped {
            child: child.clone(),
            at,
        },
        ChildRecord::Died {
            child: child.clone(),
            pane: PaneId::new("%2".into()).unwrap(),
            at,
        },
        ChildRecord::Submitted {
            child: child.clone(),
            branch: Branch::new("root.a".into()).unwrap(),
            sha: "deadbeef".into(),
            reviewed: true,
            at,
        },
    ];
    for v in &variants {
        assert_roundtrip(v);
    }
}

#[test]
fn test_child_record_lifecycle_wire_pinning() {
    // Tag names + `at`/`reviewed` defaulting when a writer omits them.
    let reaped: ChildRecord = serde_json::from_str(r#"{"record":"reaped","child":"a"}"#).unwrap();
    assert_eq!(
        reaped,
        ChildRecord::Reaped {
            child: AgentName::new("a".into()).unwrap(),
            at: None
        }
    );

    let died: ChildRecord =
        serde_json::from_str(r#"{"record":"died","child":"a","pane":"%2"}"#).unwrap();
    assert_eq!(
        died,
        ChildRecord::Died {
            child: AgentName::new("a".into()).unwrap(),
            pane: PaneId::new("%2".into()).unwrap(),
            at: None
        }
    );

    let submitted: ChildRecord =
        serde_json::from_str(r#"{"record":"submitted","child":"a","branch":"root.a","sha":"abc"}"#)
            .unwrap();
    assert_eq!(
        submitted,
        ChildRecord::Submitted {
            child: AgentName::new("a".into()).unwrap(),
            branch: Branch::new("root.a".into()).unwrap(),
            sha: "abc".into(),
            reviewed: false,
            at: None
        }
    );
}

#[test]
fn test_child_state_wire_pinning() {
    for (state, tag) in [
        (ChildState::Live, r#"{"state":"live"}"#),
        (ChildState::Reaped, r#"{"state":"reaped"}"#),
        (ChildState::Died, r#"{"state":"died"}"#),
    ] {
        assert_eq!(serde_json::to_string(&state).unwrap(), tag);
        assert_eq!(serde_json::from_str::<ChildState>(tag).unwrap(), state);
    }
    let submitted = ChildState::Submitted {
        sha: "abc".into(),
        reviewed: true,
    };
    assert_roundtrip(&submitted);
    assert!(serde_json::to_string(&submitted)
        .unwrap()
        .contains(r#""state":"submitted""#));
}

#[test]
fn test_topology_roundtrip() {
    let node = TreeNode {
        name: AgentName::new("root".into()).unwrap(),
        kind: None,
        pane: PaneId::new("%1".into()).unwrap(),
        pane_alive: true,
        state: None,
        model: None,
        model_label: None,
        directives_hash: None,
        children: vec![TreeNode {
            name: AgentName::new("child".into()).unwrap(),
            kind: Some(ChildKind::Worktree),
            pane: PaneId::new("%2".into()).unwrap(),
            pane_alive: false,
            state: Some(ChildState::Died),
            model: Some("sonnet".into()),
            model_label: Some("kimi".into()),
            directives_hash: Some(
                "deadbeef00000000000000000000000000000000000000000000000000000000".into(),
            ),
            children: vec![],
        }],
    };
    let view = TopologyView {
        node,
        parent: Some("boss".into()),
        path: vec![
            AgentName::new("boss".into()).unwrap(),
            AgentName::new("root".into()).unwrap(),
        ],
    };
    assert_roundtrip(&view);
}

#[test]
fn test_tree_node_wire_omits_directives_hash_when_none() {
    let node = TreeNode {
        name: AgentName::new("root".into()).unwrap(),
        kind: None,
        pane: PaneId::new("%1".into()).unwrap(),
        pane_alive: true,
        state: None,
        model: None,
        model_label: None,
        directives_hash: None,
        children: vec![],
    };
    assert!(!serde_json::to_string(&node)
        .unwrap()
        .contains("directives_hash"));
}

#[test]
fn test_old_wire_tree_node_without_directives_hash_still_parses() {
    // A `children.jsonl`-derived `TreeNode` written before `directives_hash` existed.
    let raw = r#"{"name":"child","pane":"%2","pane_alive":true,"children":[]}"#;
    let node: TreeNode = serde_json::from_str(raw).unwrap();
    assert_eq!(node.directives_hash, None);
}
