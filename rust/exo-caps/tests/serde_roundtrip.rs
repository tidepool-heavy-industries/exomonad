use exo_caps::*;
use serde_json::json;
use chrono::Utc;

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
            TestRole::Root => AgentType::Claude,
            TestRole::Dev => AgentType::Gemini,
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
    ]).unwrap();
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
    for variant in [AgentType::Claude, AgentType::Gemini, AgentType::Shoal] {
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
    assert_eq!(serde_json::to_value(&p2).unwrap(), json!({"synthetic": "github"}));
}

#[test]
fn test_message_roundtrip() {
    let m = Message {
        text: MessageBody::new("body".into()).unwrap(),
        summary: Summary::new("summary".into()).unwrap(),
        kind: MessageKind::Chat,
    };
    assert_roundtrip(&m);
}

#[test]
fn test_ingestion_entry_roundtrip() {
    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        from: Persona::Agent(AgentName::new("root".into()).unwrap()),
        msg: Message {
            text: MessageBody::new("hello".into()).unwrap(),
            summary: Summary::new("hi".into()).unwrap(),
            kind: MessageKind::Chat,
        },
    };
    assert_roundtrip(&entry);

    // Verify flattening
    let json = serde_json::to_value(&entry).unwrap();
    assert!(json.get("text").is_some());
    assert!(json.get("msg").is_none());
}

#[test]
fn test_message_kind_roundtrip() {
    let variants = [
        MessageKind::Chat,
        MessageKind::Event,
        MessageKind::Control(ControlKind::Shutdown { grace_ms: 100, force: true }),
        MessageKind::Lifecycle(Lifecycle::ChildIdle { summary: "done".into() }),
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

    let control: MessageKind = serde_json::from_str(r#"{"control":{"shutdown":{"grace_ms":100}}}"#).unwrap();
    if let MessageKind::Control(ControlKind::Shutdown { grace_ms, force }) = control {
        assert_eq!(grace_ms, 100);
        assert!(!force); // default
    } else {
        panic!("not control");
    }
}

#[test]
fn test_control_kind_roundtrip() {
    let c = ControlKind::Shutdown { grace_ms: 500, force: false };
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
        Lifecycle::ChildIdle { summary: "idle".into() },
        Lifecycle::ChildExited { reason: "done".into() },
        Lifecycle::ShutdownResponse {
            status: ShutdownStatus::Accepted,
            live_children: vec!["a".into()],
            busy: false,
            reason: "ok".into(),
        },
    ];
    for v in variants {
        assert_roundtrip(&v);
    }
}

#[test]
fn test_lifecycle_wire_pinning() {
    let idle: Lifecycle = serde_json::from_str(r#"{"type":"child_idle","summary":"ok"}"#).unwrap();
    assert!(matches!(idle, Lifecycle::ChildIdle { .. }));

    let exited: Lifecycle = serde_json::from_str(r#"{"type":"child_exited","reason":"bye"}"#).unwrap();
    assert!(matches!(exited, Lifecycle::ChildExited { .. }));

    let resp: Lifecycle = serde_json::from_str(r#"{"type":"shutdown_response","status":"accepted"}"#).unwrap();
    if let Lifecycle::ShutdownResponse { status, .. } = resp {
        assert_eq!(status, ShutdownStatus::Accepted);
    } else {
        panic!("not shutdown_response");
    }
}

#[test]
fn test_node_status_roundtrip() {
    let s = NodeStatus {
        node: NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
        kind: "dev".into(),
        branch: "main".into(),
        shutdown_pending: false,
        children: vec![ChildStatus { name: "c1".into(), busy: true }],
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
    ).unwrap();
    assert_roundtrip(&papers);
}

#[test]
fn test_child_record_roundtrip() {
    let r1 = ChildRecord::Spawned {
        child: AgentName::new("a".into()).unwrap(),
        kind: ChildKind::Worktree,
        pane: PaneId::new("%2".into()).unwrap(),
        inbox: InboxPath::new("/tmp/i".into()),
    };
    let r2 = ChildRecord::Started { child: AgentName::new("a".into()).unwrap() };
    assert_roundtrip(&r1);
    assert_roundtrip(&r2);

    // Tag pinning
    assert!(serde_json::to_string(&r1).unwrap().contains(r#""record":"spawned""#));
    assert!(serde_json::to_string(&r2).unwrap().contains(r#""record":"started""#));
}

#[test]
fn test_child_lifecycle_roundtrip() {
    assert_roundtrip(&ChildLifecycle::Spawned);
    assert_roundtrip(&ChildLifecycle::Started);
}

#[test]
fn test_topology_roundtrip() {
    let node = TreeNode {
        name: "root".into(),
        kind: None,
        pane: "%1".into(),
        pane_alive: true,
        children: vec![TreeNode {
            name: "child".into(),
            kind: Some(ChildKind::Worktree),
            pane: "%2".into(),
            pane_alive: false,
            children: vec![],
        }],
    };
    let view = TopologyView {
        node,
        parent: Some("boss".into()),
        path: vec!["boss".into(), "root".into()],
    };
    assert_roundtrip(&view);
}
