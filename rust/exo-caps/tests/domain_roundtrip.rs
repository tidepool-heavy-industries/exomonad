use chrono::Utc;
use exo_caps::types::{
    AgentName, DomainPayload, IngestionEntry, Message, MessageBody, MessageKind, Persona, Summary,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct TestDomainSystem {
    foo: String,
    bar: u32,
}

#[test]
fn domain_message_in_ingestion_entry_round_trips() {
    let system = TestDomainSystem {
        foo: "hello".into(),
        bar: 42,
    };
    let json = serde_json::to_string(&system).unwrap();

    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        from: Persona::Agent(AgentName::new("dev".into()).unwrap()),
        msg: Message {
            text: MessageBody::new("domain msg".into()).unwrap(),
            summary: Summary::new("summary".into()).unwrap(),
            kind: MessageKind::Domain(DomainPayload(json)),
        },
    };

    let serialized = serde_json::to_string(&entry).unwrap();
    // Prior to the fix, this would fail to deserialize because RawValue
    // cannot be read from the buffered Content map used by #[serde(flatten)].
    let deserialized: IngestionEntry = serde_json::from_str(&serialized)
        .expect("Failed to deserialize IngestionEntry with Domain payload");

    assert_eq!(entry, deserialized);
}

#[test]
fn chat_message_in_ingestion_entry_round_trips() {
    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        from: Persona::Agent(AgentName::new("dev".into()).unwrap()),
        msg: Message {
            text: MessageBody::new("chat msg".into()).unwrap(),
            summary: Summary::new("summary".into()).unwrap(),
            kind: MessageKind::Chat,
        },
    };

    let serialized = serde_json::to_string(&entry).unwrap();
    let deserialized: IngestionEntry = serde_json::from_str(&serialized)
        .expect("Failed to deserialize IngestionEntry with Chat payload");

    assert_eq!(entry, deserialized);
}
