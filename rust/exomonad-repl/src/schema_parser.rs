#[derive(Debug, Clone)]
pub struct ToolSchema {
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone)]
pub struct Property {
    pub name: String,
    pub kind: PropertyKind,
    pub required: bool,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum PropertyKind {
    String,
    Number,
    Boolean,
    Enum { options: Vec<String> },
    Array { item_type: Box<PropertyKind> },
    Object { schema: Box<ToolSchema> },
    Unknown,
}

impl ToolSchema {
    pub fn from_json(value: &serde_json::Value) -> Self {
        let mut properties = Vec::new();
        let mut required = Vec::new();

        if let Some(req) = value.get("required").and_then(|v| v.as_array()) {
            for r in req {
                if let Some(s) = r.as_str() {
                    required.push(s.to_string());
                }
            }
        }

        if let Some(props) = value.get("properties").and_then(|v| v.as_object()) {
            for (name, prop_val) in props {
                let kind = parse_kind(prop_val);
                let is_required = required.contains(name);
                
                properties.push(Property {
                    name: name.clone(),
                    kind,
                    required: is_required,
                });
            }
        }

        Self {
            properties,
        }
    }
}

fn parse_kind(v: &serde_json::Value) -> PropertyKind {
    if let Some(enum_vals) = v.get("enum").and_then(|v| v.as_array()) {
        let options = enum_vals.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect();
        return PropertyKind::Enum { options };
    }

    match v.get("type").and_then(|v| v.as_str()) {
        Some("string") => PropertyKind::String,
        Some("number") | Some("integer") => PropertyKind::Number,
        Some("boolean") => PropertyKind::Boolean,
        Some("array") => {
            let item_type = v.get("items").map(|v| Box::new(parse_kind(v))).unwrap_or(Box::new(PropertyKind::Unknown));
            PropertyKind::Array { item_type }
        }
        Some("object") => {
            PropertyKind::Object { schema: Box::new(ToolSchema::from_json(v)) }
        }
        _ => PropertyKind::Unknown,
    }
}
