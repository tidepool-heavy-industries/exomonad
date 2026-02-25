-- v0.3.0 Migration: Update push notification configs to support multiple configs per task
-- This migration enhances the push_notification_configs table to support the v0.3.0 spec

-- Drop the old table (backing up data if needed in production)
DROP TABLE IF EXISTS push_notification_configs;

-- Create new table with support for multiple configs per task
CREATE TABLE IF NOT EXISTS push_notification_configs (
    id TEXT PRIMARY KEY,  -- Unique config ID
    task_id TEXT NOT NULL,  -- Task this config belongs to
    url TEXT NOT NULL,  -- Webhook URL
    token TEXT,  -- Optional authentication token
    authentication JSONB,  -- Optional authentication scheme (OAuth2, etc.)
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT (datetime('now')),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT (datetime('now')),
    FOREIGN KEY (task_id) REFERENCES tasks(id) ON DELETE CASCADE
);

-- Index for efficient lookups
CREATE INDEX IF NOT EXISTS idx_push_configs_task_id ON push_notification_configs(task_id);

-- Trigger to automatically update the updated_at timestamp
CREATE TRIGGER IF NOT EXISTS update_push_configs_updated_at
    AFTER UPDATE ON push_notification_configs
    FOR EACH ROW
BEGIN
    UPDATE push_notification_configs SET updated_at = datetime('now') WHERE id = NEW.id;
END;
