/**
 * WebSocket test client for the Tidepool graph execution protocol.
 *
 * Usage:
 *   node test/client.mjs                    # Uses default URL and input
 *   WS_URL=ws://localhost:8787/session/test node test/client.mjs
 *   GRAPH_ID=MyGraph INPUT='{"foo":1}' node test/client.mjs
 *
 * See docs/PROTOCOL.md for the full protocol specification.
 */

import WebSocket from 'ws';

const WS_URL = process.env.WS_URL || 'ws://localhost:8787/session/test123';
const GRAPH_ID = process.env.GRAPH_ID || 'test';
const INPUT = process.env.INPUT ? JSON.parse(process.env.INPUT) : 5;

console.log(`Connecting to ${WS_URL}...`);
console.log(`Graph: ${GRAPH_ID}, Input: ${JSON.stringify(INPUT)}`);

const ws = new WebSocket(WS_URL);

// Track session ID for potential reconnection
let sessionId = null;

ws.on('open', () => {
  console.log('Connected!');

  // Send init message with graphId
  const msg = {
    type: 'init',
    graphId: GRAPH_ID,
    input: INPUT
  };
  console.log('Sending:', JSON.stringify(msg));
  ws.send(JSON.stringify(msg));
});

ws.on('message', (data) => {
  const msg = JSON.parse(data.toString());

  switch (msg.type) {
    case 'progress':
      console.log(`[Progress] Effect: ${msg.effect.type}, Status: ${msg.status}`);
      break;

    case 'yield':
      console.log('[Yield] Server yielded effect for client handling:');
      console.log('  Effect:', JSON.stringify(msg.effect, null, 2));
      console.log('  SessionId:', msg.sessionId);
      sessionId = msg.sessionId;
      // In a real client, you would handle the effect and send a resume message:
      // ws.send(JSON.stringify({ type: 'resume', result: { type: 'success', value: ... } }));
      break;

    case 'done':
      console.log('[Done] Graph completed successfully!');
      console.log('Result:', JSON.stringify(msg.result, null, 2));
      ws.close();
      break;

    case 'error':
      console.error('[Error]', msg.message);
      console.log('  Recoverable:', msg.recoverable);
      if (msg.sessionId) {
        console.log('  SessionId:', msg.sessionId);
        sessionId = msg.sessionId;
      }
      if (!msg.recoverable) {
        ws.close();
      }
      break;

    case 'pong':
      console.log('[Pong] Keepalive response received');
      break;

    default:
      console.log('Unknown message type:', msg);
  }
});

ws.on('close', (code, reason) => {
  console.log(`Connection closed: ${code} ${reason}`);
  if (sessionId) {
    console.log(`Session ID for reconnection: ${sessionId}`);
  }
  process.exit(0);
});

ws.on('error', (err) => {
  console.error('WebSocket error:', err.message);
  process.exit(1);
});

// Keepalive ping every 30 seconds
const pingInterval = setInterval(() => {
  if (ws.readyState === WebSocket.OPEN) {
    console.log('[Ping] Sending keepalive...');
    ws.send(JSON.stringify({ type: 'ping' }));
  }
}, 30000);

// Timeout - configurable via TIMEOUT_MS env var (default 60 seconds)
// Exit code 1 indicates test failure (graph didn't complete in time)
const TIMEOUT_MS = parseInt(process.env.TIMEOUT_MS || '60000', 10);
setTimeout(() => {
  clearInterval(pingInterval);
  console.log(`Timeout after ${TIMEOUT_MS}ms - graph did not complete`);
  ws.close();
  process.exit(1);
}, TIMEOUT_MS);

// Clean up on exit
process.on('SIGINT', () => {
  clearInterval(pingInterval);
  ws.close();
  process.exit(0);
});
