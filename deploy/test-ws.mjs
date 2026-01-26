/**
 * Simple WebSocket test client for the ExoMonad worker.
 * Usage: node test-ws.mjs
 */

import { WebSocket } from 'ws';

const WS_URL = process.env.WS_URL || 'ws://localhost:65485/session/test123';

console.log(`Connecting to ${WS_URL}...`);

const ws = new WebSocket(WS_URL);

ws.on('open', () => {
  console.log('Connected!');

  // Send init message
  const msg = {
    type: 'init',
    input: { messageText: 'I need a refund for my order' }
  };
  console.log('Sending:', JSON.stringify(msg));
  ws.send(JSON.stringify(msg));
});

ws.on('message', (data) => {
  const msg = JSON.parse(data.toString());
  console.log('Received:', JSON.stringify(msg, null, 2));

  if (msg.type === 'done' || msg.type === 'error') {
    console.log('Closing connection...');
    ws.close();
  }
});

ws.on('close', (code, reason) => {
  console.log(`Connection closed: ${code} ${reason}`);
  process.exit(0);
});

ws.on('error', (err) => {
  console.error('WebSocket error:', err.message);
  process.exit(1);
});

// Timeout after 30 seconds
setTimeout(() => {
  console.log('Timeout - closing');
  ws.close();
  process.exit(1);
}, 30000);
