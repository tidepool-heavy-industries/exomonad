#!/usr/bin/env python3
"""Mock GitHub API server for E2E testing.

Stateful HTTP server that mimics GitHub's REST API for PR workflows.
Tracks created PRs and auto-generates Copilot approval reviews.

Usage:
    MOCK_LOG=/tmp/mock.log python3 mock_github.py --port 8888

Endpoints:
    GET  /repos/{owner}/{repo}/pulls          - List open PRs
    POST /repos/{owner}/{repo}/pulls          - Create a PR
    PUT  /repos/{owner}/{repo}/pulls/{n}/merge - Merge a PR
    GET  /repos/{owner}/{repo}/pulls/{n}/reviews - Get reviews (auto-generates Copilot approval)
    GET  /repos/{owner}/{repo}/pulls/{n}      - Get PR details
    GET  /repos/{owner}/{repo}/pulls/{n}/comments - Get PR comments (empty array)
    GET  /repos/{owner}/{repo}/commits/{sha}/check-runs - Get commit check runs

All requests logged to $MOCK_LOG (one JSON object per line).
"""

import argparse
import datetime
import json
import os
import re
import signal
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse


class GitHubState:
    def __init__(self):
        self.prs = {}
        self.next_pr_number = 1

    def create_pr(self, owner, repo, title, head_ref, base_ref):
        number = self.next_pr_number
        self.next_pr_number += 1
        now = datetime.datetime.now(datetime.timezone.utc).isoformat()
        sha = "mocksha" + str(number)
        pr = {
            "number": number,
            "title": title,
            "head": {"ref": head_ref, "sha": sha},
            "base": {"ref": base_ref},
            "state": "open",
            "html_url": f"https://github.com/{owner}/{repo}/pull/{number}",
            "user": {"login": "test-bot"},
            "created_at": now,
            "updated_at": now,
        }
        self.prs[number] = pr
        return pr

    def merge_pr(self, number):
        if number in self.prs:
            self.prs[number]["state"] = "closed"
            self.prs[number]["updated_at"] = datetime.datetime.now(
                datetime.timezone.utc
            ).isoformat()
            return True
        return False


state = GitHubState()


class GitHubMockHandler(BaseHTTPRequestHandler):
    def _log_request(self, status_code):
        log_path = os.environ.get("MOCK_LOG", "/tmp/mock_github.log")
        log_entry = {
            "method": self.command,
            "path": self.path,
            "timestamp": datetime.datetime.now(datetime.timezone.utc).isoformat(),
            "status_code": status_code,
        }
        try:
            with open(log_path, "a") as f:
                f.write(json.dumps(log_entry) + "\n")
        except Exception as e:
            sys.stderr.write(f"Failed to write log: {e}\n")

    def _send_json(self, data, status_code=200):
        self.send_response(status_code)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps(data).encode("utf-8"))
        self._log_request(status_code)

    def _send_error(self, status_code, message):
        self.send_response(status_code)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps({"message": message}).encode("utf-8"))
        self._log_request(status_code)

    def do_GET(self):
        path = urlparse(self.path).path

        # GET /repos/{owner}/{repo}/pulls
        m = re.match(r"^/repos/([^/]+)/([^/]+)/pulls$", path)
        if m:
            open_prs = [pr for pr in state.prs.values() if pr["state"] == "open"]
            return self._send_json(open_prs)

        # GET /repos/{owner}/{repo}/pulls/{n}/reviews
        m = re.match(r"^/repos/([^/]+)/([^/]+)/pulls/(\d+)/reviews$", path)
        if m:
            pr_number = int(m.group(3))
            if pr_number in state.prs:
                now = datetime.datetime.now(datetime.timezone.utc).isoformat()
                review = {
                    "id": 1,
                    "user": {"login": "copilot[bot]"},
                    "state": "APPROVED",
                    "submitted_at": now,
                }
                return self._send_json([review])
            return self._send_error(404, "PR not found")

        # GET /repos/{owner}/{repo}/pulls/{n}/comments
        m = re.match(r"^/repos/([^/]+)/([^/]+)/pulls/(\d+)/comments$", path)
        if m:
            pr_number = int(m.group(3))
            if pr_number in state.prs:
                return self._send_json([])
            return self._send_error(404, "PR not found")

        # GET /repos/{owner}/{repo}/pulls/{n}
        m = re.match(r"^/repos/([^/]+)/([^/]+)/pulls/(\d+)$", path)
        if m:
            pr_number = int(m.group(3))
            if pr_number in state.prs:
                return self._send_json(state.prs[pr_number])
            return self._send_error(404, "PR not found")

        # GET /repos/{owner}/{repo}/commits/{sha}/check-runs
        m = re.match(r"^/repos/([^/]+)/([^/]+)/commits/([^/]+)/check-runs$", path)
        if m:
            data = {
                "total_count": 1,
                "check_runs": [
                    {
                        "name": "build",
                        "status": "completed",
                        "conclusion": "success",
                    }
                ],
            }
            return self._send_json(data)

        return self._send_error(404, "Not Found")

    def do_POST(self):
        path = urlparse(self.path).path

        # POST /repos/{owner}/{repo}/pulls
        m = re.match(r"^/repos/([^/]+)/([^/]+)/pulls$", path)
        if m:
            owner, repo = m.groups()
            content_length = int(self.headers.get("Content-Length", 0))
            body = self.rfile.read(content_length).decode("utf-8")
            try:
                data = json.loads(body)
                title = data.get("title", "No Title")
                head = data.get("head", "main")
                base = data.get("base", "main")
                pr = state.create_pr(owner, repo, title, head, base)
                return self._send_json(pr, 201)
            except json.JSONDecodeError:
                return self._send_error(400, "Invalid JSON")

        return self._send_error(404, "Not Found")

    def do_PUT(self):
        path = urlparse(self.path).path

        # PUT /repos/{owner}/{repo}/pulls/{n}/merge
        m = re.match(r"^/repos/([^/]+)/([^/]+)/pulls/(\d+)/merge$", path)
        if m:
            pr_number = int(m.group(3))
            if state.merge_pr(pr_number):
                return self._send_json({"merged": True, "sha": "abc123"})
            return self._send_error(404, "PR not found")

        return self._send_error(404, "Not Found")


def run_server():
    parser = argparse.ArgumentParser(description="Mock GitHub API server")
    parser.add_argument("--host", default="127.0.0.1", help="Host to listen on")
    parser.add_argument("--port", type=int, default=8888, help="Port to listen on")
    args = parser.parse_args()

    httpd = HTTPServer((args.host, args.port), GitHubMockHandler)

    def signal_handler(sig, frame):
        sys.stderr.write("\nShutting down Mock GitHub API...\n")
        httpd.server_close()
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    sys.stderr.write(f"Mock GitHub API listening on {args.host}:{args.port}\n")
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass


if __name__ == "__main__":
    run_server()
