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

All requests logged to $MOCK_LOG (one JSON object per line).
"""
# TODO: implement (Wave 1, Leaf B)
