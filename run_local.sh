#!/bin/bash
# Run 5 Buckets Impact Dashboard locally (v3 — app.R, port 3838)

cd "$(dirname "$0")"

echo "Starting 5 Buckets Impact Dashboard on http://127.0.0.1:3838"
echo "Press Ctrl+C to stop"
echo ""

Rscript run_app.R 2>&1
