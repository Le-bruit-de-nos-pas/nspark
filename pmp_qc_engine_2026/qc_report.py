import pandas as pd

def generate_html_report(summary_path, audit_path, output_html):
    
    # ------------------------
    # Load data
    # ------------------------
    summary = pd.read_csv(summary_path)
    audit = pd.read_csv(audit_path)

    # ------------------------
    # Basic stats
    # ------------------------
    total_vars = len(summary)
    total_issues = len(audit)

    missing_vars = (summary["missing_pct"] > 0).sum()
    out_range_vars = (summary["out_of_range_pct"] > 0).sum()

    # ------------------------
    # Add PASS / FAIL status
    # ------------------------
    summary["status"] = summary.apply(
        lambda r: "FAIL" if (r["missing_pct"] > 0 or r["out_of_range_pct"] > 0) else "PASS",
        axis=1
    )

    # ------------------------
    # Row styling functions
    # ------------------------
    def highlight_summary(row):
        if row["status"] == "FAIL":
            return ['background-color: #2d1111'] * len(row)  # dark red
        return [''] * len(row)

    def highlight_audit(row):
        return ['background-color: #2d1111'] * len(row)

    # Apply styling
    summary_html = (
        summary.style
        .apply(highlight_summary, axis=1)
        .format(precision=2)
        .to_html()
    )

    audit_html = (
        audit.style
        .apply(highlight_audit, axis=1)
        .to_html()
    )

    # ------------------------
    # HTML Template
    # ------------------------
    html = f"""
    <html>
    <head>
        <title>QC Report</title>

        <style>
            body {{
                background-color: #0d1117;
                color: #c9d1d9;
                font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
                margin: 0;
                padding: 30px;
            }}

            h1 {{
                margin-bottom: 5px;
            }}

            h2 {{
                margin-top: 0;
                color: #58a6ff;
            }}

            .subtitle {{
                color: #8b949e;
                margin-bottom: 30px;
            }}

            .card {{
                background: #161b22;
                border: 1px solid #30363d;
                border-radius: 10px;
                padding: 20px;
                margin-bottom: 25px;
            }}

            .metrics {{
                display: flex;
                gap: 20px;
                flex-wrap: wrap;
            }}

            .metric {{
                background: #0d1117;
                border: 1px solid #30363d;
                padding: 15px;
                border-radius: 8px;
                flex: 1;
                text-align: center;
            }}

            .metric h3 {{
                margin: 0;
                font-size: 22px;
                color: #58a6ff;
            }}

            .metric p {{
                margin: 5px 0 0 0;
                font-size: 12px;
                color: #8b949e;
            }}

            table {{
                width: 100%;
                border-collapse: collapse;
                font-size: 12px;
            }}

            th {{
                position: sticky;
                top: 0;
                background-color: #161b22;
                border-bottom: 1px solid #30363d;
                padding: 8px;
                text-align: left;
            }}

            td {{
                padding: 8px;
                border-bottom: 1px solid #30363d;
            }}

            tr:hover {{
                background-color: #161b22;
            }}

            .badge {{
                padding: 3px 8px;
                border-radius: 6px;
                font-size: 11px;
                font-weight: bold;
            }}

            .pass {{
                background-color: #033a16;
                color: #2ea043;
            }}

            .fail {{
                background-color: #3b0d0c;
                color: #f85149;
            }}
        </style>
    </head>

    <body>

    <h1>QC Report</h1>
    <div class="subtitle">Clinical Data Validation • Automated QC Engine</div>

    <!-- Metrics -->
    <div class="card">
        <h2>Summary Metrics</h2>
        <div class="metrics">
            <div class="metric">
                <h3>{total_vars}</h3>
                <p>Total Variables</p>
            </div>
            <div class="metric">
                <h3>{total_issues}</h3>
                <p>Total Issues</p>
            </div>
            <div class="metric">
                <h3>{missing_vars}</h3>
                <p>Variables with Missing</p>
            </div>
            <div class="metric">
                <h3>{out_range_vars}</h3>
                <p>Variables Out of Range</p>
            </div>
        </div>
    </div>

    <!-- Summary Table -->
    <div class="card">
        <h2>QC Summary</h2>
        {summary_html}
    </div>

    <!-- Audit Table -->
    <div class="card">
        <h2>Patient-level Audit Trail</h2>
        {audit_html}
    </div>

    </body>
    </html>
    """

    # ------------------------
    # Inject badges (post-processing)
    # ------------------------
    html = html.replace(
        ">PASS<",
        '><span class="badge pass">PASS</span><'
    ).replace(
        ">FAIL<",
        '><span class="badge fail">FAIL</span><'
    )

    # ------------------------
    # Save file
    # ------------------------
    with open(output_html, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"HTML report saved: {output_html}")


# ------------------------
# CLI
# ------------------------
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate QC HTML report")

    parser.add_argument("--summary", required=True, help="Path to QC summary CSV")
    parser.add_argument("--audit", required=True, help="Path to QC audit CSV")
    parser.add_argument("--out", default="qc_report.html", help="Output HTML file")

    args = parser.parse_args()

    generate_html_report(
        summary_path=args.summary,
        audit_path=args.audit,
        output_html=args.out
    )