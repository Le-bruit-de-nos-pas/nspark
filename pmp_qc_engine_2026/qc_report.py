import pandas as pd

def generate_html_report(summary_path, audit_path, output_html):
    
    # Load data
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
    # Convert tables to HTML
    # ------------------------
    summary_html = summary.to_html(index=False, classes="table table-summary")
    audit_html = audit.to_html(index=False, classes="table table-audit")

    # ------------------------
    # Full HTML
    # ------------------------
    html = f"""
    <html>
    <head>
        <title>QC Report</title>
        <style>
            body {{
                background-color: #0b0f17;
                color: #e6edf3;
                font-family: Arial;
                padding: 20px;
            }}

            h1, h2 {{
                color: #60a5fa;
            }}

            .card {{
                background: #111827;
                padding: 15px;
                margin-bottom: 20px;
                border-radius: 10px;
                border: 1px solid #1f2937;
            }}

            .metrics {{
                display: flex;
                gap: 20px;
            }}

            .metric {{
                background: #0f172a;
                padding: 10px;
                border-radius: 8px;
                text-align: center;
                flex: 1;
            }}

            table {{
                width: 100%;
                border-collapse: collapse;
                font-size: 12px;
            }}

            th, td {{
                padding: 8px;
                border-bottom: 1px solid #1f2937;
            }}

            th {{
                background-color: #1f2937;
            }}

            tr:hover {{
                background-color: #1e293b;
            }}
        </style>
    </head>

    <body>

    <h1>QC Report</h1>

    <div class="card">
        <h2>Summary Metrics</h2>
        <div class="metrics">
            <div class="metric">
                <h3>{total_vars}</h3>
                <p>Variables Checked</p>
            </div>
            <div class="metric">
                <h3>{total_issues}</h3>
                <p>Total Issues</p>
            </div>
            <div class="metric">
                <h3>{missing_vars}</h3>
                <p>Vars with Missing</p>
            </div>
            <div class="metric">
                <h3>{out_range_vars}</h3>
                <p>Vars Out of Range</p>
            </div>
        </div>
    </div>

    <div class="card">
        <h2>QC Summary</h2>
        {summary_html}
    </div>

    <div class="card">
        <h2>QC Patient-level Audit</h2>
        {audit_html}
    </div>

    </body>
    </html>
    """

    # Save file
    with open(output_html, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"HTML report saved: {output_html}")


# ------------------------
# Run
# ------------------------
if __name__ == "__main__":
    generate_html_report(
        summary_path="today_qc_summary.csv",
        audit_path="today_qc_audit.csv",
        output_html="qc_report.html"
    )