import pandas as pd
import numpy as np
import io
import re
import argparse

# Dataset Loader
class DatasetLoader:
    def __init__(self, file_path):
        self.file_path = file_path
    def load(self):
        with open(self.file_path, "r", encoding="utf-8") as f:
            lines = f.readlines()
        # Header line (robust to spacing)
        start = next(
            i for i, l in enumerate(lines)
            if l.strip().startswith("Study Subject ID")
        )
        metadata = lines[:start]
        data_text = "".join(lines[start:])
        df = pd.read_csv(io.StringIO(data_text), sep="\t", dtype=str, engine="python")
        # Column names, remove white spaces
        df.columns = [c.strip().replace(" ", ".") for c in df.columns]
        return df, metadata


# Helper functions
def is_missing(x):
    if x.dtype == "O":
        return x.isna() | (x.astype(str).str.strip() == "")
    return x.isna()

def parse_range(x):
    if pd.isna(x):
        return None
    x = str(x)
    x = re.sub(r"c\(|\)", "", x)
    vals = [v.strip() for v in x.split(",") if v.strip() != ""]
    try:
        return [float(v) for v in vals]
    except:
        return vals

def is_year_var(varname):
    return ("YEAR" in varname) or ("DATE" in varname) or ("DOB" in varname)

def extract_year(x):
    return pd.to_numeric(x.astype(str).str[:4], errors="coerce")

# Lookup rules
def build_rules(lookup):
    rules = {}
    for _, r in lookup.iterrows():
        var = r["names.data."]
        type_ = r["type_of"]
        range_ = r["range_of"]
        if pd.isna(type_) or pd.isna(range_):
            continue

        parsed = parse_range(range_)
        if parsed is None:
            continue
        if "continuous" in str(type_):
            rules[var] = {
                "type": "continuous",
                "min": min(parsed),
                "max": max(parsed)
            }
        elif "discrete" in str(type_):
            rules[var] = {
                "type": "discrete",
                "values": parsed
            }
    return rules


# QC Engine
def run_qc(df, rules, id_col="Study.Subject.ID"):
    summary_rows = []
    audit_rows = []
    ids = df[id_col]
    for var, rule in rules.items():
        if var not in df.columns:
            continue
        x = df[var]
        miss = is_missing(x)
        miss_pct = miss.mean() * 100
        x_num = pd.to_numeric(x, errors="coerce")
        if is_year_var(var):
            x_num = extract_year(x)
        valid = np.zeros(len(df), dtype=bool)


        # Evaluation checks
        if rule["type"] == "continuous":
            valid = (~x_num.isna()) & (x_num >= rule["min"]) & (x_num <= rule["max"])
        elif rule["type"] == "discrete":
            valid = (~x_num.isna()) & x_num.isin(rule["values"])

        # Summary
        summary_rows.append({
            "variable": var,
            "missing_pct": miss_pct,
            "in_range_pct": valid.mean() * 100,
            "out_of_range_pct": ((~valid) & (~miss)).mean() * 100
        })

        # Audit patient-level
        for i in range(len(df)):
            if miss.iloc[i]:
                audit_rows.append({
                    "subject_id": ids.iloc[i],
                    "variable": var,
                    "issue_type": "MISSING",
                    "value": None,
                    "expected": None,
                    "status": "FAIL"
                })
            elif not valid[i]:
                audit_rows.append({
                    "subject_id": ids.iloc[i],
                    "variable": var,
                    "issue_type": "OUT_OF_RANGE",
                    "value": x.iloc[i],
                    "expected": rule,
                    "status": "FAIL"
                })
    return pd.DataFrame(summary_rows), pd.DataFrame(audit_rows)
