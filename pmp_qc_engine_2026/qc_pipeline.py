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
