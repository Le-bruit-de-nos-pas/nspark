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
