# Import libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
import pyreadstat

# Read SAS tables
vs, _ = pyreadstat.read_sas7bdat("../data/vs.sas7bdat")
dm, _ = pyreadstat.read_sas7bdat("../data/dm.sas7bdat")
sc, _ = pyreadstat.read_sas7bdat("../data/sc.sas7bdat")
ra, _ = pyreadstat.read_sas7bdat("../data/ra.sas7bdat")
cm, _ = pyreadstat.read_sas7bdat("../data/cm.sas7bdat")
pt, _ = pyreadstat.read_sas7bdat("../data/pt.sas7bdat")
groupe, _ = pyreadstat.read_sas7bdat("../data/groupe.sas7bdat")

print(groupe['RAN_GRP_LIB'].value_counts())
print(sc.columns.tolist())

# Define column lists for each scale
tremor_cols = ['SCMDS2_10', 'SCMDS3_15A', 'SCMDS3_15B', 'SCMDS3_16A', 'SCMDS3_16B',
               'SCMDS3_17A', 'SCMDS3_17B', 'SCMDS3_17C', 'SCMDS3_17D', 'SCMDS3_17E', 'SCMDS3_18']

non_tremor_cols = (['SCMDS2_1', 'SCMDS2_2', 'SCMDS2_3', 'SCMDS2_4', 'SCMDS2_5', 'SCMDS2_6', 'SCMDS2_7',
                    'SCMDS2_8', 'SCMDS2_9', 'SCMDS2_11', 'SCMDS2_12', 'SCMDS2_13'] +
                   ['SCMDS3_1', 'SCMDS3_2', 'SCMDS3_3A', 'SCMDS3_3B', 'SCMDS3_3C', 'SCMDS3_3D', 'SCMDS3_3E',
                    'SCMDS3_4A', 'SCMDS3_4B', 'SCMDS3_5A', 'SCMDS3_5B', 'SCMDS3_6A', 'SCMDS3_6B',
                    'SCMDS3_7A', 'SCMDS3_7B', 'SCMDS3_8A', 'SCMDS3_8B', 'SCMDS3_9', 'SCMDS3_10',
                    'SCMDS3_11', 'SCMDS3_12', 'SCMDS3_13', 'SCMDS3_14'])

eleven_cols = ['SCMDS1_13', 'SCMDS2_1', 'SCMDS2_4', 'SCMDS2_5', 'SCMDS2_6', 'SCMDS2_7',
               'SCMDS2_8', 'SCMDS2_9', 'SCMDS2_11', 'SCMDS2_12', 'SCMDS2_13']

mds3_cols = (['SCMDS3_1', 'SCMDS3_2', 'SCMDS3_3A', 'SCMDS3_3B', 'SCMDS3_3C', 'SCMDS3_3D', 'SCMDS3_3E',
              'SCMDS3_4A', 'SCMDS3_4B', 'SCMDS3_5A', 'SCMDS3_5B', 'SCMDS3_6A', 'SCMDS3_6B',
              'SCMDS3_7A', 'SCMDS3_7B', 'SCMDS3_8A', 'SCMDS3_8B', 'SCMDS3_9', 'SCMDS3_10',
              'SCMDS3_11', 'SCMDS3_12', 'SCMDS3_13', 'SCMDS3_14', 'SCMDS3_15A', 'SCMDS3_15B',
              'SCMDS3_16A', 'SCMDS3_16B', 'SCMDS3_17A', 'SCMDS3_17B', 'SCMDS3_17C', 'SCMDS3_17D',
              'SCMDS3_17E', 'SCMDS3_18'])


# Compute scale sums 
sc['tremor_mod_scale'] = sc[tremor_cols].sum(axis=1, skipna=True)
sc['non_tremor_mod_scale'] = sc[non_tremor_cols].sum(axis=1, skipna=True)
sc['eleven_item_mod_scale'] = sc[eleven_cols].sum(axis=1, skipna=True)
sc['mds_partIII_total'] = sc[mds3_cols].sum(axis=1, skipna=True)

# Count missing items per scale
sc['tremor_items_missing'] = sc[tremor_cols].isna().sum(axis=1)
sc['non_tremor_items_missing'] = sc[non_tremor_cols].isna().sum(axis=1)
sc['eleven_item_missing'] = sc[eleven_cols].isna().sum(axis=1)
sc['mds_partIII_missing'] = sc[mds3_cols].isna().sum(axis=1)

# Check new columns
print(sc[['tremor_mod_scale', 'non_tremor_mod_scale', 'eleven_item_mod_scale', 'mds_partIII_total',
          'tremor_items_missing', 'non_tremor_items_missing', 'eleven_item_missing', 'mds_partIII_missing']].head())


# Select relevant columns
sc = sc[['USUBJID', 'VISITNUM', 'VISITNAM',
         'tremor_mod_scale', 'non_tremor_mod_scale', 'eleven_item_mod_scale',
         'tremor_items_missing', 'non_tremor_items_missing', 'eleven_item_missing',
         'mds_partIII_total', 'mds_partIII_missing']].copy()

# Filter for baseline, 6 months, 12 months
allowed_visits = ["Visit 2 - Baseline", "Visit 6 - Follow-up", "Visit 8 - Follow-up"]
sc = sc[sc['VISITNAM'].isin(allowed_visits)]

# See how many missing
print(sc['tremor_items_missing'].value_counts().sort_index())
print(sc['non_tremor_items_missing'].value_counts().sort_index())
print(sc['eleven_item_missing'].value_counts().sort_index())
print(sc['mds_partIII_missing'].value_counts().sort_index())

# Identify rows where scale is completely missing
sc_complete_missing = sc[
    (sc['tremor_items_missing'] == 11) |
    (sc['non_tremor_items_missing'] == 35) |
    (sc['eleven_item_missing'] == 11) |
    (sc['mds_partIII_missing'] == 33)
]

# Check these rows (select USUBJID, VISITNAM, and all columns ending with "missing")
missing_cols = [col for col in sc.columns if col.endswith('missing')]
print(sc_complete_missing[['USUBJID', 'VISITNAM'] + missing_cols].sort_values('USUBJID'))


# Add exclusion flags
sc['exclude_complete_tremor'] = sc['tremor_items_missing'] == 11
sc['exclude_complete_non_tremor'] = sc['non_tremor_items_missing'] == 35
sc['exclude_complete_eleven'] = sc['eleven_item_missing'] == 11
sc['exclude_complete_partIII'] = sc['mds_partIII_missing'] == 33

sc['exclude_excessive_tremor'] = sc['tremor_items_missing'] > 2       # >20% of 11 items
sc['exclude_excessive_non_tremor'] = sc['non_tremor_items_missing'] > 7   # >20% of 35 items
sc['exclude_excessive_eleven'] = sc['eleven_item_missing'] > 2        # >20% of 11 items
sc['exclude_excessive_mds_III'] = sc['mds_partIII_missing'] > 7       # >20% of 33 items

sc['exclude_from_primary'] = (
    sc['exclude_complete_tremor'] |
    sc['exclude_complete_non_tremor'] |
    sc['exclude_complete_eleven'] |
    sc['exclude_excessive_tremor'] |
    sc['exclude_excessive_non_tremor'] |
    sc['exclude_excessive_eleven'] |
    sc['exclude_complete_partIII'] |
    sc['exclude_excessive_mds_III']
)

# Frequency of exclusion
print(sc['exclude_from_primary'].value_counts())

# Excluded primary rows
excluded_primary = sc[sc['exclude_from_primary'] == True].copy()


# Select columns: USUBJID, VISITNAM, any ending with 'missing', any starting with 'exclude_complete'
missing_cols = [col for col in sc.columns if col.endswith('missing')]
complete_cols = [col for col in sc.columns if col.startswith('exclude_complete')]
selected_cols = ['USUBJID', 'VISITNAM'] + missing_cols + complete_cols
excluded_primary = excluded_primary[selected_cols]

# Sort by USUBJID, VISITNAM
excluded_primary = excluded_primary.sort_values(['USUBJID', 'VISITNAM'])
print(excluded_primary.to_string())  # print all rows

# Summarise per subject: count visits and concatenate visit names
excluded_summary = (excluded_primary.groupby('USUBJID')
                    .agg(
                        visits_missing=('VISITNAM', 'count'),
                        visits=('VISITNAM', lambda x: '; '.join(x))
                    )
                    .reset_index())
print(excluded_summary.to_string())


# Subjects excluded: compute total_visits, excluded_visits, percentage
subjects_excluded = (sc.groupby('USUBJID')
                     .agg(
                         total_visits=('exclude_from_primary', 'size'),
                         excluded_visits=('exclude_from_primary', 'sum')
                     )
                     .reset_index())
subjects_excluded['pct_excluded'] = subjects_excluded['excluded_visits'] / subjects_excluded['total_visits'] * 100
subjects_excluded = subjects_excluded[subjects_excluded['excluded_visits'] > 0]
subjects_excluded = subjects_excluded.sort_values('pct_excluded', ascending=False)
print(subjects_excluded)


# Final datasets
# Dataset for primary analyses (exclude flagged rows)
sc_primary_analysis = sc[sc['exclude_from_primary'] == False].copy()
print("Unique subjects in sc_primary_analysis:", sc_primary_analysis['USUBJID'].nunique())  # 156

# For longitudinal analyses, ensure subjects have all 3 visits
# Count visits per subject
visit_counts = sc_primary_analysis.groupby('USUBJID').size()
subjects_with_3 = visit_counts[visit_counts == 3].index
sc_longitudinal = sc_primary_analysis[sc_primary_analysis['USUBJID'].isin(subjects_with_3)]

print("Subjects with all 3 visits:", sc_longitudinal['USUBJID'].nunique())  # 137


# Merge sc_longitudinal with groupe
check_df = pd.merge(sc_longitudinal[['USUBJID', 'VISITNUM', 'mds_partIII_total']], 
                    groupe[['USUBJID', 'RAN_GRP_LIB']], on='USUBJID', how='inner')
# Drop any rows with missing mds_partIII_total (if needed)
check_df = check_df.dropna(subset=['mds_partIII_total'])


# Group by VISITNUM and RAN_GRP_LIB
summary = check_df.groupby(['VISITNUM', 'RAN_GRP_LIB'])['mds_partIII_total'].agg(['mean', 'std']).reset_index()
print(summary)


# Select columns for wide format
wide_cols = ['USUBJID', 'VISITNAM', 'tremor_mod_scale', 'non_tremor_mod_scale', 'eleven_item_mod_scale', 'mds_partIII_total']
sc_wide_base = sc_longitudinal[wide_cols].copy()

# Pivot: index=USUBJID, columns=VISITNAM, values=the four scales
# This will create a MultiIndex columns. We'll flatten.
pivoted = sc_wide_base.pivot(index='USUBJID', columns='VISITNAM', 
                              values=['tremor_mod_scale', 'non_tremor_mod_scale', 'eleven_item_mod_scale', 'mds_partIII_total'])

# Flatten column names: e.g., ('tremor_mod_scale', 'Visit 2 - Baseline') -> 'tremor_mod_scale_Visit 2 - Baseline'
pivoted.columns = [f"{col[0]}_{col[1]}" for col in pivoted.columns]
pivoted = pivoted.reset_index()  # make USUBJID a column again

# Now pivoted is sc_wide
sc_wide = pivoted.copy()
print(sc_wide.columns.tolist())

temp = pd.merge(sc_wide, groupe[['USUBJID', 'RAN_GRP_LIB']], on='USUBJID', how='inner')
print(temp.columns.tolist())

#  Select and rename columns (wide format)
temp_long_pre = temp[[
    'USUBJID', 'RAN_GRP_LIB',
    'tremor_mod_scale_Visit 2 - Baseline',
    'tremor_mod_scale_Visit 6 - Follow-up',
    'tremor_mod_scale_Visit 8 - Follow-up',
    'non_tremor_mod_scale_Visit 2 - Baseline',
    'non_tremor_mod_scale_Visit 6 - Follow-up',
    'non_tremor_mod_scale_Visit 8 - Follow-up',
    'eleven_item_mod_scale_Visit 2 - Baseline',
    'eleven_item_mod_scale_Visit 6 - Follow-up',
    'eleven_item_mod_scale_Visit 8 - Follow-up',
    'mds_partIII_total_Visit 2 - Baseline',
    'mds_partIII_total_Visit 6 - Follow-up',
    'mds_partIII_total_Visit 8 - Follow-up'
]].copy()

# Rename columns to simpler names
temp_long_pre.columns = [
    'USUBJID', 'RAN_GRP_LIB',
    'tremor_visit2', 'tremor_visit6', 'tremor_visit8',
    'non_tremor_visit2', 'non_tremor_visit6', 'non_tremor_visit8',
    'eleven_visit2', 'eleven_visit6', 'eleven_visit8',
    'mdsIII_visit2', 'mdsIII_visit6', 'mdsIII_visit8'
]

# Melt to long format
temp_long = pd.melt(
    temp_long_pre,
    id_vars=['USUBJID', 'RAN_GRP_LIB'],
    var_name='variable',
    value_name='value'
)

# Extract scale and visit from variable names using string matching
conditions = [
    temp_long['variable'].str.contains('non_tremor'),
    temp_long['variable'].str.contains('tremor'),
    temp_long['variable'].str.contains('eleven'),
    temp_long['variable'].str.contains('mdsIII')
]
choices = ['Non-Tremor Scale', 'Tremor Scale', '11-Item Scale', 'MDS-UPDRS Part III']
temp_long['scale'] = np.select(conditions, choices, default='Other')

visit_conditions = [
    temp_long['variable'].str.contains('visit2'),
    temp_long['variable'].str.contains('visit6'),
    temp_long['variable'].str.contains('visit8')
]
visit_choices = ['Baseline', 'Month 6', 'Month 12']
temp_long['visit'] = np.select(visit_conditions, visit_choices, default='Unknown')

# Convert visit to categorical with specified order
temp_long['visit'] = pd.Categorical(
    temp_long['visit'],
    categories=['Baseline', 'Month 6', 'Month 12'],
    ordered=True
)

# Keep only necessary columns
temp_long = temp_long[['USUBJID', 'RAN_GRP_LIB', 'scale', 'visit', 'value']]

counts = pd.crosstab(temp_long['scale'], temp_long['visit'])
print(counts) # 137 everywhere, ok


