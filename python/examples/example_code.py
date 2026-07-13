"""
bugphyzz Example Script

"""
# %%
from pathlib import Path

import pandas as pd
import bugphyzz as bp

# %%
# ------------------------------------------------------------
#  Import Bugphyzz
# ------------------------------------------------------------

bug = bp.import_bugphyzz()

print("Imported attributes:")
print(sorted(bug.keys()))

print()
print(f"Number of attributes: {len(bug)}")


# %%
# ------------------------------------------------------------
# Examine one attribute
# ------------------------------------------------------------

print("\nAerophilicity:")
print(bug["aerophilicity"].head())

print()
print(bug["aerophilicity"].info())

# %%
# ------------------------------------------------------------
# Create signatures
# ------------------------------------------------------------

aero_sigs = bp.make_signatures(
    bug["aerophilicity"]
)

print("\nSignature names:")
print(list(aero_sigs.keys()))

first_name = next(iter(aero_sigs))

print("\nFirst signature:")
print(first_name)
print(aero_sigs[first_name])

# %%
# ------------------------------------------------------------
# Numeric signatures
# ------------------------------------------------------------

temp_sigs = bp.make_signatures(
    bug["growth temperature"]
)

first_numeric = next(iter(temp_sigs))
print("\nTemperature signatures:")
print(list(temp_sigs.keys()))

print(temp_sigs[first_numeric])
print(len(first_numeric))


# %%
# ------------------------------------------------------------
# Restrict taxonomic rank
# ------------------------------------------------------------

genus_sigs = bp.make_signatures(
    bug["aerophilicity"],
    tax_level="genus"
)

print("\nGenus-only signatures:")
print(list(genus_sigs.keys()))



# %%
# ------------------------------------------------------------
# Restrict evidence
# ------------------------------------------------------------

exp_sigs = bp.make_signatures(
    bug["aerophilicity"],
    evidence="exp"
)

print("\nExperimental signatures:")
print(list(exp_sigs.keys()))

# %%
# ------------------------------------------------------------
# Restrict frequency
# ------------------------------------------------------------

always_sigs = bp.make_signatures(
    bug["aerophilicity"],
    frequency="always"
)

print("\nAlways-present signatures:")
print(list(always_sigs.keys()))

# %%
# ------------------------------------------------------------
# Numeric filtering
# ------------------------------------------------------------

ph_sigs = bp.make_signatures(
    bug["optimal ph"],
    min_value=6,
    max_value=8
)

print("\npH signatures:")
print(list(ph_sigs.keys()))

# %%
# ------------------------------------------------------------
# Taxon signatures
# ------------------------------------------------------------

ecoli = bp.get_taxon_signatures(
    "562",
    bug
)

print("\nSignatures containing taxid 562:")
print(ecoli)

# %%
# ------------------------------------------------------------
# Taxon name lookup
# ------------------------------------------------------------

ecoli = bp.get_taxon_signatures(
    "Escherichia coli",
    bug,
    taxid_type="Taxon_name"
)

print("\nSignatures containing Escherichia coli:")
print(ecoli)

# %%
# ------------------------------------------------------------
# Create signatures for every physiology
# ------------------------------------------------------------

all_sigs = {}

#shape = bp.physiologies("shape")["shape"]
#print(shape["Attribute"].value_counts())

for phys_name, df in bug.items():
    sigs = bp.make_signatures(df)
    all_sigs.update(sigs)

print(f"\nTotal signatures: {len(all_sigs)}")

# %%
# ------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------

sig_sizes = pd.Series(
    {k: len(v) for k, v in all_sigs.items()}
)

print("\nSignature size summary:")
print(sig_sizes.describe())

print("\nLargest signature:")
print(sig_sizes.max())

print("\nSmallest signature:")
print(sig_sizes.min())

# %%
# ------------------------------------------------------------
# Largest signature
# ------------------------------------------------------------

largest_sig = sig_sizes.idxmax()

print("\nLargest signature:")
print(largest_sig)

print("\nFirst 20 taxa:")
print(all_sigs[largest_sig][:20])

# %%
# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

Path("output").mkdir(exist_ok=True)

summary = pd.DataFrame(
    {
        "signature": sig_sizes.index,
        "size": sig_sizes.values
    }
)

summary.to_csv(
    "output/signature_summary.csv",
    index=False
)

for phys_name, df in bug.items():
    df.to_csv(
        f"output/{phys_name}.csv",
        index=False
    )

print("\nDone.")