"""
Direct Python translation of the Bugphyzz physiologies.R script.

This module intentionally mirrors the original R helper structure rather than
attempting a Pythonic package redesign. It expects Bugphyzz package resource
files to be available under RESOURCE_DIR, or an explicit resource_dir argument
where supported.
"""

from __future__ import annotations

import re
import warnings
from pathlib import Path
from typing import Iterable, Mapping, Sequence

import numpy as np
import pandas as pd

try:
    # These are translated in bacdive.py.
    from .bacdive import _get_bacdive, _reshape_bacdive
except ImportError:  # pragma: no cover - useful during incremental porting
    _get_bacdive = None
    _reshape_bacdive = None


RESOURCE_DIR = Path(__file__).resolve().parent / "resources"

# These correspond to internal sysdata.rda objects in the R package.
# In the Python port, populate them from portable resource files.
ranks_parents: pd.DataFrame | None = None
bacdive_phys_names: list[str] | None = None


# -------------------------------------------------------------------------
# Public functions
# -------------------------------------------------------------------------

def physiologies(
    keyword: str | Sequence[str] = "all",
    full_source: bool = False,
    resource_dir: str | Path | None = None,
    ranks_parents_df: pd.DataFrame | None = None,
    bacdive_names: Sequence[str] | None = None,
) -> dict[str, pd.DataFrame]:
    """
    Direct Python translation of physiologies().

    Imports a dictionary of pandas DataFrames. This data is in a raw state
    before cleaning and imputation and is intended for developers/curators.
    """
    resource_dir = _resource_dir(resource_dir)
    keyword_list = _check_keyword(keyword, resource_dir, bacdive_names)

    spreadsheet_names = set(show_phys("spreadsheets", resource_dir, bacdive_names))
    bacdive_names_set = set(show_phys("bacdive", resource_dir, bacdive_names))

    cond1 = any(k in spreadsheet_names for k in keyword_list)
    cond2 = any(k in bacdive_names_set for k in keyword_list)

    physiology_tables: dict[str, pd.DataFrame] = {}

    if cond1 and cond2:
        spreadsheets = _import_spreadsheets(
            keyword_list,
            resource_dir=resource_dir,
            ranks_parents_df=ranks_parents_df,
        )
        spreadsheets = {k: v for k, v in spreadsheets.items() if k in keyword_list}

        if _get_bacdive is None or _reshape_bacdive is None:
            raise ImportError("bacdive.py must be available to import BacDive physiologies.")
        bacdive = _reshape_bacdive(_get_bacdive(verbose=False))
        bacdive = {k: v for k, v in bacdive.items() if k in keyword_list}

        for key in keyword_list:
            df1 = spreadsheets.get(key)
            df2 = bacdive.get(key)
            frames = [x for x in (df1, df2) if x is not None]
            if frames:
                physiology_tables[key] = pd.concat(frames, ignore_index=True, sort=False)
            else:
                physiology_tables[key] = pd.DataFrame()
            print(f"Finished {key}.")

    elif cond1 and not cond2:
        spreadsheets = _import_spreadsheets(
            keyword_list,
            resource_dir=resource_dir,
            ranks_parents_df=ranks_parents_df,
        )
        physiology_tables = {k: v for k, v in spreadsheets.items() if k in keyword_list}
        for key in keyword_list:
            print(f"Finished {key}.")

    elif not cond1 and cond2:
        if _get_bacdive is None or _reshape_bacdive is None:
            raise ImportError("bacdive.py must be available to import BacDive physiologies.")
        bacdive = _reshape_bacdive(_get_bacdive(verbose=False))
        physiology_tables = {k: v for k, v in bacdive.items() if k in keyword_list}
        for key in keyword_list:
            print(f"Finished {key}.")

    cleaned: dict[str, pd.DataFrame] = {}
    for key, df in physiology_tables.items():
        df = df.copy()
        df = _squish_character_columns(df)
        df = _add_source_info(df, resource_dir)
        df = _squish_lower_columns(df, ["Frequency", "Evidence", "Confidence_in_curation"])
        df = df.drop_duplicates().reset_index(drop=True)

        if full_source and "fullSource" in df.columns:
            df["Attribute_source"] = df["fullSource"]
        if "full_source" in df.columns:
            df = df.drop(columns=["full_source"])

        attr_group = _unique_scalar(df.get("Attribute_group"))
        attr_type = _unique_scalar(df.get("Attribute_type"))
        df = _reorder_columns(df, name=attr_group, attr_type=attr_type, resource_dir=resource_dir)
        df = _drop_all_na_columns(df)

        if _unique_scalar(df.get("Attribute_group")) == "aerophilicity":
            df = _homogenize_aerophilicity_attribute_names(df)

        cleaned[key] = df.drop_duplicates().reset_index(drop=True)

    return cleaned


def show_phys(
    which_names: str = "all",
    resource_dir: str | Path | None = None,
    bacdive_names: Sequence[str] | None = None,
) -> list[str]:
    """Direct Python translation of showPhys()."""
    resource_dir = _resource_dir(resource_dir)
    links = _read_tsv(resource_dir / "spreadsheet_links.tsv")
    spreadsheet_phys = links["physiology"].dropna().astype(str).tolist()

    if bacdive_names is None:
        bacdive_names = _load_bacdive_phys_names(resource_dir)

    if which_names == "all":
        return sorted(set(spreadsheet_phys).union(set(bacdive_names)))
    if which_names == "spreadsheets":
        return spreadsheet_phys
    if which_names == "bacdive":
        return list(bacdive_names)

    raise ValueError("which_names must be one of: 'all', 'spreadsheets', 'bacdive'.")


# R-style aliases
showPhys = show_phys


# -------------------------------------------------------------------------
# Helper functions for physiologies
# -------------------------------------------------------------------------

def _check_keyword(
    keyword: str | Sequence[str],
    resource_dir: str | Path | None = None,
    bacdive_names: Sequence[str] | None = None,
) -> list[str]:
    resource_dir = _resource_dir(resource_dir)
    if isinstance(keyword, str):
        keyword_list = [keyword]
    else:
        keyword_list = list(keyword)

    keyword_list = sorted(set(keyword_list))

    if "all" in keyword_list:
        if len(keyword_list) > 1:
            raise ValueError(
                "Found 'all' among the keywords. Are you sure that you want to "
                "import all of the physiologies? If so, use 'all' alone. Quitting."
            )
        print("All physiologies will be imported.")
        keyword_list = show_phys("all", resource_dir, bacdive_names)

    valid_keywords = set(show_phys("all", resource_dir, bacdive_names))
    invalid = [k for k in keyword_list if k not in valid_keywords]
    if invalid:
        raise ValueError(
            "Invalid keyword(s): "
            + ", ".join(invalid)
            + ". Check valid keywords with show_phys() or use 'all' to import all physiologies."
        )

    return keyword_list


def _import_spreadsheets(
    keyword: Sequence[str],
    resource_dir: str | Path | None = None,
    ranks_parents_df: pd.DataFrame | None = None,
) -> dict[str, pd.DataFrame]:
    """Direct Python translation of .importSpreadsheets()."""
    resource_dir = _resource_dir(resource_dir)
    parent_col_names = ["Parent_name", "Parent_NCBI_ID", "Parent_rank"]

    links = _read_tsv(resource_dir / "spreadsheet_links.tsv")
    links = links[links["physiology"].isin(keyword)].copy()

    out: dict[str, pd.DataFrame] = {}
    for _, row in links.iterrows():
        phys_name = row["physiology"]
        attr_type = row["attribute_type"]
        url = row["link"]

        df = pd.read_csv(url).drop_duplicates().reset_index(drop=True)
        df["Attribute_type"] = attr_type
        df["Attribute_group"] = phys_name
        if "NCBI_ID" in df.columns:
            df["NCBI_ID"] = df["NCBI_ID"].astype(str)
        if "Attribute_value" in df.columns:
            df = df[df["Attribute_value"].notna()].copy()

        unique_attr_type = _unique_scalar(df.get("Attribute_type"))
        if unique_attr_type == "numeric":
            df = _numeric_to_range(df)
        elif unique_attr_type == "range":
            df = _modify_range(df)
        elif unique_attr_type in _discrete_attribute_types(resource_dir):
            df = df[df["Attribute_value"].isin([True, False])].copy()

        if all(col in df.columns for col in parent_col_names):
            df["Parent_NCBI_ID"] = df["Parent_NCBI_ID"].astype(str).map(_squish)
        else:
            rp = _get_ranks_parents(resource_dir, ranks_parents_df)
            rp = rp.copy()
            for col in ["NCBI_ID", "Parent_NCBI_ID"]:
                if col in rp.columns:
                    rp[col] = rp[col].astype(str)
            df = df.merge(rp, on="NCBI_ID", how="left")

        out[str(phys_name)] = df

    return out


def _numeric_to_range(df: pd.DataFrame) -> pd.DataFrame:
    """Direct Python translation of .numericToRange()."""
    df = df.copy()
    df["Attribute_value_min"] = pd.to_numeric(df["Attribute_value"], errors="coerce")
    df["Attribute_value_max"] = pd.to_numeric(df["Attribute_value"], errors="coerce")
    df["Attribute_type"] = "range"
    df = df.drop(columns=["Attribute_value"])
    return df.drop_duplicates().reset_index(drop=True)


def _modify_range(df: pd.DataFrame) -> pd.DataFrame:
    """Direct Python translation of .modifyRange()."""
    df = df.copy()
    num = r"[0-9]+(\.[0-9]+)?"
    regex1 = rf"^\-?{num}(\-{num})?$"
    regex2 = rf"^(<|>)(\-)?{num}$"
    regex = rf"({regex1}|{regex2})"

    df["Attribute_value"] = df["Attribute_value"].astype(str)
    df = df[df["Attribute_value"].str.contains(regex, regex=True, na=False)].copy()

    df["Attribute_value"] = df["Attribute_value"].str.replace(
        rf"^(\-)([0-9]+(\.[0-9]+)?)", r"minus\2", regex=True
    )
    df["Attribute_value"] = df["Attribute_value"].str.replace(" ", "", regex=False)

    def first_case_when(value: str) -> str:
        if "<" in value:
            return f"-{value}"
        if ">" in value:
            return f"{value}-"
        if "-" not in value:
            return f"{value}-{value}"
        if value.startswith("-"):
            return f"minusInf{value}"
        if value.endswith("-"):
            return f"{value}Inf"
        return value

    df["Attribute_value"] = df["Attribute_value"].map(first_case_when)
    df["Attribute_value"] = df["Attribute_value"].str.replace(r"(<|>)", "", regex=True)

    def second_case_when(value: str) -> str:
        if value.startswith("-"):
            return f"minusInf{value}"
        if value.endswith("-"):
            return f"{value}Inf"
        return value

    df["Attribute_value"] = df["Attribute_value"].map(second_case_when)

    split_values = df["Attribute_value"].str.split("-", n=1, expand=True)
    df["Attribute_value_min"] = split_values[0]
    df["Attribute_value_max"] = split_values[1]

    df["Attribute_value_min"] = df["Attribute_value_min"].str.replace("minus", "-", regex=False)
    df["Attribute_value_max"] = df["Attribute_value_max"].str.replace("minus", "-", regex=False)

    df["Attribute_value_min"] = pd.to_numeric(df["Attribute_value_min"], errors="coerce")
    df["Attribute_value_max"] = pd.to_numeric(df["Attribute_value_max"], errors="coerce")

    df = df.drop(columns=["Attribute_value"])
    return df.drop_duplicates().reset_index(drop=True)


def _discrete_attribute_types(resource_dir: str | Path | None = None) -> list[str]:
    """Direct Python translation of .discreteAttributeTypes()."""
    resource_dir = _resource_dir(resource_dir)
    dat = _read_tsv(resource_dir / "spreadsheet_links.tsv")
    return dat.loc[dat["trait_type"] == "discrete", "attribute_type"].dropna().unique().tolist()


def _add_source_info(dat: pd.DataFrame, resource_dir: str | Path | None = None) -> pd.DataFrame:
    """Direct Python translation of .addSourceInfo()."""
    resource_dir = _resource_dir(resource_dir)
    source_data = _read_tsv(resource_dir / "attribute_sources.tsv", quotechar=None)
    return dat.merge(source_data, on="Attribute_source", how="left")


def _reorder_columns(
    df: pd.DataFrame,
    name: str | None = None,
    attr_type: str | None = None,
    resource_dir: str | Path | None = None,
) -> pd.DataFrame:
    """Direct Python translation of .reorderColumns()."""
    req_cols = _required_columns(attr_type, resource_dir)
    existing = [col for col in req_cols if col in df.columns]
    missing = [col for col in req_cols if col not in df.columns]

    if missing:
        if name is not None:
            msg = f"Missing columns in {name}. Missing columns are: {', '.join(missing)}"
        else:
            msg = f"Missing columns. Missing columns are: {', '.join(missing)}"
        warnings.warn(msg, stacklevel=2)

    remaining = [col for col in df.columns if col not in existing]
    return df.loc[:, existing + remaining]


def _homogenize_aerophilicity_attribute_names(df: pd.DataFrame) -> pd.DataFrame:
    """Direct Python translation of .homogenizeAerophilicityAttributeNames()."""
    df = df.copy()
    df["Attribute"] = df["Attribute"].replace(
        {
            "obligately anaerobic": "anaerobic",
            "microaerophilic": "aerobic",
            "obligately aerobic": "aerobic",
        }
    )
    return df


def _required_columns(attr_type: str | None, resource_dir: str | Path | None = None) -> list[str]:
    """Direct Python translation of .requiredColumns()."""
    resource_dir = _resource_dir(resource_dir)
    df = _read_tsv(resource_dir / "curation_template.tsv")
    if attr_type is None:
        return []
    mask = (df["requiredness"] == "required") & df["attribute_types"].astype(str).str.contains(
        str(attr_type), regex=True, na=False
    )
    df = df.loc[mask].copy()
    df = df.sort_values("required_column_order")
    return df["column_name"].dropna().astype(str).tolist()


def _template(dataset: pd.DataFrame, resource_dir: str | Path | None = None) -> pd.DataFrame:
    """Direct Python translation of .template()."""
    resource_dir = _resource_dir(resource_dir)
    template = _read_tsv(resource_dir / "curation_template.tsv")
    return template[template["column_name"].isin(dataset.columns)].copy()


def _attributes(resource_dir: str | Path | None = None) -> list[str]:
    """Direct Python translation of .attributes()."""
    resource_dir = _resource_dir(resource_dir)
    df = _read_tsv(resource_dir / "attributes.tsv")
    return df["attribute"].dropna().unique().tolist()


def _append_links(x: pd.DataFrame, resource_dir: str | Path | None = None) -> pd.DataFrame:
    """Direct Python translation of .appendLinks()."""
    resource_dir = _resource_dir(resource_dir)
    links = _read_tsv(resource_dir / "spreadsheet_links.tsv")
    select_cols = ["physiology", "source_link"]
    phys_links = links.loc[:, select_cols]
    custom_links = _custom_links(resource_dir).loc[:, select_cols]
    all_links = pd.concat([phys_links, custom_links], ignore_index=True, sort=False)
    return x.merge(all_links, left_on="dataset", right_on="physiology", how="left")


# -------------------------------------------------------------------------
# Extra helpers needed for standalone Python behavior
# -------------------------------------------------------------------------

def _resource_dir(resource_dir: str | Path | None = None) -> Path:
    return Path(resource_dir) if resource_dir is not None else RESOURCE_DIR


def _read_tsv(path: str | Path, quotechar: str | None = '"') -> pd.DataFrame:
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(
            f"Required Bugphyzz resource file not found: {path}. "
            "For the Python port, copy the package extdata files into RESOURCE_DIR "
            "or pass resource_dir=... ."
        )
    kwargs = {"sep": "\t", "dtype": object, "keep_default_na": True}
    if quotechar is None:
        kwargs["quoting"] = 3  # csv.QUOTE_NONE, avoid importing csv just for this
    return pd.read_csv(path, **kwargs)


def _load_bacdive_phys_names(resource_dir: Path) -> list[str]:
    global bacdive_phys_names
    if bacdive_phys_names is not None:
        return list(bacdive_phys_names)

    txt = resource_dir / "bacdive_phys_names.txt"
    tsv = resource_dir / "bacdive_phys_names.tsv"
    if txt.exists():
        return [line.strip() for line in txt.read_text().splitlines() if line.strip()]
    if tsv.exists():
        df = _read_tsv(tsv)
        col = "physiology" if "physiology" in df.columns else df.columns[0]
        return df[col].dropna().astype(str).tolist()

    raise FileNotFoundError(
        "BacDive physiology names are required. Provide bacdive_names=..., "
        "set physiologies.bacdive_phys_names, or create resources/bacdive_phys_names.txt."
    )


def _get_ranks_parents(
    resource_dir: Path,
    ranks_parents_df: pd.DataFrame | None = None,
) -> pd.DataFrame:
    global ranks_parents
    if ranks_parents_df is not None:
        return ranks_parents_df
    if ranks_parents is not None:
        return ranks_parents

    for fname in ["ranks_parents.tsv", "ranksParents.tsv"]:
        path = resource_dir / fname
        if path.exists():
            return _read_tsv(path)

    raise FileNotFoundError(
        "ranksParents is required for spreadsheets missing parent columns. "
        "Provide ranks_parents_df=..., set physiologies.ranks_parents, "
        "or create resources/ranks_parents.tsv."
    )


def _custom_links(resource_dir: str | Path | None = None) -> pd.DataFrame:
    """
    Placeholder for .customLinks(), which was referenced here but defined in
    another R script. If resources/custom_links.tsv exists, use it; otherwise
    return an empty compatible table.
    """
    resource_dir = _resource_dir(resource_dir)
    path = resource_dir / "custom_links.tsv"
    if path.exists():
        return _read_tsv(path)
    return pd.DataFrame(columns=["physiology", "source_link"])


def _squish(value):
    if pd.isna(value):
        return value
    return re.sub(r"\s+", " ", str(value)).strip()


def _squish_character_columns(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    for col in df.columns:
        if pd.api.types.is_object_dtype(df[col]) or pd.api.types.is_string_dtype(df[col]):
            df[col] = df[col].map(_squish)
    return df


def _squish_lower_columns(df: pd.DataFrame, columns: Sequence[str]) -> pd.DataFrame:
    df = df.copy()
    for col in columns:
        if col in df.columns:
            df[col] = df[col].map(lambda x: _squish(x).lower() if not pd.isna(x) else x)
    return df


def _drop_all_na_columns(df: pd.DataFrame) -> pd.DataFrame:
    return df.loc[:, ~df.isna().all(axis=0)].copy()


def _unique_scalar(series: pd.Series | None):
    if series is None:
        return None
    values = pd.Series(series).dropna().unique()
    if len(values) == 0:
        return None
    return values[0]


# R-style aliases for internal helpers, where valid Python identifiers differ.
checkKeyword = _check_keyword
importSpreadsheets = _import_spreadsheets
numericToRange = _numeric_to_range
modifyRange = _modify_range
discreteAttributeTypes = _discrete_attribute_types
addSourceInfo = _add_source_info
reorderColumns = _reorder_columns
homogenizeAerophilicityAttributeNames = _homogenize_aerophilicity_attribute_names
requiredColumns = _required_columns
appendLinks = _append_links
