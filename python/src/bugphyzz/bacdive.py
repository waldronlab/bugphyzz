"""Direct Python rewrite of Bugphyzz BacDive import/reshape helpers.

This module mirrors the original R helper script as closely as practical before
larger package reorganization. R data.frames are represented as pandas
DataFrames, and R named lists are represented as ``dict[str, DataFrame]``.

Original R functions mapped here:
    .getBacDive          -> _get_bacdive / get_bacdive
    .importBacDiveExcel  -> _import_bacdive_excel
    .changeBDColNames    -> _change_bd_col_names
    .getTidyBD           -> _get_tidy_bd
    .reshapeBacDive      -> _reshape_bacdive / reshape_bacdive
    .catToLog            -> _cat_to_log

"""

from __future__ import annotations

import re
import warnings
from typing import Dict, Iterable

import pandas as pd


BACDIVE_CSV_URL = (
    "https://docs.google.com/spreadsheets/d/"
    "1mq3Crfis_CoJimLVrIoDFGf592bvMAqo/export?format=csv"
)


def _get_bacdive(verbose: bool = False) -> pd.DataFrame:
    """Main function for importing BacDive.

    Direct rewrite of R ``.getBacDive()``.
    """
    bacdive_data = _import_bacdive_excel(verbose=verbose)
    bacdive_data.columns = _change_bd_col_names(bacdive_data.columns)
    return _get_tidy_bd(bacdive_data)


def get_bacdive(verbose: bool = False) -> pd.DataFrame:
    """Public/Pythonic alias for ``_get_bacdive``."""
    return _get_bacdive(verbose=verbose)


def _import_bacdive_excel(verbose: bool = False) -> pd.DataFrame:
    """Import the BacDive Google Sheet CSV.

    Direct rewrite of R ``.importBacDiveExcel()``.
    """
    if verbose:
        print("Importing BacDive...")

    bacdive = pd.read_csv(BACDIVE_CSV_URL)
    bacdive.columns = [str(col).lower() for col in bacdive.columns]
    return bacdive


def _change_bd_col_names(columns: Iterable[str]) -> list[str]:
    """Rename BacDive columns to Bugphyzz-style column names.

    Direct rewrite of R ``.changeBDColNames()``.
    """
    mapping = {
        "bacdive_id": "BacDive_ID",
        "taxon_name": "Taxon_name",
        "ncbi_id": "NCBI_ID",
        "rank": "Rank",
        "parent_taxon_name": "Parent_name",
        "parent_ncbi_id": "Parent_NCBI_ID",
        "parent_rank": "Parent_rank",
        "sequence_16S_ncbi_id": "Seq16S_NCBI_ID",
        "sequence_genome_ncbi_id": "Genome_ID",
        "type_strain": "Type_strain",
    }
    return [mapping.get(str(col), str(col)) for col in columns]


def _get_tidy_bd(bacdive_data: pd.DataFrame) -> pd.DataFrame:
    """Convert wide BacDive data to tidy long format.

    Direct rewrite of R ``.getTidyBD()``. Attributes begin at the ``gram_stain``
    column and continue through the last column.
    """
    if "gram_stain" not in bacdive_data.columns:
        raise KeyError("Expected a 'gram_stain' column; attributes start there in the R script.")

    cols = list(bacdive_data.columns)
    first_attr_idx = cols.index("gram_stain")
    id_cols = cols[:first_attr_idx]
    value_cols = cols[first_attr_idx:]

    tidy = bacdive_data.melt(
        id_vars=id_cols,
        value_vars=value_cols,
        var_name="Attribute",
        value_name="Attribute_value",
    )

    tidy = tidy.loc[~tidy["Attribute_value"].isna()].copy()
    tidy = tidy.loc[tidy["Attribute_value"].astype(str) != ""].copy()
    tidy["Attribute"] = tidy["Attribute"].astype(str).str.replace("_", " ", regex=False)

    tidy["Attribute"] = tidy["Attribute"].replace(
        {
            "oxygen tolerance": "aerophilicity",
            "cell shape": "shape",
            "pathogenicity animal": "animal pathongen",  # Preserve R typo.
            "sample type": "isolation site",
        }
    )

    return tidy.drop_duplicates().reset_index(drop=True)


def _reshape_bacdive(df: pd.DataFrame) -> Dict[str, pd.DataFrame]:
    """Return a dictionary of tidy BacDive DataFrames, one per attribute.

    Direct rewrite of R ``.reshapeBacDive()``.
    """
    df = df.copy()
    df["Attribute_source"] = "BacDive"

    split_df: Dict[str, pd.DataFrame] = {
        str(attr): group.copy().reset_index(drop=True)
        for attr, group in df.groupby("Attribute", dropna=False)
        if not pd.isna(attr)
    }

    attr_names = [
        "aerophilicity",
        "shape",
        "country",
        "cultivation medium used",
        "geographic location",
        "isolation site",
    ]

    for attr_name in attr_names:
        if attr_name not in split_df:
            warnings.warn(f"Expected BacDive attribute {attr_name!r} was not present.", stacklevel=2)
            continue
        split_df[attr_name] = _cat_to_log(split_df[attr_name])
        if attr_name in {"aerophilicity", "shape"}:
            split_df[attr_name]["Attribute_type"] = "multistate-intersection"
        else:
            split_df[attr_name]["Attribute_type"] = "multistate-union"

    # aerophilicity ---------------------------------------------------------
    if "aerophilicity" in split_df:
        aer = split_df["aerophilicity"].copy()
        aer["Attribute"] = aer["Attribute"].replace(
            {
                "aerobe": "aerobic",
                "anaerobe": "anaerobic",
                "facultative anaerobe": "facultatively anaerobic",
                "microaerophile": "microaerophilic",
                "obligate anaerobe": "obligately anaerobic",
                "obligate aerobe": "obligately aerobic",
            }
        )
        split_df["aerophilicity"] = aer

    # animal pathogen -------------------------------------------------------
    if "animal pathongen" in split_df:
        split_df["animal pathogen"] = split_df.pop("animal pathongen")
    if "animal pathogen" in split_df:
        x = split_df["animal pathogen"].copy()
        vals = x["Attribute_value"].replace({"yes, in single cases": "yes"})
        x["Attribute_value"] = vals.map({"yes": True, "no": False})
        x["Attribute_group"] = "animal pathogen"
        x["Attribute"] = "animal pathogen"
        x["Attribute_type"] = "binary"
        split_df["animal pathogen"] = x

    # biosafety level -------------------------------------------------------
    if "biosafety level" in split_df and "biosafety level comment" in split_df:
        y = split_df["biosafety level comment"][["BacDive_ID", "Attribute_value"]].copy()
        y = y.rename(columns={"Attribute_value": "Note"})
        x = split_df["biosafety level"].merge(y, how="left", on="BacDive_ID")
        x["Attribute_value"] = "biosafety level " + x["Attribute_value"].astype(str)
        x["Attribute"] = x["Attribute_value"]
        x["Attribute_value"] = True
        x["Attribute_group"] = "biosafety level"
        x["Attribute_type"] = "multistate-intersection"
        split_df["biosafety level"] = x
        split_df.pop("biosafety level comment", None)

    # colony color: remove --------------------------------------------------
    split_df.pop("colony color", None)

    # cultivation medium used -> growth medium -----------------------------
    if "cultivation medium used" in split_df:
        split_df["growth medium"] = split_df.pop("cultivation medium used")
        split_df["growth medium"]["Attribute_group"] = "growth medium"

    # growth temperature ----------------------------------------------------
    split_df.pop("culture temperature range", None)
    split_df.pop("culture temperature type", None)
    if "culture temperature" in split_df and "culture temperature growth" in split_df:
        a = split_df["culture temperature"].copy()
        b = split_df["culture temperature growth"][["BacDive_ID", "Attribute_value"]].copy()
        b = b.rename(columns={"Attribute_value": "growth"})
        ab = a.merge(b, how="left", on="BacDive_ID")
        ab = ab.loc[ab["growth"] == "positive"].copy()
        ab = ab.drop(columns=["growth"])
        ab["Attribute_group"] = "growth temperature"
        ab["Attribute_type"] = "range"
        ab["Attribute"] = "growth temperature"
        split_df["growth temperature"] = ab
        split_df.pop("culture temperature", None)
        split_df.pop("culture temperature growth", None)

    # gram stain ------------------------------------------------------------
    if "gram stain" in split_df:
        gs = split_df["gram stain"].copy()
        gs["Attribute"] = gs["Attribute"].astype(str) + " " + gs["Attribute_value"].astype(str)
        gs["Attribute_value"] = True
        gs["Attribute_group"] = "gram stain"
        gs["Attribute_type"] = "multistate-intersection"
        split_df["gram stain"] = gs

    # halophily -------------------------------------------------------------
    if "halophily" in split_df:
        valid_terms = [
            r"NaCl",
            r"KCl",
            r"MgCl2",
            r"MgCl2x6H2O",
            r"Na\+",
            r"MgSO4x7H2O",
            r"Na2SO4",
            r"Sea salts",
            r"Chromium \(Cr6\+\)",
        ]
        regex = "(" + "|".join(valid_terms) + ")"
        hal = _split_explode(split_df["halophily"], "Attribute_value", ";")
        hal = hal.loc[~hal["Attribute_value"].astype(str).str.contains("no growth", na=False)].copy()
        hal["Attribute_value"] = hal["Attribute_value"].map(_str_squish)
        hal["Attribute_value"] = hal["Attribute_value"].str.replace("NaCL", "NaCl", regex=False)
        hal["Attribute_value"] = hal["Attribute_value"].str.replace("Marine", "Sea", regex=False)
        hal["Attribute_value"] = hal["Attribute_value"].str.replace("Salts", "salts", regex=False)
        hal = hal.loc[hal["Attribute_value"].str.contains(regex, regex=True, na=False)].copy()
        numeric_expr = r" [<>]??[0-9]+\.??[0-9]*.*"
        extracted = hal["Attribute_value"].str.extract(f"({numeric_expr})", expand=False).map(_str_squish)
        hal["Attribute"] = hal["Attribute_value"].str.extract(regex, expand=False)
        hal["Unit"] = extracted.str.replace(r"^.* ", "", regex=True)
        hal["Attribute_value"] = extracted.str.replace(r" .*$", "", regex=True)
        hal["Attribute_group"] = "halophily"
        hal["Attribute_type"] = "range"
        hal = hal.loc[~hal["Unit"].astype(str).str.contains(r"[0-9]", regex=True, na=False)].copy()
        split_df["halophily"] = hal.drop_duplicates().reset_index(drop=True)

    # hemolysis -------------------------------------------------------------
    if "hemolysis" in split_df:
        hem = _split_explode(split_df["hemolysis"], "Attribute_value", r";|/", regex=True)
        hem["Attribute_value"] = hem["Attribute_value"].map(_str_squish)
        hem = hem.loc[hem["Attribute_value"] != ""].copy()
        hem["Attribute"] = hem["Attribute_value"]
        hem["Attribute_value"] = True
        hem["Attribute_group"] = "hemolysis"
        hem["Attribute_type"] = "multistate-intersection"
        split_df["hemolysis"] = hem

    # incubation period: remove -------------------------------------------
    split_df.pop("incubation period", None)

    # motility --------------------------------------------------------------
    if "motility" in split_df:
        mot = split_df["motility"].copy()
        mot["Attribute_value"] = mot["Attribute_value"].map({"yes": True, "no": False})
        mot["Attribute_group"] = "motility"
        mot["Attribute_type"] = "binary"
        split_df["motility"] = mot

    # pathogenicity human ---------------------------------------------------
    if "pathogenicity human" in split_df:
        pat = split_df["pathogenicity human"].copy()
        pat["Note"] = pat["Attribute_value"].astype(str).str.extract(r"(in single cases)", expand=False).fillna("")
        pat["Attribute_value"] = pat["Attribute_value"].astype(str).str.contains(r"^yes", regex=True, na=False)
        # R sets non-yes values to NA then filters them out.
        pat = pat.loc[pat["Attribute_value"]].copy()
        pat["Attribute_group"] = "pathogenicity human"
        pat["Attribute_type"] = "binary"
        split_df["pathogenicity human"] = pat

    # metabolite production -------------------------------------------------
    if "metabolite production" in split_df:
        mp = _split_explode(split_df["metabolite production"], "Attribute_value", ";")
        yn = mp["Attribute_value"].astype(str).str.extract(r"(yes|no)$", expand=False)
        mp = mp.loc[~yn.isna()].copy()
        yn = mp["Attribute_value"].astype(str).str.extract(r"(yes|no)$", expand=False)
        mp["Attribute"] = mp["Attribute_value"]
        mp["Attribute_value"] = yn == "yes"
        mp["Attribute"] = mp["Attribute"].astype(str).str.replace(r" (yes|no)$", "", regex=True)
        mp["Attribute_group"] = "metabolite utilization"
        mp["Attribute_type"] = "multistate-intersection"
        split_df["metabolite production"] = mp

    # metabolite utilization ------------------------------------------------
    if "metabolite utiilization" in split_df:
        split_df["metabolite utilization"] = split_df.pop("metabolite utiilization")
    if "metabolite utilization" in split_df:
        mu = _split_explode(split_df["metabolite utilization"], "Attribute_value", ";")
        mu["Attribute_value"] = mu["Attribute_value"].map(_str_squish)
        x = mu["Attribute_value"].astype(str).str.replace(
            r"^.* (\+|-|\+/-) *.*$", r"\1", regex=True
        )
        y = x.where(x.isin(["+", "-", "+/-"]))
        mu = mu.loc[~y.isna()].copy()
        y = y.loc[~y.isna()]
        mu["Attribute"] = mu["Attribute_value"].astype(str).str.replace(
            r" (\+|-|\+/-) *.*$", "", regex=True
        )
        mu["Note"] = mu["Attribute_value"].astype(str).str.replace(
            r"^.*(\+|-|\+/-) ", "", regex=True
        )
        mu["Note"] = "kind of utilization tested: " + mu["Note"].astype(str)
        mu["Attribute_value"] = y.map({"+": "TRUE", "-": "FALSE", "+/-": "TRUE/FALSE"}).values
        mu = _split_explode(mu, "Attribute_value", "/")
        mu["Attribute_value"] = mu["Attribute_value"].map(_as_logical)
        mu["Attribute_group"] = "metabolite utilization"
        mu["Attribute_type"] = "multistate-intersection"
        split_df["metabolite utilization"] = mu

    # spore formation -------------------------------------------------------
    if "spore formation" in split_df:
        sf = split_df["spore formation"].copy()
        sf["Attribute_value"] = sf["Attribute_value"].map({"yes": True, "no": False})
        sf["Attribute_group"] = "spore formation"
        sf["Attribute_type"] = "binary"
        sf = sf.loc[~sf["Attribute_value"].isna()].copy()
        split_df["spore formation"] = sf

    # Final per-attribute cleanup ------------------------------------------
    cleaned: Dict[str, pd.DataFrame] = {}
    for name, x in split_df.items():
        if x is None:
            continue
        x = pd.DataFrame(x).copy()

        if "NCBI_ID" in x.columns:
            x["NCBI_ID"] = x["NCBI_ID"].astype("string")
        if "Parent_NCBI_ID" in x.columns:
            x["Parent_NCBI_ID"] = x["Parent_NCBI_ID"].astype("string")

        x["Frequency"] = "always"
        if "Attribute_value" in x.columns:
            x = x.loc[~x["Attribute_value"].isna()].copy()

        if "Attribute_type" in x.columns and not x.empty:
            unique_types = [val for val in x["Attribute_type"].dropna().unique()]
            if len(unique_types) == 1 and unique_types[0] == "numeric":
                x = _numeric_to_range(x)
            elif len(unique_types) == 1 and unique_types[0] == "range":
                x = _modify_range(x)

        x = x.dropna(axis=1, how="all")
        x = x.drop_duplicates().reset_index(drop=True)
        cleaned[name] = x

    return cleaned


def reshape_bacdive(df: pd.DataFrame) -> Dict[str, pd.DataFrame]:
    """Public/Pythonic alias for ``_reshape_bacdive``."""
    return _reshape_bacdive(df)


def _cat_to_log(df: pd.DataFrame) -> pd.DataFrame:
    """Convert categorical BacDive attribute rows to logical TRUE rows.

    Direct rewrite of R ``.catToLog()``.
    """
    df = df.copy()
    df["Attribute_group"] = df["Attribute"]
    df["Attribute"] = df["Attribute_value"]
    df["Attribute_value"] = True
    df["Attribute_type"] = "discrete"
    return df


# -------------------------------------------------------------------------
# Helpers used to express tidyverse operations in pandas
# -------------------------------------------------------------------------


def _split_explode(df: pd.DataFrame, column: str, sep: str, regex: bool = False) -> pd.DataFrame:
    out = df.copy()
    if regex:
        out[column] = out[column].astype(str).str.split(sep, regex=True)
    else:
        out[column] = out[column].astype(str).str.split(sep)
    return out.explode(column).reset_index(drop=True)


def _str_squish(value: object) -> str:
    if pd.isna(value):
        return ""
    return re.sub(r"\s+", " ", str(value)).strip()


def _as_logical(value: object) -> bool | pd.NA: # type: ignore
    if pd.isna(value):
        return pd.NA
    value_str = str(value).upper()
    if value_str == "TRUE":
        return True
    if value_str == "FALSE":
        return False
    return pd.NA


def _numeric_to_range(df: pd.DataFrame) -> pd.DataFrame:
    """
    Direct Python translation of .numericToRange().

    R behavior:
    - Converts Attribute_value to both Attribute_value_min and
      Attribute_value_max.
    - Changes Attribute_type to 'range'.
    - Removes the original Attribute_value column.
    - Keeps distinct rows.
    """
    df = df.copy()

    df["Attribute_value_min"] = pd.to_numeric(
        df["Attribute_value"], errors="coerce"
    )
    df["Attribute_value_max"] = pd.to_numeric(
        df["Attribute_value"], errors="coerce"
    )
    df["Attribute_type"] = "range"

    if "Attribute_value" in df.columns:
        df = df.drop(columns=["Attribute_value"])

    return df.drop_duplicates().reset_index(drop=True)


def _modify_range(df: pd.DataFrame) -> pd.DataFrame:
    """
    Direct Python translation of .modifyRange().

    Converts range-like strings in Attribute_value into numeric
    Attribute_value_min and Attribute_value_max columns.

    Handles values like:
        5
        5-10
        <5
        >5
        -5
        -5--1
    """
    df = df.copy()

    num = r"[0-9]+(\.[0-9]+)?"
    regex1 = rf"^\-?{num}(\-{num})?$"
    regex2 = rf"^(<|>)(\-)?{num}$"
    regex = rf"({regex1}|{regex2})"

    df["Attribute_value"] = df["Attribute_value"].astype(str).str.strip()
    df = df[df["Attribute_value"].str.contains(regex, regex=True, na=False)].copy()

    # Temporarily protect negative signs so splitting on "-" works like R.
    df["Attribute_value"] = df["Attribute_value"].str.replace(
        rf"^(\-)([0-9]+(\.[0-9]+)?)",
        r"minus\2",
        regex=True,
    )

    df["Attribute_value"] = df["Attribute_value"].str.replace(" ", "", regex=False)

    def normalize_range(value: str) -> str:
        if "<" in value:
            value = f"-{value}"
        elif ">" in value:
            value = f"{value}-"
        elif "-" not in value:
            value = f"{value}-{value}"
        elif value.startswith("-"):
            value = f"minusInf{value}"
        elif value.endswith("-"):
            value = f"{value}Inf"

        value = re.sub(r"(<|>)", "", value)

        if value.startswith("-"):
            value = f"minusInf{value}"
        elif value.endswith("-"):
            value = f"{value}Inf"

        return value

    df["Attribute_value"] = df["Attribute_value"].map(normalize_range)

    split_values = df["Attribute_value"].str.split("-", n=1, expand=True)
    df["Attribute_value_min"] = split_values[0]
    df["Attribute_value_max"] = split_values[1]

    df["Attribute_value_min"] = df["Attribute_value_min"].str.replace(
        "minus", "-", regex=False
    )
    df["Attribute_value_max"] = df["Attribute_value_max"].str.replace(
        "minus", "-", regex=False
    )

    df["Attribute_value_min"] = pd.to_numeric(
        df["Attribute_value_min"], errors="coerce"
    )
    df["Attribute_value_max"] = pd.to_numeric(
        df["Attribute_value_max"], errors="coerce"
    )

    df = df.drop(columns=["Attribute_value"])

    return df.drop_duplicates().reset_index(drop=True)


# R-style aliases for side-by-side conversion work. These are intentionally
# not PEP8-compliant, but they make it easier to compare to the source script.
getBacDive = get_bacdive
reshapeBacDive = reshape_bacdive
