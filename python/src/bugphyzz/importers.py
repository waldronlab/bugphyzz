from __future__ import annotations

import re
import tempfile
import warnings
import zipfile
from pathlib import Path
from typing import Any, Iterable

import pandas as pd
import requests
import shutil
from platformdirs import user_cache_dir


DEFAULT_VERSION = "10.5281/zenodo.12574596"
VALID_TAX_ID_TYPES = {"NCBI_ID", "Taxon_name"}
VALID_TAX_LEVELS = {
    "mixed",
    "superkingdom",
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species",
    "strain",
}
MIXED_TAX_LEVELS = [
    "superkingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species",
    "strain",
]
VALID_EVIDENCE = {"exp", "igc", "tas", "nas", "tax", "asr"}
VALID_FREQUENCY = {"always", "usually", "sometimes", "rarely", "unknown"}
DISCRETE_ATTRIBUTE_TYPES = {"multistate-intersection", "binary", "multistate-union"}
NUMERIC_ATTRIBUTE_TYPES = {"range", "numeric"}


# ---------------------------------------------------------------------------
# Public functions
# ---------------------------------------------------------------------------


def import_bugphyzz(
    version: str = DEFAULT_VERSION,
    force_download: bool = False,
    v: float = 0.8,
    exclude_rarely: bool = True,
    resource_dir: str | Path | None = None,
) -> dict[str, pd.DataFrame]:
    """Import Bugphyzz annotations as a dictionary of tidy DataFrames.

    Parameters
    ----------
    version:
        Zenodo DOI, GitHub commit hash, or ``"devel"``.
    force_download:
        If True, re-download remote files instead of using the local cache.
    v:
        Minimum validation value for ASR annotations. Must be between 0 and 1.
    exclude_rarely:
        If True, remove rows with ``Frequency == "rarely"``.
    resource_dir:
        Directory containing package resource TSVs such as
        ``validation_summary.tsv`` and ``thresholds.tsv``. If omitted, this
        function looks in ``resources/`` beside this file.

    Returns
    -------
    dict[str, pandas.DataFrame]
        Dictionary keyed by attribute name.
    """
    if not 0 <= v <= 1:
        raise ValueError("v must be between 0 and 1.")

    # R: output <- .downloadResource(version, forceDownload)
    frames = _download_resource(version=version, force_download=force_download)

    # R: split each binary/multistate/numeric table by Attribute, then flatten.
    output: dict[str, pd.DataFrame] = {}
    for frame in frames:
        for attr, group in frame.groupby("Attribute", dropna=False):
            if pd.isna(attr):
                continue
            output[str(attr)] = group.copy()

    val = _validation_data(resource_dir=resource_dir)
    val = val.loc[val["rank"] == "all", ["physiology", "attribute", "value"]].copy()
    val["physiology"] = val["physiology"].astype(str).str.lower()
    val["attribute"] = val["attribute"].astype(str).str.lower()

    filtered_output: dict[str, pd.DataFrame] = {}
    for attr_name, dat in output.items():
        dat = dat.copy()
        attr_type = _single_unique(dat["Attribute_type"], "Attribute_type")

        if attr_type == "binary":
            val_join = val[["attribute", "value"]].rename(columns={"attribute": "Attribute"})
            out = dat.merge(val_join, how="left", on="Attribute")

        elif attr_type in {"multistate-intersection", "multistate-union"}:
            val_join = val[["physiology", "attribute", "value"]].rename(
                columns={"physiology": "Attribute", "attribute": "Attribute_value"}
            )
            dat["Attribute_value"] = dat["Attribute_value"].astype(str).str.lower()
            out = dat.merge(val_join, how="left", on=["Attribute", "Attribute_value"])

        elif attr_type == "numeric":
            val_join = val[["attribute", "value"]].rename(columns={"attribute": "Attribute"})
            out = dat.merge(val_join, how="left", on="Attribute")
            if "nsti" in out.columns:
                out = out.rename(columns={"nsti": "NSTI"})

        else:
            raise ValueError(f"Unsupported Attribute_type: {attr_type!r}")

        # R: filter(!(value < v & Evidence == "asr"))
        is_asr = out["Evidence"].eq("asr")

        valid_asr = (
            out["value"].notna()
            & out["value"].ge(v)
        )

        out = out.loc[
            ~is_asr | valid_asr
        ].copy()

        out["value"] = out["value"].where(is_asr, pd.NA)

        out = out.rename(
            columns={"value": "Validation"}
        )

        if exclude_rarely:
            out = out.loc[out["Frequency"] != "rarely"].copy()

        filtered_output[attr_name] = out

    return filtered_output


def make_signatures(
    dat: pd.DataFrame,
    tax_id_type: str = "NCBI_ID",
    tax_level: str | Iterable[str] = "mixed",
    evidence: Iterable[str] = ("exp", "igc", "tas", "nas", "tax", "asr"),
    frequency: Iterable[str] = ("always", "usually", "sometimes", "unknown"),
    min_size: int = 10,
    min_value: float | None = None,
    max_value: float | None = None,
    resource_dir: str | Path | None = None,
) -> dict[str, list[str]] | None:
    """Create signatures from one Bugphyzz annotation DataFrame."""
    _validate_choice(tax_id_type, VALID_TAX_ID_TYPES, "tax_id_type")
    tax_levels = _as_list(tax_level)
    evidence_values = _as_list(evidence)
    frequency_values = _as_list(frequency)

    _validate_choices(tax_levels, VALID_TAX_LEVELS, "tax_level")
    _validate_choices(evidence_values, VALID_EVIDENCE, "evidence")
    _validate_choices(frequency_values, VALID_FREQUENCY, "frequency")

    attr_type = _single_unique(dat["Attribute_type"], "Attribute_type")

    if "mixed" in tax_levels:
        tax_levels = MIXED_TAX_LEVELS

    dat = dat.loc[
        dat["Rank"].isin(tax_levels)
        & dat["Evidence"].isin(evidence_values)
        & dat["Frequency"].isin(frequency_values)
    ].copy()

    if dat.empty:
        warnings.warn(
            "Not enough data for creating signatures. Try different filtering options",
            stacklevel=2,
        )
        return None

    if attr_type in DISCRETE_ATTRIBUTE_TYPES:
        signatures = _make_signatures_discrete(dat=dat, tax_id_type=tax_id_type)
    elif attr_type in NUMERIC_ATTRIBUTE_TYPES:
        signatures = _make_signatures_numeric(
            dat=dat,
            tax_id_type=tax_id_type,
            min_value=min_value,
            max_value=max_value,
            resource_dir=resource_dir,
        )
    else:
        raise ValueError(f"Unsupported Attribute_type: {attr_type!r}")

    output = {name: values for name, values in signatures.items() if len(values) >= min_size}
    if not output:
        warnings.warn(
            "Not enough data for creating signatures. Try different filtering options",
            stacklevel=2,
        )
    return output


def get_taxon_signatures(
    tax: str,
    bp: dict[str, pd.DataFrame],
    **make_signature_kwargs: Any,
) -> list[str]:
    """Return names of all signatures associated with a taxon."""
    signatures: dict[str, list[str]] = {}
    for dat in bp.values():
        sigs = make_signatures(dat, **make_signature_kwargs)
        if sigs:
            signatures.update(sigs)

    return [name for name, taxa in signatures.items() if tax in taxa]


# ---------------------------------------------------------------------------
# Non-exported/direct-rewrite helpers
# ---------------------------------------------------------------------------


def _make_signatures_discrete(dat: pd.DataFrame, tax_id_type: str = "NCBI_ID") -> dict[str, list[str]]:
    dat = dat.copy()
    # Mirrors the R script exactly, including the original "bugphyz" spelling.
    dat["Attribute"] = "bugphyz:" + dat["Attribute"].astype(str) + "|" + dat["Attribute_value"].astype(str)
    return _split_unique(dat, group_col="Attribute", value_col=tax_id_type)


def _make_signatures_numeric(
    dat: pd.DataFrame,
    tax_id_type: str = "NCBI_ID",
    min_value: float | None = None,
    max_value: float | None = None,
    resource_dir: str | Path | None = None,
) -> dict[str, list[str]]:
    dat = dat.copy()
    dat["Attribute_value"] = pd.to_numeric(dat["Attribute_value"], errors="coerce")

    if min_value is not None or max_value is not None:
        if min_value is None:
            min_value = float(dat["Attribute_value"].min())
            print(f"Minimum unspecified. Using {min_value}.")
        if max_value is None:
            max_value = float(dat["Attribute_value"].max())
            print(f"Maximum unspecified. Using {max_value}.")

        dat = dat.loc[
            (dat["Attribute_value"] >= min_value) & (dat["Attribute_value"] <= max_value)
        ].copy()
        dat["Attribute"] = (
            "bugphyzz:" + dat["Attribute"].astype(str) + f"| >= {min_value} & <= {max_value}"
        )
    else:
        thr = _thresholds(resource_dir=resource_dir)
        attr_group = _single_unique(dat["Attribute"], "Attribute")
        thr = thr.loc[thr["Attribute_group"] == attr_group].copy()

        dat["tmp_col"] = pd.NA
        for _, row in thr.iterrows():
            attr_name = row["Attribute"]
            lower = row["lower"]
            upper = row["upper"]

            if pd.isna(lower):
                lower = dat["Attribute_value"].min() - 0.01
            if pd.isna(upper):
                upper = dat["Attribute_value"].max()

            pos = (dat["Attribute_value"] > lower) & (dat["Attribute_value"] <= upper)
            dat.loc[pos, "tmp_col"] = attr_name
            dat.loc[pos, "Attribute"] = (
                "bugphyzz:"
                + dat.loc[pos, "Attribute"].astype(str)
                + f"|{attr_name}| > {round(float(lower), 2)} & <= {upper}"
            )

    return _split_unique(dat, group_col="Attribute", value_col=tax_id_type)


def _thresholds(resource_dir: str | Path | None = None) -> pd.DataFrame:
    path = _resource_path("thresholds.tsv", resource_dir=resource_dir)
    dat = pd.read_csv(path, sep="\t")

    def _range(row: pd.Series) -> str:
        lower = row.get("lower")
        upper = row.get("upper")
        if pd.isna(lower):
            return f"<={upper}"
        if pd.isna(upper):
            return f">={lower}"
        return f"{lower}-{upper}"

    dat["range"] = dat.apply(_range, axis=1)
    dat["unit"] = dat["unit"].fillna("") if "unit" in dat.columns else ""
    dat["Attribute_range"] = dat["range"].astype(str) + dat["unit"].astype(str)

    first_cols = ["Attribute_group", "Attribute", "Attribute_range"]
    remaining = [col for col in dat.columns if col not in first_cols]
    return dat[first_cols + remaining]


def _validation_data(resource_dir: str | Path | None = None) -> pd.DataFrame:
    path = _resource_path("validation_summary.tsv", resource_dir=resource_dir)
    dat = pd.read_csv(path, sep="\t")

    mcc = dat.get("mcc_mean")
    r2 = dat.get("r2_mean")
    if mcc is None or r2 is None:
        raise ValueError("validation_summary.tsv must contain mcc_mean and r2_mean columns.")

    dat["value"] = pd.NA
    dat.loc[mcc.notna() & r2.isna(), "value"] = mcc[mcc.notna() & r2.isna()]
    dat.loc[mcc.isna() & r2.notna(), "value"] = r2[mcc.isna() & r2.notna()]
    dat["value"] = pd.to_numeric(dat["value"], errors="coerce")
    return dat


def _download_resource(version: str, force_download: bool) -> list[pd.DataFrame]:
    if re.match(r"^10\.5281/zenodo\.[0-9]+$", version):
        record = re.sub(r"^10\.5281/zenodo\.", "", version)
        return _download_z(record=record, force_download=force_download)

    # R used stringr::regex("^[:alnum:]{7}$"). The intended pattern appears to
    # be a 7-character alphanumeric GitHub commit hash.
    if version == "devel" or re.match(r"^[A-Za-z0-9]{7}$", version):
        return _download_gh(version=version, force_download=force_download)

    raise ValueError("Version must be a Zenodo DOI, GitHub commit hash, or 'devel'.")


def _download_z(record: str, force_download: bool) -> list[pd.DataFrame]:
    base_url = f"https://zenodo.org/api/records/{record}"
    response = requests.get(base_url, timeout=60)
    response.raise_for_status()
    metadata = response.json()

    file_urls = []
    for file_info in metadata.get("files", []):
        api_url = file_info.get("links", {}).get("self")
        if api_url:
            # R: sub("(^.*)(api/)(.*)(/content$)", "\\1\\3", fileNamesApi)
            file_urls.append(re.sub(r"(^.*)(api/)(.*)(/content$)", r"\1\3", api_url))

    if not file_urls:
        raise RuntimeError(f"No downloadable files found for Zenodo record {record}.")

    rpath = _get_resource(
        rname="bugphyzz.zip",
        url=file_urls,
        verbose=True,
        force=force_download,
    )

    frames: list[pd.DataFrame] = []
    with tempfile.TemporaryDirectory() as tmpdir:
        with zipfile.ZipFile(rpath) as zf:
            zf.extractall(tmpdir)
        for path in Path(tmpdir).rglob("*.csv"):
            dat = pd.read_csv(path, skiprows=1)
            dat["Attribute"] = dat["Attribute"].astype(str).str.lower()
            frames.append(dat)

    return frames


def _download_gh(version: str, force_download: bool) -> list[pd.DataFrame]:
    file_suffixes = ["binary", "multistate", "numeric"]
    frames: list[pd.DataFrame] = []

    for suffix in file_suffixes:
        print(f"Importing {suffix} data...")
        url = f"https://github.com/waldronlab/bugphyzzExports/raw/{version}/bugphyzz_{suffix}.csv"
        rpath = _get_resource(
            rname=f"bugphyzz_{suffix}.csv",
            url=url,
            verbose=True,
            force=force_download,
        )
        dat = pd.read_csv(rpath, skiprows=1)
        dat["Attribute"] = dat["Attribute"].astype(str).str.lower()
        frames.append(dat)

    return frames


# ---------------------------------------------------------------------------
# Replacement for the R helper .getResource(), which was not defined in this
# script. This is deliberately small and direct; it can be swapped out later.
# ---------------------------------------------------------------------------


def _get_resource(
    rname: str,
    url: str | Iterable[str],
    verbose: bool = True,
    force: bool = False,
    cache_dir: str | Path | None = None,
) -> Path:
    cache_root = Path(cache_dir or user_cache_dir("bugphyzz", "waldronlab"))
    cache_root.mkdir(parents=True, exist_ok=True)
    dest = cache_root / rname

    if dest.exists() and not force:
        return dest

    urls = [url] if isinstance(url, str) else list(url)
    last_error: Exception | None = None

    for one_url in urls:
        try:
            if verbose:
                print(f"Downloading {one_url}")
            with requests.get(one_url, stream=True, timeout=120) as response:
                response.raise_for_status()
                with open(dest, "wb") as handle:
                    for chunk in response.iter_content(chunk_size=1024 * 1024):
                        if chunk:
                            handle.write(chunk)
            return dest
        except Exception as exc:  # Try next Zenodo file URL, if present.
            last_error = exc

    raise RuntimeError(f"Could not download resource {rname!r}.") from last_error


def _resource_path(filename: str, resource_dir: str | Path | None = None) -> Path:
    if resource_dir is None:
        base = Path(__file__).resolve().parent / "resources"
    else:
        base = Path(resource_dir)

    path = base / filename
    if not path.exists():
        raise FileNotFoundError(
            f"Could not find {filename!r}. Expected it at {path}. "
            "Pass resource_dir=... or place package TSV resources in resources/."
        )
    return path


def _single_unique(series: pd.Series, column_name: str) -> Any:
    values = series.dropna().unique()
    if len(values) != 1:
        raise ValueError(f"Expected exactly one unique value in {column_name}, found {values!r}.")
    return values[0]


def _as_list(value: str | Iterable[str]) -> list[str]:
    if isinstance(value, str):
        return [value]
    return list(value)


def _validate_choice(value: str, valid_values: set[str], arg_name: str) -> None:
    if value not in valid_values:
        raise ValueError(f"Invalid {arg_name}: {value!r}. Valid options: {sorted(valid_values)}")


def _validate_choices(values: Iterable[str], valid_values: set[str], arg_name: str) -> None:
    bad = [value for value in values if value not in valid_values]
    if bad:
        raise ValueError(f"Invalid {arg_name}: {bad!r}. Valid options: {sorted(valid_values)}")


def _split_unique(dat: pd.DataFrame, group_col: str, value_col: str) -> dict[str, list[str]]:
    if value_col not in dat.columns:
        raise KeyError(f"Column {value_col!r} is not present in the DataFrame.")

    output: dict[str, list[str]] = {}
    for name, group in dat.groupby(group_col, dropna=False):
        values = group[value_col].dropna().astype(str).drop_duplicates().tolist()
        output[str(name)] = values
    return output

def remove_cache() -> None:
    cache_dir = Path(user_cache_dir("bugphyzz"))
    if cache_dir.exists():
        shutil.rmtree(cache_dir)


# Optional R-style aliases while doing direct conversion.
importBugphyzz = import_bugphyzz
makeSignatures = make_signatures
getTaxonSignatures = get_taxon_signatures
