from pathlib import Path
from typing import Union

import pandas as pd

from .physiologies import _add_source_info, _reorder_columns


def custom_links(
    keyword: Union[str, list[str]] = "all",
    resource_dir: Union[str, Path] = "resources",
) -> pd.DataFrame:
    resource_dir = Path(resource_dir)
    links_path = resource_dir / "spreadsheet_customlinks.tsv"

    links = pd.read_csv(links_path, sep="\t")

    if isinstance(keyword, str):
        keyword = [keyword]

    if keyword[0] == "all":
        return links

    return links[links["physiology"].isin(keyword)].copy()


def fatty_acid_composition(
    resource_dir: Union[str, Path] = "resources",
    ranks_parents: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """
    Direct Python translation of .fattyAcidComposition().

    Parameters
    ----------
    resource_dir
        Directory containing extdata files, now stored as resources/.
    ranks_parents
        Python equivalent of the internal R object ranksParents.
        Must contain NCBI_ID and parent taxonomy columns.
    """
    if ranks_parents is None:
        ranks_path = Path(resource_dir) / "ranks_parents.tsv"
        ranks_parents = pd.read_csv(ranks_path, sep="\t", dtype=str)

    links = custom_links(resource_dir=resource_dir)

    link = links.loc[
        links["functionname"] == "fattyAcidComposition",
        "link",
    ].iloc[0]

    fac_wide = pd.read_csv(link)

    start_col = fac_wide.columns.get_loc("Br-C10:1")
    end_col = fac_wide.columns.get_loc("Oxo-C19:1")
    fatty_acid_cols = list(fac_wide.columns[start_col : end_col + 1])

    id_cols = [col for col in fac_wide.columns if col not in fatty_acid_cols]

    fac_long = fac_wide.melt(
        id_vars=id_cols,
        value_vars=fatty_acid_cols,
        var_name="Attribute_new",
        value_name="Attribute_value",
    )

    fac_long["NCBI_ID"] = fac_long["NCBI_ID"].astype(str)
    ranks_parents = ranks_parents.copy()
    ranks_parents["NCBI_ID"] = ranks_parents["NCBI_ID"].astype(str)

    out = fac_long.merge(ranks_parents, on="NCBI_ID", how="left")

    out = _add_source_info(out, resource_dir=resource_dir)

    for col in [
        "Attribute",
        "Frequency",
        "Evidence",
        "Confidence_in_curation",
    ]:
        if col in out.columns:
            out[col] = out[col].astype(str).str.lower().str.strip()

    if "Attribute" in out.columns:
        out = out.drop(columns=["Attribute"])

    out = out.rename(columns={"Attribute_new": "Attribute"})

    out = _reorder_columns(
        out,
        attr_type="numeric",
        resource_dir=resource_dir,
    )

    return out.reset_index(drop=True)