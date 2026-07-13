"""Python port of Bugphyzz."""

try:
    from .importers import import_bugphyzz, importBugphyzz
except Exception:  # pragma: no cover
    pass

try:
    from .importers import make_signatures, get_taxon_signatures
    makeSignatures = make_signatures
    getTaxonSignatures = get_taxon_signatures
except Exception:  # pragma: no cover
    pass

try:
    from .physiologies import physiologies, show_phys
    showPhys = show_phys
except Exception:  # pragma: no cover
    pass

try:
    from .fatty_acid_composition import fatty_acid_composition, custom_links
except Exception:  # pragma: no cover
    pass

__version__ = "0.0.1"
