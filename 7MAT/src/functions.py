from collections import defaultdict
from typing import Callable
from warnings import warn

from .block_layout import Linker, BlockRegistry
from .sixmat import SixMat, A
from .constants import ControlChar

class Fn:
    @staticmethod
    def jump(s: SixMat, target: Callable | str, *parameters, perform_checks=True):
        """
        Don't use this for function calls!
        """
        key = target if isinstance(target, str) else target.__qualname__

        if perform_checks:
            try:
                from .block_layout.block_registry import QualifiedKey
                BlockRegistry.check_qualified_key(QualifiedKey(key, *parameters))
            except Exception as e:
                warn(str(e))

        s.instn("COPYC!", A.chr(ControlChar.HELL_FILL_CHAR))
        s.instn("PRINC", A.chr(ControlChar.HELL_FILL_CHAR))

        Linker.insert_path(s, key, *parameters)
        s.instn("PRFF")


__all__ = ["Fn"]
