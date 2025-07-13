import inspect
from typing import Callable
from warnings import warn

from .linker import Linker
from .sixmat import SixMat

class Fn:
    @staticmethod
    def jump(s: SixMat, target: Callable | str, specifier = None, perform_checks = True):
        """
        Don't use this for function calls!
        """
        jumper = inspect.currentframe().f_back.f_code.co_qualname
        key = target if isinstance(target, str) else target.__qualname__

        if perform_checks:
            # yeah yeah i get it maybe i shouldn't depend on linker internals
            try:
                Linker._check_key_specifier(key, specifier)
            except Exception as e:
                warn(str(e))

            if Linker._registry.get(key).requires_hell and not Linker._registry.get(jumper).creates_hell:
                warn(f"Attempted to jump to {key}, which expects a set-up hell, from {jumper}, which does not set up hell.")

        with s.block_instn("LOOP"):
            s.instn("COPY")
        s.instn("PRFF")
        Linker.insert_path(s, key, specifier)
        s.instn("PRFF")

