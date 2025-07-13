from dataclasses import dataclass
from inspect import signature
from typing import Callable, Optional

from .sixmat import SixMat


@dataclass
class RegistryEntry:
    generator: Callable
    is_generic: Optional[bool] = None

    requires_hell: bool = False
    creates_hell: bool = False


class Linker:
    _registry: dict[str, RegistryEntry] = {}
    _generated_blocks: dict[str, str] = {}

    @staticmethod
    def reset():
        Linker._registry = {}
        Linker._generated_blocks = {}

    @staticmethod
    def register(requires_hell=False, creates_hell=False):
        """
        Should be the first decorator after `@staticmethod` (i.e. the last one to be called).
        The function being decorated must have one argument, and may accept a second.
            1. first argument is SixMat
            2. second argument (optional) is the parameter in the parameterization of the function (usually the integer
               base for arithmetic functions)

        The __qualname__ of the function is used as the registry key. Usually, this is `classname.fnname`

        Block preconditions:
            - A read from the tape pointer will return the first character of the first local variable in the currently
              executing function (this is handled automatically by this decorator)
            - Function stack laid out correctly in previous block

        Block postconditions:
            - Primary concern is that everything is written in order. You are responsible for copying everything within
              this function's stack frame.
            - Hell was set up if requested.
        """

        def decorator(fn):
            arg_count = len(signature(fn).parameters)
            if arg_count < 1 or arg_count > 2:
                raise ValueError

            entry = RegistryEntry(fn, is_generic=(arg_count==3), requires_hell=requires_hell, creates_hell=creates_hell)
            Linker._registry[fn.__qualname__] = entry

            return fn

        return decorator

    @staticmethod
    def _check_key_specifier(key: str, specifier = None) -> RegistryEntry:
        if key not in Linker._registry:
            raise KeyError(f"Registry: Unknown key '{key}'")

        entry = Linker._registry[key]
        if specifier is None and entry.is_generic:
            raise ValueError(f"Registry: Block '{key}' is generic, but no type specifier was provided.")

        return entry

    @staticmethod
    def _get_generator(key: str, specifier = None) -> Callable[[SixMat], None]:
        entry = Linker._check_key_specifier(key, specifier = specifier)

        if specifier is None:
            return entry.generator
        else:
            return lambda s: entry.generator(s, specifier)

    # Guiding philosophy: only this class should care about what the linker directive format actually is.
    @staticmethod
    def insert_path(s: SixMat, key: str, specifier = None):
        # debug check; when we are programmatically generating code, _check_key_specifier won't work because the blocks
        # won't all be in the registry yet (this is, like, the point of this class). When blocks are registered
        # self._check_key_specifier(key, specifier)
        if specifier is None:
            s.raw_text(f"%PATH NON_GENERIC {key}\n")
        if isinstance(specifier, int):
            s.raw_text(f"%PATH INT_GENERIC {key} {specifier}\n")