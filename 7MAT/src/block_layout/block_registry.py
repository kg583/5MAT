import collections
from dataclasses import dataclass
from inspect import Parameter, signature
from typing import Callable
from ..sixmat import SixMat

type Key = str


@dataclass
class QualifiedKey:
    key: str
    params: tuple[...]

    def __init__(self, key: str | Callable, *params):
        self.key = key if isinstance(key, str) else key.__qualname__
        self.params = tuple(params)

    def __hash__(self) -> int:
        return hash((self.key, self.params))

    def __str__(self):
        def paramstr(p):
            if isinstance(p, int):
                return f"i{p}"
            else:
                raise TypeError("QualifiedKey only knows about integers (can be easily expanded)")

        return " ".join([self.key, *map(paramstr, self.params)])

    @classmethod
    def from_fragments(cls, parts: list[str]) -> "QualifiedKey":
        registry_key = parts[0]
        parameters = []
        for param in parts[1:]:
            match param[0]:
                case 'i':
                    parameters.append(int(param[1:]))
                case _:
                    raise ValueError(f"Invalid QualifiedKey parameter {param}")

        return cls(registry_key, *parameters)


@dataclass
class UngeneratedRegistryEntry:
    generator: Callable[[SixMat, ...], None]
    params_cnt: int
    """
    Number of compile-time parameters.
    """

    requires_hell: bool = False


@dataclass
class GeneratedRegistryEntry:
    text: str
    creates_hell: bool = False


class BlockRegistry:
    _ungenerated_blocks: dict[Key, UngeneratedRegistryEntry] = {}

    @classmethod
    def register(cls, *, requires_hell=False):
        """
        Should be the first decorator after `@staticmethod` (i.e. the last one to be called).
        The function being decorated must have at least one argument, and the first argument must be SixMat, for writing
        output 6MAT to. Remaining arguments (optional) describe how the block is compile-time parameterized (eg. usually
        the first integer argument is the base for arithmetic functions). It is your responsibility to ensure that
        any compile-time parameters are correct and valid.

        The __qualname__ of the function is used as the registry key. Usually, this takes the form `classname.fnname`

        Block preconditions:
            - A read from the tape pointer will return the first character of the first local variable in the currently
              executing function (this is handled automatically by this decorator)
            - Function stack laid out correctly in previous block

        Block postconditions:
            - Primary concern is that everything is written in order. You are responsible for copying everything within
              this function's stack frame.
            - Hell was set up if requested. This is handled by the linker.
        """

        def decorator(fn):
            arg_count = len(list(
                filter(lambda p: p.kind == Parameter.POSITIONAL_ONLY or p.kind == Parameter.POSITIONAL_OR_KEYWORD,
                       signature(fn).parameters.values())))
            if arg_count < 1:
                raise ValueError

            entry = UngeneratedRegistryEntry(fn, params_cnt=arg_count - 1, requires_hell=requires_hell)
            cls._ungenerated_blocks[fn.__qualname__] = entry

            return fn

        return decorator

    @classmethod
    def _get_generator(cls, qual_key: QualifiedKey) -> Callable[[SixMat], None]:
        entry = cls.check_qualified_key(qual_key)

        if not qual_key.params:
            return entry.generator
        else:
            return lambda s: entry.generator(s, *qual_key.params)

    @classmethod
    def check_qualified_key(cls, qual_key: QualifiedKey):
        if qual_key.key not in cls._ungenerated_blocks:
            raise KeyError(f"Registry: Unknown key '{qual_key}'")

        entry = cls._ungenerated_blocks[qual_key.key]
        if len(qual_key.params) != entry.params_cnt:
            raise ValueError(
                f"Registry: Parameter count mismatch. Block '{qual_key}' takes {entry.params_cnt} but {len(qual_key.params)} {'was' if len(qual_key.params) == 1 else 'were'} provided")

        return entry

    @classmethod
    def generate_blocks(cls, entrypoint: str | Callable = "start", *entry_point_params) -> dict[
        QualifiedKey, GeneratedRegistryEntry]:
        result = {}

        class UniqueQueue:
            """
            Queue that only enqueues elements if they have never been enqueued before.
            """

            def __init__(self):
                self._queue = collections.deque()
                self._seen = set()

            def enqueue_if_unseen(self, item):
                if item not in self._seen:
                    self._queue.append(item)
                    self._seen.add(item)

            def dequeue(self):
                if not self._queue:
                    return None
                item = self._queue.popleft()
                # item stays in _seen
                return item

            def __len__(self):
                return len(self._queue)

        reference_queue = UniqueQueue()
        reference_queue.enqueue_if_unseen(QualifiedKey(entrypoint, *entry_point_params))

        while reference_queue:
            key = reference_queue.dequeue()
            s = SixMat()
            cls._get_generator(key)(s)

            new_entry = GeneratedRegistryEntry(s.result)

            # scan references of %Path and %Htap to find new blocks-- poor man's dead code elimination
            for line in filter(lambda x: x.startswith('%'), new_entry.text.split('\n')):
                start, *rest = line.split(' ')
                match start:
                    case "%Path" | "%Htap":
                        found_block = QualifiedKey.from_fragments(rest)

                        if cls._ungenerated_blocks.get(found_block.key).requires_hell:
                            new_entry.creates_hell = True

                        reference_queue.enqueue_if_unseen(found_block)
                    case _:
                        raise SyntaxError(f"Linker: Unrecognized directive {line}")

            result[key] = new_entry
        return result
