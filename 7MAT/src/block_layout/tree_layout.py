from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Callable

from .block_registry import QualifiedKey, GeneratedRegistryEntry, BlockRegistry
from .linker import Linker
from ..sixmat import A, SixMat
from ..constants import ControlChar


class BlockTree(ABC):
    @abstractmethod
    def to_sixmat(self, s: SixMat):
        pass


@dataclass
class BlockLeaf(BlockTree):
    key: QualifiedKey
    contents: str

    def to_sixmat(self, s: SixMat):
        s.instn("GOTO", A.int(0))
        s.instn("SKIPF!")
        s.raw_text(self.contents, indent=True)


@dataclass
class BlockBranch(BlockTree):
    condition: str
    if_true: BlockTree
    if_false: BlockTree

    def to_sixmat(self, s: SixMat):
        with s.block_instn("IFEQ!", A.read(), A.chr(self.condition)):
            self.if_true.to_sixmat(s)
            s.instn("SKIP", A.remaining())  # sometimes this is important
        s.instn("BRZR")
        self.if_false.to_sixmat(s)


def tree_layout(generated_blocks: dict[QualifiedKey, GeneratedRegistryEntry]):
    paths = {}

    def arrange(leaves: list[BlockLeaf], prefix: str, depth: int = 0):
        if not leaves:
            return None

        if len(leaves) == 1:
            leaf = leaves[0]
            paths[leaf.key] = prefix
            return leaf

        # we can fiddle with this condition as we please, just needs to divide in two
        # something less sensitive to small changes in #(blocks) could be nice but isn't really necessary.
        left_group = leaves[::2]
        right_group = leaves[1::2]

        condition_char = ControlChar.path(depth, True)
        left_prefix = prefix + condition_char
        right_prefix = prefix + ControlChar.path(depth, False)

        left_branch = arrange(left_group, left_prefix, depth + 1)
        right_branch = arrange(right_group, right_prefix, depth + 1)

        return BlockBranch(condition_char, left_branch, right_branch)

    hell_leaves = []
    normal_leaves = []
    for qualified_key, generated_block in generated_blocks.items():
        (hell_leaves if generated_blocks[qualified_key].creates_hell else normal_leaves).append(
            BlockLeaf(qualified_key, generated_block.text))

    return paths, (arrange(hell_leaves, ControlChar.HELL_PATH_DESIGNATOR), arrange(normal_leaves, ""))


def blocks_to_sixmat(entrypoint: str | Callable = "start", *entrypoint_params) -> str:
    generated_blocks = BlockRegistry.generate_blocks(entrypoint, *entrypoint_params)
    paths, (hell_tree, normal_tree) = tree_layout(generated_blocks)

    s = SixMat()
    with s.block_instn("INIT"):
        s.instn("PRFF")
        s.instn("PRINC", A.chr(ControlChar.HELL_FILL_CHAR))
        Linker.insert_path(s, entrypoint, *entrypoint_params)
        s.instn("PRFF")

    with s.block_instn("DO"):
        s.instn("SKIPC!", A.chr(ControlChar.HELL_FILL_CHAR))
        if hell_tree is not None:
            with s.block_instn("IFEQ!", A.read(), A.chr(ControlChar.HELL_PATH_DESIGNATOR)):
                with s.block_instn("JUST", A.remaining(), A.remaining(), A.remaining(),
                                   A.chr(ControlChar.HELL_FILL_CHAR)):
                    with s.block_instn():
                        hell_tree.to_sixmat(s)
                if normal_tree is not None:
                    s.instn("SKIP", A.remaining())
            if normal_tree is not None:
                s.instn("BRZR")
                s.instn("BACK")
        if normal_tree is not None:
            normal_tree.to_sixmat(s)

    return Linker.resolve_path_directives(s.result, paths)
