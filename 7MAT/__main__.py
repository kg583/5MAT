from src.sixmat import SixMat, A
from src.functions import Fn

from src.block_layout import *

class MyFn(Fn):
    @staticmethod
    @BlockRegistry.register()
    def block_1(s: SixMat):
        s.comment("Entrypoint")

        s.comment("JUMP TO BLOCK 5")
        Fn.jump(s, "MyFn.counter", 5)

    @staticmethod
    @BlockRegistry.register(requires_hell=True)
    def counter(s: SixMat, base: int):
        # s.instn("PRINA", A.str(str(base)))
        s.comment(f"BLOCK {base}")

        if base > 2:
            s.comment(f"JUMP TO BLOCK {base-1}")
            Fn.jump(s, "MyFn.counter", base - 1)
        else:
            s.instn("CRASH")


print(blocks_to_sixmat(MyFn.block_1))