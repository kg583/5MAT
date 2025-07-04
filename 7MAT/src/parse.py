from lark import Lark
from pathlib import Path

parser = Lark.open(Path(__file__).parent / "7mat.lark", propagate_positions=True, parser="lalr")

test = """func my_func(Int_32 my_var) -> Dec {
    if (my_var == 10) { // conditional path taken
        return 2 * my_var + 1;
    } else {
        return 2;
    }
}
"""
print(parser.parse(test).pretty())