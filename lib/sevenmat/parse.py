from lark import Lark
from pathlib import Path

parser = Lark.open(Path(__file__).parent / "7mat.lark", propagate_positions=True, parser="lalr")

test = """
struct my_struct {
    Int_16[10] hexArray;
}

func my_func(Int_36 my_var, Int_16 value, struct my_struct hex) -> Int_36 {
    hex.hexArray[5] = value;
    if (my_var == 10) { // conditional path taken
        return 2 * my_var + -1;
    } else {
        return 2;
    }
}
"""

print(parser.parse(test).pretty())