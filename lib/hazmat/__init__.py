from .translator import Translator

def fivemat_to_c(source: str) -> str:
    tr = Translator(source)
    tr.translate_ast_root()

    return tr.writer.result
