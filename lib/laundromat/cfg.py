import matplotlib.pyplot as plt
import networkx as nx

from lib.fourmat.parse import *
from lib.laundromat.node import *


START = Node("!S")
END   = Node("!E")
CRASH = Node("!C")


def program_to_cfg(program: str) -> nx.DiGraph:
    cfg = nx.DiGraph()

    def build_clause(clause: list[Directive | str], current: Node, condition: Condition, end: Node, outer: Node):
        it = iter(clause)

        while directive := next(it, None):
            cfg.add_edge(current, current := current.copy(directive=directive), condition=condition)

            match current.kind:
                case '[':
                    closing = current.copy(directive=directive.closing_token)

                    if directive.colon:
                        if directive.at_sign:
                            raise InvalidDirective(directive)

                        build_clause(directive.clauses[0], current, Nil(), closing, outer)
                        build_clause(directive.clauses[1], current, ~Nil(), closing, outer)

                    elif directive.at_sign:
                        build_clause(directive.clauses[0], current, ~Nil(), closing, outer)
                        cfg.add_edge(current, closing, condition=Nil())

                    else:
                        match directive.get_param(0):
                            case Special.V | None:
                                raise InvalidDirective(directive)

                            case Special.Hash:
                                for index, clause in enumerate(directive.clauses[:-1] if directive.default_token else directive.clauses):
                                    build_clause(clause, current, Equal(Special.Hash, index), closing, outer)

                                default = Less(index + 1, index + 1, Special.Hash)
                                if directive.default_token:
                                    build_clause(directive.clauses[-1], current, default, closing, outer)

                                else:
                                    cfg.add_edge(current, closing, condition=default)

                            case n if 0 <= n < len(directive.clauses):
                                build_clause(directive.clauses[n], current, Condition(), closing, outer)

                            case _:
                                cfg.add_edge(current, closing, condition=Condition())

                    current = closing
                    condition = Condition()

                case '{':
                    closing = current.copy(directive=directive.closing_token)
                    try:
                        escape = current.copy(directive=next(it))

                    except StopIteration:
                        escape = end

                    if directive.colon:
                        raise InvalidDirective(directive)

                    if not directive.at_sign:
                        if current.pointer.on_tape:
                            raise InvalidDirective(directive)

                        current.pointer = current.pointer.copy(on_tape=True)
                        closing.pointer = closing.pointer.copy(on_tape=True)

                    build_clause(directive.clauses[0], current, Less(1, 1, Special.Hash), closing, escape)

                    cfg.add_edge(closing, current, condition=Condition())
                    cfg.add_edge(current, current := escape, condition=Equal(Special.Hash, 0))
                    condition = Condition()

                case '<':
                    closing = current.copy(directive=directive.closing_token)
                    try:
                        escape = current.copy(directive=next(it))

                    except StopIteration:
                        escape = end

                    # TODO: Actually handle justification
                    build_clause(sum(directive.clauses, []), current, Condition(), closing, escape)

                    cfg.add_edge(closing, current := escape, condition=Condition())
                    condition = Condition()

                case '^':
                    match current.directive.params:
                        case []:
                            termination = Equal(Special.Hash, 0)

                        case [a]:
                            termination = Equal(a, 0)

                        case [a, b]:
                            termination = Equal(a, b)

                        case [a, b, c]:
                            termination = Less(a, b, c)

                        case _:
                            raise InvalidDirective(directive)

                    cfg.add_edge(current, outer, condition=termination)
                    condition = ~termination

                case '?':
                    cfg.add_edge(current, CRASH, condition=Condition())
                    return

                case _:
                    condition = Condition()

        if current != end:
            cfg.add_edge(current, end, condition=condition)

    build_clause(parse(tokenize(program)).clauses[0], START, Condition(), END, END)
    return cfg.subgraph(nx.descendants(cfg, START) | {START}).to_directed()


def draw_cfg(cfg: nx.DiGraph, *, size: int = 12):
    plt.figure(1, figsize=(size, size))
    pos = nx.kamada_kawai_layout(cfg)

    node_categories = {
        "buffer": ([], "salmon"),
        "conditional": ([], "orange"),
        "loop": ([], "khaki"),
        "directive": ([], "skyblue"),
        "control": ([], "violet"),
        "string": ([], "silver")
    }

    # Node types
    for node in cfg:
        if node.is_buffer:
            category = "buffer"

        elif node.is_conditional:
            category = "conditional"

        elif node.is_loop:
            category = "loop"

        elif node.kind == "str":
            category = "string"

        elif node.kind in ["!S", "!E", "!C"]:
            category = "control"

        else:
            category = "directive"

        node_categories[category][0].append(node)

    nx.draw_networkx_labels(cfg, pos, {node: str(node) for node in cfg})
    for category, (nodes, color) in node_categories.items():
        nx.draw_networkx_nodes(cfg, nodelist=nodes, pos=pos, node_color=color,
                               node_size=1200, node_shape="o", edgecolors="gray")


    conditions = nx.get_edge_attributes(cfg, "condition")
    nx.draw_networkx_edges(cfg, pos=pos)
    nx.draw_networkx_edge_labels(cfg, pos=pos, edge_labels=conditions)

    plt.show()


CFG = program_to_cfg("""~:[█ ~%~;~]~:*~
~{~<~#,#,101^~?~>█~@{~#,#,3^~
    ~<~v,' ^~<~v,' ^~<~v,' ^ ~>~:*~>~:*~>~:*~
    ~<~v,' ^~<~v,' ^~<~v,'█^█~>~:*~>~:*~>~:*~
    ~<~v,' ^~<~v,'█^~<~v,' ^█~>~:*~>~:*~>~:*~
    ~<~v,' ^~<~v,'█^~<~v,'█^█~>~:*~>~:*~>~:*~
    ~<~v,'█^~<~v,' ^~<~v,' ^█~>~:*~>~:*~>~:*~
    ~<~v,'█^~<~v,' ^~<~v,'█^█~>~:*~>~:*~>~:*~
    ~<~v,'█^~<~v,'█^~<~v,' ^ ~>~:*~>~:*~>~:*~
    ~<~v,'█^~<~v,'█^~<~v,'█^ ~>~:*~>~:*~>~
~}█ ~%~0^~}""")
draw_cfg(CFG, size=48)
