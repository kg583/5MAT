import logging
import matplotlib.pyplot as plt
import networkx as nx

from lib.fivemat.util import *
from lib.laundromat.node import *

logger = logging.getLogger(__name__)


class CFG(nx.DiGraph):
    START = Node(Control.Start)
    END = Node(Control.End)
    CRASH = Node(Control.Crash)
    UB = Node(Control.UB)

    START.closing = END
    START.escape = END
    START.pointer = Less(1, 1, Special.Hash).enforce(Pointer().start())

    class Walker:
        def sep(self, node: Node, index: int):
            pass

        def visit(self, node: Node):
            pass

        def walk(self, cfg: 'CFG'):
            def step(node: Node):
                self.visit(node)

                match node.kind:
                    case '[':
                        clauses = {}

                        if node.directive.colon or node.directive.at_sign:
                            for child in cfg[node]:
                                match str(cfg.conditions()[(node, child)]):
                                    case "NIL":
                                        clauses[0] = child

                                    case "T":
                                        clauses[1] = child

                                    case _:
                                        raise InvalidNode(node.directive)

                            order = [0, 1] if node.directive.colon else [1]

                        else:
                            for child in cfg[node]:
                                if child == node.closing:
                                    continue

                                match cfg.conditions()[(node, child)]:
                                    case Equal(Special.Hash, a) | Equal(a, Special.Hash):
                                        clauses[a] = child

                                    case Less(a, b, Special.Hash):
                                        clauses[-1] = child

                                    case _:
                                        raise InvalidNode(node.directive)

                            order = [*range(max(clauses, default=-1) + 1), -1]

                        for n in order:
                            self.sep(node, n)
                            if n in clauses:
                                step(clauses[n])

                        step(node.closing)

                    case '{':
                        for child in cfg[node]:
                            if child == node.entry:
                                step(child)

                        self.visit(node.closing)
                        step(node.escape)

                    case '<':
                        child, = cfg[node]
                        step(child)

                        step(node.closing)

                    case _:
                        for child in cfg[node]:
                            if child not in [node.closing, node.escape]:
                                step(child)

            step(CFG.START)
            self.visit(CFG.END)

    def __init__(self, program=None):
        if not isinstance(program, BlockDirective):
            super().__init__(program)
            return

        def build(clause: list[Directive | str], node: Node, *, condition: Condition = Always()):
            for directive in clause:
                self.add_edge(node, child := node.copy(directive=directive), condition=condition)
                condition = Always()

                if isinstance(directive, str):
                    node = child
                    continue

                match child.kind:
                    case '[':
                        child.closing = child.copy(directive=directive.closing_token)

                        if directive.colon:
                            if directive.at_sign:
                                raise InvalidNode(directive)

                            build(directive.clauses[0], child, condition=Nil())
                            build(directive.clauses[1], child, condition=~Nil())

                        elif directive.at_sign:
                            build(directive.clauses[0], child, condition=~Nil())
                            self.add_edge(child, child.closing, condition=Nil())

                        else:
                            match directive.get_param(0):
                                case Special.V | None:
                                    raise InvalidNode(directive)

                                case Special.Hash:
                                    if directive.default_token:
                                        for index, clause in enumerate(directive.clauses[:-1]):
                                            build(clause, child, condition=Equal(Special.Hash, index))

                                        build(directive.clauses[-1], child, condition=Less(index + 1, index + 1, Special.Hash))

                                    else:
                                        for index, clause in enumerate(directive.clauses):
                                            build(clause, child, condition=Equal(Special.Hash, index))

                                        self.add_edge(child, child.closing, condition=Less(index + 1, index + 1, Special.Hash))

                                case n if 0 <= n < len(directive.clauses):
                                    build(directive.clauses[n], child)

                                case _:
                                    self.add_edge(child, child.closing)

                        node = child.closing

                    case '{':
                        if directive.colon or not directive.at_sign:
                            raise InvalidNode(directive)

                        child.escape, child.closing = (child.copy(directive=Control.Empty),
                                                       child.copy(directive=directive.closing_token))
                        child.entry = child.copy(directive=Control.Empty)

                        self.add_edge(child, child.entry, condition=Less(1, 1, Special.Hash))
                        self.add_edge(child.closing, child.entry, condition=Less(1, 1, Special.Hash), back=True)
                        build(directive.clauses[0], child.entry)

                        self.add_edge(child.closing, child.escape, condition=Equal(Special.Hash, 0))
                        self.add_edge(child, node := child.escape, condition=Equal(Special.Hash, 0))

                    case '<':
                        child.escape, child.closing = (child.copy(directive=Control.Empty),
                                                       child.copy(directive=directive.closing_token))

                        buffer = []
                        clauses = directive.clauses.copy()

                        if directive.default_token:
                            buffer.extend(clauses.pop(0) + [directive.default_token])

                        for section in clauses[:-1]:
                            buffer.extend(section + [Directive(";", [])])

                        # TODO: Actually handle justification
                        build(buffer + clauses[-1], child)

                        self.add_edge(child.closing, node := child.escape)

                    case "(":
                        child.closing = child.copy(directive=directive.closing_token)
                        build(directive.clauses, child)
                        node = child.closing

                    case '^':
                        match directive.params:
                            case []:
                                termination = Equal(Special.Hash, 0)

                            case [a]:
                                termination = Equal(a, 0)

                            case [a, b]:
                                termination = Equal(a, b)

                            case [a, b, c]:
                                termination = Less(a, b, c)

                            case _:
                                raise InvalidNode(directive)

                        self.add_edge(node := child, child.escape, condition=termination)
                        condition = ~termination

                    case _ if child.crashes:
                        self.crash_on(child)
                        break

                    case _:
                        node = child

            else:
                self.add_edge(node, node.closing, condition=condition)

        super().__init__()
        build(program.clauses[0], CFG.START)

    def __iter__(self):
        class Walker(CFG.Walker, list):
            def visit(self, node: Node):
                self.append(node)

        walker = Walker()
        walker.walk(self)
        return iter(walker)

    def __str__(self) -> str:
        class Walker(CFG.Walker, list):
            def sep(self, node: Node, index: int):
                match index:
                    case 0:
                        pass

                    case 1 if node.directive.at_sign:
                        pass

                    case -1:
                        if node.directive.default_token:
                            self.append("~:;")

                    case _:
                        self.append("~;")

            def visit(self, node: Node):
                if node.kind != "ctrl":
                    self.append(str(node))

        walker = Walker()
        walker.walk(self)
        return encode_escapes("".join(walker))

    def add_edge(self, u: Node, v: Node, **attrs):
        super().add_edge(u, v,
                         condition=attrs.get("condition", Always()),
                         back=attrs.get("back", False))

    def conditions(self) -> dict[tuple[Node, Node], Condition]:
        return nx.get_edge_attributes(self, "condition")

    def contract_subgraph(self, u: Node, v: Node):
        empty = nx.DiGraph()
        empty.add_node(Node(Control.Empty))

        self.replace_subgraph(u, v, empty)

    def crash_on(self, node: Node):
        self.remove_edges_from([*self.edges(node)])
        self.add_edge(node, CFG.CRASH)
        self.add_edge(node, node.closing, condition=~Always())

        nx.relabel_nodes(self, {node: node.copy(directive=Directive("?", []))}, copy=False)

    def descendants(self, node: Node) -> set[Node]:
        return nx.descendants(self, node) | {node}

    def draw(self, *, size: int = 12, layout=nx.kamada_kawai_layout):
        plt.figure(1, figsize=(size, size))
        pos = layout(self)

        node_categories = {
            "buffer": ([], "salmon"),
            "conditional": ([], "orange"),
            "loop": ([], "khaki"),
            "directive": ([], "skyblue"),
            "control": ([], "violet"),
            "string": ([], "silver")
        }

        # Node types
        for node in self:
            if node.kind in "<>":
                category = "buffer"

            elif node.kind in "[]":
                category = "conditional"

            elif node.kind in "{}":
                category = "loop"

            elif node.kind == "str":
                category = "string"

            elif node.kind == "ctrl":
                category = "control"

            else:
                category = "directive"

            node_categories[category][0].append(node)

        nx.draw_networkx_labels(self, pos, {node: str(node) for node in self}, font_size="small")
        for category, (nodes, color) in node_categories.items():
            nx.draw_networkx_nodes(self, nodelist=nodes, pos=pos, node_color=color,
                                   node_size=1200, node_shape="o", edgecolors="gray")

        true = [edge for edge, condition in self.conditions().items() if condition != ~Always()]
        false = [edge for edge, condition in self.conditions().items() if condition == ~Always()]

        nx.draw_networkx_edges(self, edgelist=true, style="solid", pos=pos, node_size=1300, arrowstyle="]->")
        nx.draw_networkx_edges(self, edgelist=false, style="dotted", pos=pos, node_size=1300, arrowstyle="]->")

        nx.draw_networkx_edge_labels(self, pos=pos, edge_labels=self.conditions())

        plt.show()

    def nodes_between(self, u: Node, v: Node) -> set[Node]:
        # This *seems* terrible no good very bad but maybe isn't?
        return {node for path in self.paths_between(u, v) for node in path}

    def paths_between(self, u: Node, v: Node):
        return nx.all_simple_paths(self, u, v)

    def path_from(self, node: Node, length: int) -> list[Node]:
        path = []
        for _ in range(length):
            path.append(node)
            try:
                node, = self[node]

            except ValueError:
                break

        return path

    def progeny(self, node: Node) -> 'CFG':
        return self.subgraph(self.descendants(node))

    def reachable(self) -> 'CFG':
        return self.progeny(CFG.START)

    def replace_subgraph(self, u: Node, v: Node, subgraph: nx.DiGraph):
        pre, succ = self.predecessors(u), self.successors(v)

        self.remove_edges_from([(t, u) for t in pre])
        self.remove_edges_from([(v, w) for w in succ])

        self.add_edges_from([(t, min(subgraph, key=subgraph.in_degree)) for t in pre])
        self.add_edges_from([(min(subgraph, key=subgraph.out_degree), w) for w in succ])

    def subgraph(self, nodes) -> 'CFG':
        return CFG(super().subgraph(nodes).to_directed())

    def subgraph_between(self, u: Node, v: Node) -> 'CFG':
        return self.subgraph(self.nodes_between(u, v))

    def swap_nodes(self, u: Node, v: Node):
        pre, succ = self.predecessors(u), self.successors(v)

        self.remove_edges_from([(t, u) for t in pre])
        self.remove_edges_from([(v, w) for w in succ])
        self.remove_edge(u, v)

        self.add_edges_from([(t, v) for t in pre])
        self.add_edges_from([(u, w) for w in succ])
        self.add_edge(v, u)

    def terminates_from(self, node: Node) -> bool:
        return bool({CFG.CRASH, CFG.END} & self.descendants(node))

    def tree(self) -> BlockDirective:
        class Walker(CFG.Walker, list):
            def sep(self, node: Node, index: int):
                self[-1].clauses.append([])

            def visit(self, node: Node):
                if isinstance(node.directive, Control):
                    return

                elif isinstance(node.directive, BlockDirective):
                    self.append(node.directive)
                    return

                elif node.directive == self[-1].closing_token:
                    directive = self.pop()

                else:
                    directive = node.directive

                self[-1].clauses[-1].append(directive)

        walker = Walker([BlockDirective("{", [1], closing_token=Directive("}", []))])
        walker.walk(self)
        return walker.pop()

    def update_pointers(self):
        for node in self:
            if node.kind != "ctrl":
                node.pointer = node.pointer.clear()

        for node in self:
            for child in self[node]:
                if child.kind == "ctrl":
                    continue

                child.pointer |= self[node][child]["condition"].enforce(node.pointer).step(child)
                logger.debug(f"Updated {str(child):10}{child.pointer}")


__all__ = ["CFG"]
