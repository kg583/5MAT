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

    def __init__(self, program=None):
        if not isinstance(program, str):
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
        build(parse(program).clauses[0], CFG.START)

    def __iter__(self):
        topo = self.copy()
        for edge, back in nx.get_edge_attributes(topo, "back").items():
            if back:
                topo.remove_edge(*edge)

        return nx.topological_sort(topo)

    def __str__(self) -> str:
        nodes = list(self)
        program = ""

        while nodes:
            node = nodes.pop(0)
            match node.kind:
                case "ctrl":
                    continue

                case "[" if node.directive.get_param(0) == Special.Hash:
                    cases = {}
                    for child in self[node]:
                        descendants = self.nodes_between(child, node.closing) - {node.closing}
                        path = str(self.subgraph(descendants))

                        for descendant in descendants:
                            if descendant in nodes:
                                nodes.remove(descendant)

                        match self.conditions()[(node, child)]:
                            case Equal(Special.Hash, a) | Equal(a, Special.Hash):
                                cases[a] = path

                            case Less(a, b, Special.Hash):
                                cases[-1] = path

                            case _:
                                raise InvalidNode(node.directive)

                    program += f"~#[{'~;'.join(cases.get(n, '') for n in range(max(cases) + 1))}"
                    if cases.get(-1):
                        program += f"~:;{cases[-1]}"

                case _:
                    program += str(node.directive)

        return encode_escapes(program)

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

    def walk(self, node: Node, length: int) -> list[Node]:
        walk = []
        for _ in range(length):
            walk.append(node)
            try:
                node, = self[node]

            except ValueError:
                break

        return walk
