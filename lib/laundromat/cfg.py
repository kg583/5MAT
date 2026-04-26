import matplotlib.pyplot as plt
import networkx as nx

from lib.fourmat.parse import *
from lib.laundromat.node import *


class CFG(nx.DiGraph):
    START = Node(Control.Start)
    END = Node(Control.End)
    CRASH = Node(Control.Crash)
    UB = Node(Control.UB)

    START.closing = END
    START.escape = END
    START.pointer = Less(1, 1, Special.Hash).enforce(Pointer().start())

    def __init__(self, program: str = None):
        super().__init__()

        def build(clause: list[Directive | str], current: Node, condition: Condition):
            for directive in clause:
                self.add_edge(current, current := current.copy(directive=directive), condition=condition)

                condition = Condition()

                if isinstance(directive, str):
                    continue

                match current.kind:
                    case '[':
                        current.closing = current.copy(directive=directive.closing_token)

                        if directive.colon:
                            if directive.at_sign:
                                raise InvalidNode(directive)

                            build(directive.clauses[0], current, Nil())
                            build(directive.clauses[1], current, ~Nil())

                        elif directive.at_sign:
                            build(directive.clauses[0], current, ~Nil())
                            self.add_edge(current, current.closing, condition=Nil())

                        else:
                            match directive.get_param(0):
                                case Special.V | None:
                                    raise InvalidNode(directive)

                                case Special.Hash:
                                    if directive.default_token:
                                        for index, clause in enumerate(directive.clauses[:-1]):
                                            build(clause, current, Equal(Special.Hash, index))

                                        build(directive.clauses[-1], current, Less(index + 1, index + 1, Special.Hash))

                                    else:
                                        for index, clause in enumerate(directive.clauses):
                                            build(clause, current, Equal(Special.Hash, index))

                                        self.add_edge(current, current.closing, condition=Less(index + 1, index + 1, Special.Hash))

                                case n if 0 <= n < len(directive.clauses):
                                    build(directive.clauses[n], current, Condition())

                                case _:
                                    self.add_edge(current, current.closing)

                        current = current.closing

                    case '{':
                        if directive.colon or not directive.at_sign:
                            raise InvalidNode(directive)

                        current.escape, current.closing = (current.copy(directive=Control.Empty),
                                                           current.copy(directive=directive.closing_token))
                        current.entry = current.copy(directive=Control.Empty)

                        self.add_edge(current, current.entry, condition=Less(1, 1, Special.Hash))
                        self.add_edge(current.closing, current.entry, condition=Less(1, 1, Special.Hash), back=True)
                        build(directive.clauses[0], current.entry, Condition())

                        self.add_edge(current.closing, current.escape, condition=Equal(Special.Hash, 0))
                        self.add_edge(current, current := current.escape, condition=Equal(Special.Hash, 0))

                    case '<':
                        current.escape, current.closing = (current.copy(directive=Control.Empty),
                                                           current.copy(directive=directive.closing_token))

                        buffer = []
                        clauses = directive.clauses.copy()

                        if directive.default_token:
                            buffer.extend(clauses.pop() + [directive.default_token])

                        for section in clauses[:-1]:
                            buffer.extend(section + [Directive(";", [])])

                        # TODO: Actually handle justification
                        build(buffer + clauses[-1], current, Condition())

                        self.add_edge(current.closing, current := current.escape)

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

                        self.add_edge(current, current.escape, condition=termination)
                        condition = ~termination

                    case '?':
                        self.add_edge(current, CFG.CRASH)
                        return

                    case '%' | '&' | '|' | '~' | '*' if Special.V in directive.params:
                        self.crash_on(current)
                        return

                    case _:
                        pass

            self.add_edge(current, current.closing, condition=condition)

        if program:
            build(parse(tokenize(program)).clauses[0], CFG.START, Condition())

    def __iter__(self):
        topo = self.copy()
        for edge, back in nx.get_edge_attributes(topo, "back").items():
            if back:
                topo.remove_edge(*edge)

        return nx.topological_sort(topo)

    def __str__(self) -> str:
        nodes = list(self)
        conditions = nx.get_edge_attributes(self, "condition")
        program = ""

        while nodes:
            current = nodes.pop(0)

            match current.kind:
                case "ctrl":
                    continue

                # Loop exit always occurs on first iteration
                case "{" if current.closing not in nx.descendants(self, current):
                    nil = non_nil = ""
                    for child in self[current]:
                        path = str(self.progeny(child))

                        match conditions[(current, child)]:
                            case Equal(Special.Hash, 0) | Equal(0, Special.Hash):
                                nil = path

                            case Less(1, 1, Special.Hash):
                                non_nil = path

                            case _:
                                raise InvalidNode(current.directive)

                    match nil, non_nil:
                        case "", "":
                            return program

                        case nil, "":
                            return program + f"~#[{nil}~]"

                        case "", non_nil:
                            return program + f"~^{non_nil}"

                        case nil, non_nil:
                            return program + f"~#[{nil}~:;{non_nil}"

                case "[" if current.directive.get_param(0) == Special.Hash:
                    cases = {}
                    for child in self[current]:
                        # Terrible no good very bad
                        descendants = set(sum(nx.all_simple_paths(self, child, current.closing), [])) - {current.closing}
                        path = str(self.subgraph(descendants))

                        for descendant in descendants:
                            if descendant in nodes:
                                nodes.remove(descendant)

                        match conditions[(current, child)]:
                            case Equal(Special.Hash, a) | Equal(a, Special.Hash):
                                cases[a] = path

                            case Less(a, b, Special.Hash):
                                cases[-1] = path

                            case _:
                                raise InvalidNode(current.directive)

                    program += f"~#[{'~;'.join(cases.get(n, '') for n in range(max(cases) + 1))}"
                    if cases.get(-1):
                        program += f"~:;{cases[-1]}"

                case _:
                    program += str(current.directive)

        return program

    def add_edge(self, u: Node, v: Node, **attrs):
        super().add_edge(u, v, condition=attrs.get("condition", Condition()), back=attrs.get("back", False))

    def crash_on(self, node: Node):
        self.remove_edges_from([*self.edges(node)])
        self.add_edge(node, crash := Node(Directive("?", [])))
        self.add_edge(crash, CFG.CRASH)

    def descendants(self, node: Node) -> set[Node]:
        return nx.descendants(self, node) | {node}

    def draw(self, *, size: int = 12):
        plt.figure(1, figsize=(size, size))
        pos = nx.kamada_kawai_layout(self)

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

        conditions = nx.get_edge_attributes(self, "condition")
        nx.draw_networkx_edges(self, pos=pos, node_size=1300, arrowstyle="]->")
        nx.draw_networkx_edge_labels(self, pos=pos, edge_labels=conditions)

        plt.show()

    def progeny(self, node: Node) -> 'CFG':
        return self.subgraph(self.descendants(node))

    def reachable(self) -> 'CFG':
        return self.progeny(CFG.START)

    def subgraph(self, nodes) -> 'CFG':
        subgraph = super().subgraph(nodes).to_directed()
        subgraph.__class__ = CFG
        return subgraph

    def terminates_from(self, node: Node) -> bool:
        return bool({CFG.CRASH, CFG.END} & self.descendants(node))

    def update_pointers(self):
        # TODO: Use the logger
        for node in self:
            if node.kind != "ctrl":
                node.pointer = node.pointer.clear()

        for node in self:
            for child in self[node]:
                if child.kind == "ctrl":
                    continue

                child.pointer |= self[node][child]["condition"].enforce(node.pointer).step(child)
                print(f"Updated {str(child):10}{child.pointer}")
