from importlib import import_module

COMMANDS = [
    "fourmat",
    "fivemat",
    "sixmat",

    "hazmat",
]

def register_commands(subparsers):
    for command in COMMANDS:
        module = import_module(f"{__name__}.{command}")
        module.register(subparsers)
