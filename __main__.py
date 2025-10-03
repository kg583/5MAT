import argparse

from cli import register_commands


def main():
    parser = argparse.ArgumentParser(prog='nmat')
    subparsers = parser.add_subparsers()
    register_commands(subparsers)

    args, unknown = parser.parse_known_args()

    if hasattr(args, "func"):
        args.func(args, unknown)
    else:
        parser.print_help()


if __name__ == '__main__':
    main()
