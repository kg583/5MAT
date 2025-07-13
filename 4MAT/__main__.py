from src import *
import sys


def main():
    print(fivemat(open(sys.argv[-1], encoding="UTF-8").read()))

"""
arg options...
 - perf <path to 5MAT>
   for a performance trace
 - <path to 5MAT>
   just to run the file
"""
if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "perf":
        import cProfile
        import pstats

        cProfile.run("main()", filename="profile.pstats")
        pstats.Stats("profile.pstats").print_stats()
    else:
        main()