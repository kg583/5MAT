import unittest

from io import StringIO
from pathlib import Path

from lib.fivemat.evaluate import *
from lib.sixmat.assembler import *


class SampleTests(unittest.TestCase):
    def sample(self, sample: str, expected: str, input: str = "", *,
               max_lifetimes: int = None, length: int = None, sixmat: bool = True):
        top = Path(__file__).parents[2]
        programs = [top.joinpath(f"docs/samples/5MAT/{sample}.5mat").read_text(encoding="utf8")]

        if sixmat:
            programs.append(assemble(top.joinpath(f"docs/samples/6MAT/{sample}.6mat").read_text(encoding="utf8")))

        for program in programs:
            buffer = StringIO()
            fivemat(program, max_lifetimes=max_lifetimes, input_stream=StringIO(input), output_stream=buffer)

            buffer.seek(0)
            self.assertEqual(expected.rstrip(), buffer.read(length).rstrip())

    def test_12_days_of_christmas(self):
        self.sample("12-days-of-christmas", "On the First day of Christmas\nMy true love sent to me",
                    length=54, sixmat=False)

    def test_bct(self):
        self.sample("bct", "101\n01\n1\n11\n110\n10\n101\n1010\n010\n010\n010\n10",
                    max_lifetimes=12)

    def test_counter(self):
        self.sample("counter", "0\n1\n2\n3\n4\n5\n6\n7\n8",
                    max_lifetimes=10)

    def test_echo(self):
        self.sample("echo", "Echo!", "Echo!",
                    sixmat=False)
        self.sample("echo", "New\nline", "New\nline",
                    sixmat=False)

    def test_fibonacci(self):
        self.sample("fibonacci", "1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233",
                    max_lifetimes=1000)

    def test_fizzbuzz(self):
        self.sample("fizzbuzz", "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz",
                    max_lifetimes=16)

    def test_hello_world(self):
        self.sample("hello-world", "Hello, World!")

    def test_is_palindrome(self):
        self.sample("is-palindrome", "True", "RacecaR")
        self.sample("is-palindrome", "False", "palindrome")

    def test_sort(self):
        self.sample("sort", "0356789", "8675309")
        self.sample("sort", "  5AGMTaeirst", "5MAT is Great")

    def test_truth_machine(self):
        self.sample("truth-machine", "0", "0",
                    sixmat=False)
        self.sample("truth-machine", "11111", "1",
                    max_lifetimes=5, sixmat=False)
