import random as rand
import subprocess


def gen_operand():
    option = rand.randint(0, 2)
    if option <= 1:
        sig_bytes = int(round(rand.normalvariate(40, 50))) % 256
        bytes = rand.choices(range(256), k=sig_bytes)
    if option == 1:
        bytes.extend([0] * (256 - sig_bytes))
    if option == 2:
        bytes = rand.choices(range(256), k=256)

    n = 0
    for byte in bytes:
        n <<= 8
        n |= byte

    hex_n = "{:X}".format(n)
    assert len(hex_n) <= 512
    return n, hex_n

def gen_op():
    return rand.choice([
        (int.__add__, '+'),
        (int.__sub__, '-'),
        (int.__mul__, '*'),
        (int.__mod__, '%'),
    ])

def run_command(cmd, input=None):
    res = subprocess.run(cmd.split(' '), input=input, capture_output=True)
    if res.returncode != 0:
        print("Failed at", input)
        print("Process error:")
        for line in res.stderr.decode().split('\n'):
            print(line)
        exit(-1)
    return res

res = run_command("cargo build --release --example calc")

for i in range(20000):
    a, hex_a = gen_operand()
    b, hex_b = gen_operand()
    op, str_op = gen_op()

    if b == 0 and str_op == '%':
        continue

    expected = op(a, b) % (1 << 2048)
    input = "{} {} {}".format(hex_a, str_op, hex_b)
    res = run_command("target/release/examples/calc", input=input.encode())

    actual = bytes.decode(res.stdout).split("> ")
    actual = "".join(actual).split('\n')
    actual = "".join(actual)

    if expected != int(actual, 16):
        print("{} == {} but got 0x{}".format(input, hex(expected), actual))
        assert False

    if i % 200 == 0:
        print(i, "/ 20000 OK")

