def fib():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b


if __name__ == "__main__":
    total = 0

    for i in fib():
        if i % 2 == 0:
            total += i

        if i > 4000000:
            break

    print(total)
