if __name__ == "__main__":
    num3 = set(range(3, 1000, 3))
    num5 = set(range(5, 1000, 5))
    ans = sum(num3.union(num5))

    print(ans)
