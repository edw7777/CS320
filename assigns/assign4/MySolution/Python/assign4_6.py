def natural_numbers():
    n = 0
    while True:
        yield n
        n += 1

def theNatPairs_cubesum():
    stream_i = natural_numbers()
    stream_j = natural_numbers()

    for i in stream_i:
        for j in stream_j:
            if (i**3 + j**3) < ((i+1)**3 + (j+1)**3):
                yield (i, j)

# Example usage:
nat_pair_stream = theNatPairs_cubesum()
for _ in range(10):
    print(next(nat_pair_stream))