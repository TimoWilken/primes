# Primes utility

This program can generate primes, check numbers for prime-ness, or filter a list
of numbers, passing only primes through. All input is on `stdin`.

## Try it

```{sh}
$ ghc -O2 -dynamic primes
$ ./primes 20
```

## Usage

```
Usage: primes [--check | [--stdin] [NUM_PRIMES]]

  -h, --help  print this message and exit
  --check     check all numbers given on stdin for primeness
  --stdin     reads numbers from stdin; copies primes to stdout
  NUM_PRIMES  only print the first NUM_PRIMES primes;
              if not given, output is potentially unbounded

If --check is given, the exit code 0 indicates all numbers were
prime; 1 indicates that at least one non-prime was passed in.
```
