fib_aux(n, b, a) {
  if (n <= 0) {
    a
  } else {
    fib_aux(n - 1, a + b, b)
  }
}

fib(n) {
  fib_aux(n, 1, 0)
}

main() {
  fib(1000)
}
