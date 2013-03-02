fib_aux(n, b, a) {
  if (n <= 0) {
    a
  } else {
    // since this is a tail call it may
    // be optimized to reuse the stack
    // frame
    fib_aux(n - 1, a + b, b)
  }
}

fib(n) {
  fib_aux(n, 1, 0)
}

// program begins execution in main
main() {
  fib(1000)
}
