sum_aux(n, sum) {
  if ( n <= 0 ) {
    sum
  } else {
    sum_aux(n - 1, sum + n)
  }
}

sum(n) {
  sum_aux(n, 0)
}

main() {
  sum(10000)
}
