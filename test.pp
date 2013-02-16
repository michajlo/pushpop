my_fun(a, b, c) {
    let x := a;
    let y := b;
    c
}
main() {
    let v := "10";
    let v2 := 10;
    my_fun({let x := 1; x}, v, 3) + 1 + { v2 + 1 }
}
