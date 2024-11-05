describe(".eval_range", {
  it("should generate simple range correctly", {
    expect_equal(.eval_range("1:5"), c(1, 2, 3, 4, 5))
    expect_equal(.eval_range("4:6"), c(4, 5, 6))
  })
  it("should parse sequences of singular numbers correctly", {
    expect_equal(.eval_range("1,5,6"), c(1, 5, 6))
    expect_equal(.eval_range("3,4,2"), c(3, 4, 2))
  })
  it("should parse mixed sequences correctly", {
    expect_equal(.eval_range("1:4,9"), c(1, 2, 3, 4, 9))
    expect_equal(.eval_range("5,3:1,15"), c(5, 3, 2, 1, 15))
  })
  it("should return NA when input is not parsed correctly", {
    expect_identical(.eval_range("test"), NA)
    expect_identical(.eval_range("1::33;"), NA)
  })
})