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

describe(".compress_range", {
  it("compreses simple range correctly", {
    expect_equal(.compress_range(c(1, 2, 3, 4)), "1:4")
    expect_equal(.compress_range(c(1, 5, 10, 15)), "1,5,10,15")
  })

  it("compreses range with breaks correctly", {
    expect_equal(.compress_range(c(1, 2, 3, 4, 5, 10, 11, 12, 15)), "1:5,10:12,15")
  })

  it("handles unsorted vectors correctly", {
    expect_equal(.compress_range(c(15, 1, 11, 4, 5, 10, 2, 12, 3)), "1:5,10:12,15")
  })

  it("coerces character vectors to numeric if possible", {
    expect_equal(.compress_range(c("1", "2", "03")), "1:3")
  })

  it("returns NA when empty vector is provided", {
    expect_true(is.na(.compress_range(c())))
  })

  it("throws an error if any values in the vector cannot be coerced to numeric", {
    expect_error(.compress_range(c(1, 2, "A", 4)), "Error: only numeric values allowed")
  })
})