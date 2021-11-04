messageReference <- function(x) {
  sprintf("%s-%d", "ABC", sample(seq.int(1e5, 1e6), size = 1L))
}