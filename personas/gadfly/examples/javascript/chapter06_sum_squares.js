// Approach 1: Imperative, mutable
function sumOfSquaresOfEvens(numbers) {
  let sum = 0;
  for (let i = 0; i < numbers.length; i++) {
    if (numbers[i] % 2 === 0) {
      sum += numbers[i] * numbers[i];
    }
  }
  return sum;
}

// Approach 2: Functional, immutable
function sumOfSquaresOfEvens(numbers) {
  return numbers
    .filter(n => n % 2 === 0)
    .map(n => n * n)
    .reduce((sum, square) => sum + square, 0);
}
