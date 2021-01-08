export const reduceBalance = (balance, prec=3) => {
  if (balance) {
    if (balance.decimal) balance=balance.decimal;
    if (parseFloat(balance) % 1 === 0) return parseInt(balance)
    return Math.trunc(parseFloat(balance) * Math.pow(10, prec)) / Math.pow(10, prec);
  }
};

export const keepDecimal = decimal => {
  const num = decimal.toString().indexOf('.') === -1 ? `${decimal}.0` : decimal
  return num
}

export const gasUnit = decimal => {
  return decimal.toFixed(12)
}

export const extractDecimal = num => {
  if (num.decimal) return num.decimal;
  else return num;
}
