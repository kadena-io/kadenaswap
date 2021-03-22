export const reduceBalance = (balance, prec=6) => {
  if (balance) {
    if (balance.int) balance=balance.int;
    if (balance.decimal) balance=balance.decimal;
    if (parseFloat(balance) % 1 === 0) {return parseInt(balance)}
    return Math.trunc(parseFloat(balance) * Math.pow(10, prec)) / Math.pow(10, prec);
  }
  if (balance===0) return 0;
};

export const keepDecimal = decimal => {
  const num = decimal.toString().indexOf('.') === -1 ? `${decimal}.0` : decimal
  return num
}

export const gasUnit = decimal => {
  return decimal.toFixed(12);
}

export const pairUnit = decimal => {
  return Number(decimal).toFixed(14);
}

export const extractDecimal = num => {
  if (num.int) return num.int;
  if (num.decimal) return num.decimal;
  else return num;
}

export const  limitDecimalPlaces = (numStr, count) => {
  if (numStr.indexOf('.') == -1) {
    if (numStr === '') return '';
    if (!isNaN(numStr)) return Number(numStr);
  }
  if (numStr.indexOf('.') === numStr.length-1
      && !isNaN(numStr.slice(0,numStr.length-1))
    ) {
    return numStr;
  }
  if ((numStr.length - numStr.indexOf('.')) > count) {
    numStr = parseFloat(numStr).toFixed(count);
    return numStr;
  }
  else {
    return numStr;
  }
}
