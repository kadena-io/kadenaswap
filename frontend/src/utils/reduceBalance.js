export default (balance) => {
  if (balance) {
    if (balance.decimal) balance=balance.decimal;
    if (parseFloat(balance) % 1 === 0) return parseInt(balance)
    return parseFloat(balance).toFixed(3);
  }
};
