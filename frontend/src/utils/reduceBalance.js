export default (balance) => {
  if (balance) {
    if (balance.decimal) balance=balance.decimal;
    if (parseFloat(balance) % 1 === 0) return parseInt(balance)
    return (Math.floor(parseFloat(balance) * 1000) / 1000).toFixed(3);
  }
};
