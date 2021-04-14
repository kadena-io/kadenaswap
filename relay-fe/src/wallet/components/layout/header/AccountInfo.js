import React from 'react';
import styled from 'styled-components/macro';

const Container = styled.div`
  display: flex;
  border-color: 'pink';
  :hover {
    opacity: 0.7;
    cursor: pointer;
  }
`;

const BalanceContainer = styled.div`
  padding: 10px 21px;
  background: rgba(205, 205, 205, 0.5);
  font-size: 14px;
  color: white;
  border-radius: 4px;
`;

const AccountContainer = styled.div`
  padding: 10px 21px;
  background: white;
  font-size: 14px;
  color: ${({ theme: { colors } }) => colors.pink};
  border-radius: 4px;
  margin-left: -10px;
  z-index: 2;
`;

const CoinInfo = ({ account, balance, onClick }) => {
  return (
    <Container onClick={onClick}>
      <BalanceContainer>{balance}</BalanceContainer>
      <AccountContainer>{account}</AccountContainer>
    </Container>
  );
};

export default CoinInfo;
