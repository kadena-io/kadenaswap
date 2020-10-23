import React, { useEffect, useState } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as PlusIcon } from '../assets/images/shared/plus.svg';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import ButtonDivider from '../components/shared/ButtonDivider';
import Button from '../components/shared/Button';
import cryptoCurrencies from '../constants/cryptoCurrencies';
import TokenSelector from '../components/shared/TokenSelector';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const RowContainer = styled.div`
  display: flex;
  justify-content: space-between;
  margin: 15px 0px;
`;

const ColumnContainer = styled.div`
  display: flex;
  flex-flow: column;
  align-items: center;

  & > span:first-child {
    font-size: 14px;
    margin-bottom: 3px;
  }

  & > span:last-child {
    font-size: 12px;
    color: #b5b5b5;
  }
`;

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
  margin: 15px 0;
`;

const PoolContainer = () => {
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const [fromValues, setFromValues] = useState({ amount: '', balance: '', coin: cryptoCurrencies.ETH.code });
  const [toValues, setToValues] = useState({ amount: '', balance: '', coin: '' });

  useEffect(() => {
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType, fromValues, toValues]);

  const onTokenClick = ({ crypto }) => {
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, balance: 123, coin: crypto.code }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, balance: 123, coin: crypto.code }));
  };

  return (
    <Container>
      <TokenSelector
        show={tokenSelectorType !== null}
        selectedToken={selectedToken}
        onTokenClick={onTokenClick}
        onClose={() => setTokenSelectorType(null)}
      />
      <FormContainer title="pool">
        <Input
          leftLabel="input"
          rightLabel={`balance: ${fromValues.balance ?? '-'}`}
          placeholder="enter amount"
          inputRightComponent={
            fromValues.coin ? (
              <InputToken
                icon={cryptoCurrencies[fromValues.coin].icon}
                code={cryptoCurrencies[fromValues.coin].code}
                onClick={() => setTokenSelectorType('from')}
              />
            ) : null
          }
          withSelectButton
          numberOnly
          value={fromValues.amount}
          onSelectButtonClick={() => setTokenSelectorType('from')}
          onChange={(e, { value }) => setFromValues((prev) => ({ ...prev, amount: value }))}
        />
        <ButtonDivider icon={<PlusIcon />} buttonStyle={{ cursor: 'default' }} />
        <Input
          leftLabel="input"
          rightLabel={`balance: ${toValues.balance ?? '-'}`}
          placeholder="enter amount"
          inputRightComponent={
            toValues.coin ? (
              <InputToken
                icon={cryptoCurrencies[toValues.coin].icon}
                code={cryptoCurrencies[toValues.coin].code}
                onClick={() => setTokenSelectorType('to')}
              />
            ) : null
          }
          withSelectButton
          numberOnly
          value={toValues.amount}
          onSelectButtonClick={() => setTokenSelectorType('to')}
          onChange={(e, { value }) => setToValues((prev) => ({ ...prev, amount: value }))}
        />
        {fromValues.coin && toValues.coin && (
          <>
            <Label>price and pool share</Label>
            <RowContainer style={{ marginBottom: 0 }}>
              <ColumnContainer>
                <span>12785.1</span>
                <span>0xMR per ETH</span>
              </ColumnContainer>
              <ColumnContainer>
                <span>0.0000782163</span>
                <span>ETH per 0xMR</span>
              </ColumnContainer>
              <ColumnContainer>
                <span>15.46%</span>
                <span>Share of Pool</span>
              </ColumnContainer>
            </RowContainer>
          </>
        )}
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={!fromValues.amount || !fromValues.coin || !toValues.amount || !toValues.coin}
          onClick={() => console.log('POOL')}
        >
          Insufficient 0xMR Balance
        </Button>
      </FormContainer>
    </Container>
  );
};

export default PoolContainer;
