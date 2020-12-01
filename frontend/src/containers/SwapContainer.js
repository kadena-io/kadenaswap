import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as SwapArrowsIcon } from '../assets/images/shared/swap-arrows.svg';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import ButtonDivider from '../components/shared/ButtonDivider';
import Button from '../components/shared/Button';
import cryptoCurrencies from '../constants/cryptoCurrencies';
import TokenSelector from '../components/shared/TokenSelector';
import { PactContext } from '../contexts/PactContext';

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

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
`;

const SwapContainer = () => {
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const [fromValues, setFromValues] = useState({ amount: '', balance: '', coin: '' });
  const [toValues, setToValues] = useState({ amount: '', balance: '', coin: '' });

  const pact = useContext(PactContext);

  useEffect(() => {
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType, fromValues, toValues]);

  const swapValues = () => {
    const from = { ...fromValues };
    const to = { ...toValues };
    setFromValues({ ...to });
    setToValues({ ...from });
  };

  const onTokenClick = ({ crypto }) => {
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, balance: 123, coin: crypto.code }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, balance: 123, coin: crypto.code }));
  };

  const getButtonLabel = () => {
    if (!pact.account.account) return 'Connect your KDA account';
    if (!fromValues.amount || !toValues.amount) return 'Enter an amount';
    if (!fromValues.coin || !toValues.coin) return 'Select tokens';
    return 'SWAP';
  };

  return (
    <Container>
      <TokenSelector
        show={tokenSelectorType !== null}
        selectedToken={selectedToken}
        onTokenClick={onTokenClick}
        onClose={() => setTokenSelectorType(null)}
      />
      <FormContainer title="swap">
        <Input
          leftLabel="from"
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
        <ButtonDivider icon={<SwapArrowsIcon />} onClick={swapValues} />
        <Input
          leftLabel="to"
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
        {fromValues.amount && fromValues.coin && toValues.amount && toValues.coin && (
          <>
            <RowContainer>
              <Label>price</Label>
              <span>0.0000956556 ETH per 0xMR</span>
            </RowContainer>
            <RowContainer style={{ margin: 0 }}>
              <Label>price impact</Label>
              <span>15.16%</span>
            </RowContainer>
          </>
        )}
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={!fromValues.amount || !fromValues.coin || !toValues.amount || !toValues.coin}
          onClick={() => console.log('SWAPPED')}
        >
          {getButtonLabel()}
        </Button>
      </FormContainer>
    </Container>
  );
};

export default SwapContainer;
