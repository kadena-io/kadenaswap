import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as PlusIcon } from '../../assets/images/shared/plus.svg';
import FormContainer from '../../components/shared/FormContainer';
import Input from '../../components/shared/Input';
import InputToken from '../../components/shared/InputToken';
import ButtonDivider from '../../components/shared/ButtonDivider';
import Button from '../../components/shared/Button';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import TokenSelector from '../../components/shared/TokenSelector';
import PactWallet from './PactWallet';
import { PactContext } from '../../contexts/PactContext'
import { ReactComponent as LeftIcon } from '../../assets/images/shared/left-arrow.svg';

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

const LiquidityContainer = (props) => {
  const pact = useContext(PactContext);
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const [fromValues, setFromValues] = useState({ amount: 0, balance: pact.account.balance, coin: cryptoCurrencies.KDA.code });
  const [toValues, setToValues] = useState({ amount: 0, balance: pact.tokenAccount.balance, coin:cryptoCurrencies.SIL.code });
  const liquidityView = props.selectedView;

  useEffect(() => {
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType, fromValues, toValues]);

  const onTokenClick = ({ crypto }) => {
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, coin: crypto.code }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, coin: crypto.code }));
  };

  const setTokenAmount = (amount1, amount2) => {
    let ratio;
    if (amount1) {
      ratio = pact.getRatio(fromValues.coin, toValues.coin)
      setFromValues((prev) => ({ ...prev, amount: amount1 }));
      setToValues((prev) => ({ ...prev, amount: amount1/ratio }));
    } else if (amount2){
      ratio = pact.getRatio( toValues.coin, fromValues.coin)
      setToValues((prev) => ({ ...prev, amount: amount2}));
      setFromValues((prev) => ({ ...prev, amount: amount2/ratio}))
    }

  }

  return (
      <FormContainer title={liquidityView}>
        <TokenSelector
          show={tokenSelectorType !== null}
          selectedToken={selectedToken}
          onTokenClick={onTokenClick}
          onClose={() => setTokenSelectorType(null)}
        />
        <LeftIcon style={{ cursor: 'pointer', position: 'absolute', width:20, height: 30, top: 14, left: 14 }} onClick={() => props.closeLiquidity()} />
        <Input
          leftLabel="input"
          rightLabel={`balance: ${pact.account.balance ?? '-'}`}
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
          onChange={(e, { value }) => {
            setTokenAmount(value);
          }}
        />
        <ButtonDivider icon={<PlusIcon />} buttonStyle={{ cursor: 'default' }} />
        <Input
          leftLabel="input"
          rightLabel={`balance: ${pact.tokenAccount.balance ?? '-'}`}
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
          onChange={(e, { value }) => setTokenAmount(null, value)}
        />
        {fromValues.coin && toValues.coin && (
          <>
            <Label>price and pool share</Label>
            <RowContainer style={{ marginBottom: 0 }}>
              <ColumnContainer>
                <span>{pact.getRatio(toValues.coin,fromValues.coin)}</span>
                <span>{`${toValues.coin} per ${fromValues.coin}`}</span>
              </ColumnContainer>
              <ColumnContainer>
                <span>{pact.getRatio(fromValues.coin,toValues.coin)}</span>
                <span>{`${toValues.coin} per ${fromValues.coin}`}</span>
              </ColumnContainer>
              <ColumnContainer>
                <span>1%</span>
                <span>Share of Pool</span>
              </ColumnContainer>
            </RowContainer>
          </>
        )}
        <PactWallet
          toValues={toValues}
          fromValues={fromValues}
          liquidityView = {liquidityView}
        />
      </FormContainer>
  );
};

export default LiquidityContainer;
