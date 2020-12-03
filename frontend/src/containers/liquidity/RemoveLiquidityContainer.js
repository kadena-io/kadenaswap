import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as PlusIcon } from '../../assets/images/shared/plus.svg';
import FormContainer from '../../components/shared/FormContainer';
import InputToken from '../../components/shared/InputToken';
import ButtonDivider from '../../components/shared/ButtonDivider';
import Button from '../../components/shared/Button';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import TokenSelector from '../../components/shared/TokenSelector';
import PactWallet from './PactWallet';
import { Header, Input } from 'semantic-ui-react'
import { Statistic } from 'semantic-ui-react'
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

const RemoveLiquidityContainer = (props) => {
  const pact = useContext(PactContext);
  const [tokenSelectorType, setTokenSelectorType] = useState(null);
  const [selectedToken, setSelectedToken] = useState(null);
  const [fromValues, setFromValues] = useState({ amount: 0, balance: pact.account.balance, coin: cryptoCurrencies.KDA.code });
  const [toValues, setToValues] = useState({ amount: 0, balance: pact.tokenAccount.balance, coin:cryptoCurrencies.ABC.code });
  const liquidityView = props.selectedView;
  const [amount, setAmount] = useState(0)

  useEffect(() => {
    if (tokenSelectorType === 'from') return setSelectedToken(fromValues.coin);
    if (tokenSelectorType === 'to') return setSelectedToken(toValues.coin);
    return setSelectedToken(null);
  }, [tokenSelectorType, fromValues, toValues]);

  const onTokenClick = ({ crypto }) => {
    if (tokenSelectorType === 'from') setFromValues((prev) => ({ ...prev, coin: crypto.code }));
    if (tokenSelectorType === 'to') setToValues((prev) => ({ ...prev, coin: crypto.code }));
  };

  return (
      <FormContainer title={liquidityView}>
      <LeftIcon style={{ cursor: 'pointer', position: 'absolute', width:20, height: 30, top: 14, left: 14 }} onClick={() => props.closeLiquidity()} />
      <Header>Amount</Header>
      <Input
        onChange={(e) => setAmount(e.target.value)}
       />
      <Statistic>
        <Statistic.Value></Statistic.Value>
        <Statistic.Label>{`${fromValues.coin} / ${toValues.coin} Pool Tokens`}</Statistic.Label>
      </Statistic>
      <Button color='black'
        onClick={async () => {
          console.log(amount)
            pact.removeLiquidity(pact.account.account, "coin", "abc", amount);
        }
      }>
        Remove Liquidity
      </Button>
      </FormContainer>
  );
};

export default RemoveLiquidityContainer;
