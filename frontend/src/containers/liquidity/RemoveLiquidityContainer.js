import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as PlusIcon } from '../../assets/images/shared/plus.svg';
import FormContainer from '../../components/shared/FormContainer';
import InputToken from '../../components/shared/InputToken';
import ButtonDivider from '../../components/shared/ButtonDivider';
import {default as StyledButton} from '../../components/shared/Button';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import TokenSelector from '../../components/shared/TokenSelector';
import { Header, Input, Button, List, Statistic } from 'semantic-ui-react'
import TxView from '../../components/shared/TxView';
import { PactContext } from '../../contexts/PactContext'
import { ReactComponent as LeftIcon } from '../../assets/images/shared/left-arrow.svg';
import reduceBalance from '../../utils/reduceBalance';

const Container = styled.div`
  margin: 15px 0px;
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
  const liquidityView = props.selectedView;
  const [fromValues, setFromValues] = useState({ amount: 0, coin: cryptoCurrencies.KDA.code });
  const [toValues, setToValues] = useState({ amount: 0,  coin:cryptoCurrencies.ABC.code });
  const [amount, setAmount] = useState("100")
  const [showTxModal, setShowTxModal] = useState(false)
  const [loading, setLoading] = useState(false)
  const {name, token0, token1, balance, supply, pooledAmount} = props.pair

  return (
      <FormContainer title={liquidityView}>
        <TxView
          view="removeLiquidity"
          show={showTxModal}
          token0={token0.code}
          token1={token1.code}
          onClose={() => setShowTxModal(false)}
        />
        <LeftIcon style={{ cursor: 'pointer', position: 'absolute', width:20, height: 30, top: 14, left: 14 }} onClick={() => props.closeLiquidity()} />
        <Header>Pool Tokens to Remove</Header>
        <Input
          value={amount}
          onChange={(e) => {
            if (Number(e.target.value)<=100 && Number(e.target.value)>=0){
              setAmount(e.target.value)
            }
          }}
          label={{ basic: true, content: '%' }}
          labelPosition='right'
          placeholder='Enter Amount to Remove'
         />
         <Container>
           <Button onClick={() => setAmount(25)} >25%</Button>
           <Button onClick={() => setAmount(50)} >50%</Button>
           <Button onClick={() => setAmount(75)} >75%</Button>
           <Button onClick={() => setAmount(100)} >100%</Button>
         </Container>
        <Statistic>
          <Statistic.Value></Statistic.Value>
          <Statistic.Label>{`${token0.code} / ${token1.code} Pool Tokens`}</Statistic.Label>
        </Statistic>
        <List>
          <List.Item>{`${token0.code} / ${token1.code}: ${reduceBalance(reduceBalance(balance)*amount/100)}`}</List.Item>
          <List.Item>{`Pooled ${token0.code}: ${reduceBalance(reduceBalance(pooledAmount[0])*amount/100)}`}</List.Item>
          <List.Item>{`Pooled ${token1.code}: ${reduceBalance(reduceBalance(pooledAmount[1])*amount/100)}`}</List.Item>
        </List>
        <StyledButton
          loading={loading}
          onClick={async () => {
              setLoading(true)
              await pact.removeLiquidityLocal(token0.name, token1.name, reduceBalance(balance)*amount/100);
              setLoading(false)
              setShowTxModal(true)
          }
        }>
          Remove Liquidity
        </StyledButton>
      </FormContainer>
  );
};

export default RemoveLiquidityContainer;
