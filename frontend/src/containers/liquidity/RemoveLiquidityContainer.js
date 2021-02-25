import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { ReactComponent as PlusIcon } from '../../assets/images/shared/plus.svg';
import FormContainer from '../../components/shared/FormContainer';
import InputToken from '../../components/shared/InputToken';
import ButtonDivider from '../../components/shared/ButtonDivider';
import {default as StyledButton} from '../../components/shared/Button';
import cryptoCurrencies from '../../constants/tokens';
import TokenSelector from '../../components/shared/TokenSelector';
import { Header, Input, Button, List, Statistic } from 'semantic-ui-react'
import TxView from '../../components/shared/TxView';
import { PactContext } from '../../contexts/PactContext'
import { ReactComponent as LeftIcon } from '../../assets/images/shared/left-arrow.svg';
import {reduceBalance, extractDecimal, limitDecimalPlaces} from '../../utils/reduceBalance';

const Container = styled.div`
  margin: 15px 0px;
  display: flex;
  justify-content: center;
  align-items: center;
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
  const {name, token0, token1, balance, supply, pooledAmount} = props.pair

  const [amount, setAmount] = useState(100)
  const [showTxModal, setShowTxModal] = useState(false)
  const [loading, setLoading] = useState(false)
  const [pooled, setPooled] = useState(balance);
  const [pooledToken0, setPooledToken0] = useState(reduceBalance(pooledAmount[0],12));
  const [pooledToken1, setPooledToken1] = useState(reduceBalance(pooledAmount[1],12));

  useEffect(() => {
    if (!isNaN(amount)){
      setPooled(reduceBalance(extractDecimal(balance) * amount / 100, pact.PRECISION));
      setPooledToken0(reduceBalance(extractDecimal(pooledAmount[0]) * amount / 100,  pact.PRECISION));
      setPooledToken1(reduceBalance(extractDecimal(pooledAmount[1]) * amount / 100,  pact.PRECISION));
    }
  }, [amount] )

  useEffect(() => {
    if (pact.walletSuccess) {
      setLoading(false)
      pact.setWalletSuccess(false)
    }
  }, [pact.walletSuccess])

  return (
      <FormContainer title={liquidityView}>
        <TxView
          view="Remove Liquidity"
          show={showTxModal}
          token0={token0}
          token1={token1}
          onClose={() => setShowTxModal(false)}
        />
        <LeftIcon style={{ cursor: 'pointer', position: 'absolute', width:20, height: 30, top: 14, left: 14 }} onClick={() => props.closeLiquidity()} />
        <Header>Pool Tokens to Remove</Header>
        <Input
          value={amount}
          error={isNaN(amount)}
          onChange={(e) => {
            if (Number(e.target.value)<=100 && Number(e.target.value)>=0){
              setAmount(limitDecimalPlaces(e.target.value, 2));
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
          <Statistic.Label>{`${token0} / ${token1} Pool Tokens`}</Statistic.Label>
        </Statistic>
        <List>
          <List.Item>{`${token0} / ${token1}: ${pooled}`}</List.Item>
          <List.Item>{`Pooled ${token0}: ${pooledToken0}`}</List.Item>
          <List.Item>{`Pooled ${token1}: ${pooledToken1}`}</List.Item>
        </List>
        <StyledButton
          loading={loading}
          disabled={isNaN(amount) || reduceBalance(amount)===0}
          onClick={async () => {
            if (pact.signing.method !== 'sign') {
              const res = await pact.removeLiquidityLocal(pact.tokenData[token0].code, pact.tokenData[token1].code, reduceBalance(pooled));
              if (res === -1) {
                setLoading(false)
                alert('Incorrect password. If forgotten, you can reset it with your private key')
                return
              } else {
                setShowTxModal(true)
                setLoading(false)
              }
            } else {
              pact.removeLiquidityWallet(pact.tokenData[token0].code, pact.tokenData[token1].code, reduceBalance(pooled, pact.PRECISION));
            }
          }
        }>
          Remove Liquidity
        </StyledButton>
      </FormContainer>
  );
};

export default RemoveLiquidityContainer;
