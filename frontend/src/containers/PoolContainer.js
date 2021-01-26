import React, { useState, useContext } from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import ButtonDivider from '../components/shared/ButtonDivider';
import Button from '../components/shared/Button';
import cryptoCurrencies from '../constants/tokens';
import TokenSelector from '../components/shared/TokenSelector';
import LiquidityContainer from './liquidity/LiquidityContainer';
import RemoveLiquidityContainer from './liquidity/RemoveLiquidityContainer';
import LiquidityList from './liquidity/LiquidityList';
import { PactContext } from '../contexts/PactContext'

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
  const [selectedView, setSelectedView] = useState(false);
  const [pair, setPair] = useState(null);
  const pact = useContext(PactContext);

  return (
    <Container>
      {
        selectedView==="Remove Liquidity"
        ? <RemoveLiquidityContainer
            closeLiquidity = {() => setSelectedView(false)}
            selectedView = { selectedView }
            pair={pair}
          />
        : selectedView
        ?
        <LiquidityContainer
            closeLiquidity = {() => setSelectedView(false)}
            selectedView = { selectedView }
            setSelectedView = { setSelectedView }
             pair={pair}
            />
        : <LiquidityList
            selectCreatePair = {() => setSelectedView("Create A Pair")}
            selectAddLiquidity = {() => setSelectedView("Add Liquidity")}
            selectRemoveLiquidity = {() => setSelectedView("Remove Liquidity")}
            setTokenPair = {(pair) => setPair(pair)}
          />
      }
    </Container>
  );
};

export default PoolContainer;
