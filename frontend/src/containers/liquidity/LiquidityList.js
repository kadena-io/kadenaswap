import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { Message } from 'semantic-ui-react'
import { ReactComponent as PlusIcon } from '../../assets/images/shared/plus.svg';
import FormContainer from '../../components/shared/FormContainer';
import Input from '../../components/shared/Input';
import InputToken from '../../components/shared/InputToken';
import ButtonDivider from '../../components/shared/ButtonDivider';
import Button from '../../components/shared/Button';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import TokenPair from './TokenPair';
import PactWallet from './PactWallet';
import {PactContext} from '../../contexts/PactContext'

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

const LiquidityList = (props) => {
  const pact = useContext(PactContext);
  return (
    <FormContainer title="Your Liquidity">
      <Button
         buttonStyle={{ marginTop: 24, marginRight: 0 }}
         onClick={() => props.selectLiquidity()}>
         Add Liquidity
       </Button>
      <TokenPair
        account={pact.account.balance}
        pairList= {[{"from": "KDA", "to": "SIL"}]}
      />
    </FormContainer>
  );
};

export default LiquidityList;
