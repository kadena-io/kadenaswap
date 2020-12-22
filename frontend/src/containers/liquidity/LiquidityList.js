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
import pairTokens from '../../constants/pairTokens';
import TokenPair from './TokenPair';
import {PactContext} from '../../contexts/PactContext'
import reduceBalance from '../../utils/reduceBalance';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const RightContainer = styled.div`
  display: flex;
  align-items: center;

  & > *:first-child {
    margin-right: 13px;
  }

  & > *:not(:first-child):not(:last-child) {
    margin-right: 14px;
  }
  @media (min-width: ${({ theme: { mediaQueries } }) => mediaQueries.mobileBreakpoint}) {
    & > *:not(:first-child):not(:last-child) {
      margin-right: 16px;
    }
  }
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
      <RightContainer>
        <Button
           buttonStyle={{ marginLeft: 24 }}
           onClick={() => props.selectCreatePair()}>
           Create a pair
        </Button>
        <Button
           onClick={() => props.selectAddLiquidity()}>
           Add Liquidity
        </Button>
      </RightContainer>
      {pairTokens.map(pair => {
        return (
          <TokenPair
            account={pact.account.balance}
            pair = {pair}
            selectAddLiquidity = {props.selectAddLiquidity}
            selectRemoveLiquidity = {props.selectRemoveLiquidity}
          />
        )
      })}
    </FormContainer>
  );
};

export default LiquidityList;
