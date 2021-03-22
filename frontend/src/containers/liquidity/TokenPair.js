import React, { useState, useContext, useEffect } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import FormContainer from '../../components/shared/FormContainer';
import Search from '../../components/shared/Search';
import Backdrop from '../../components/shared/Backdrop';
import { List, Message, Button} from 'semantic-ui-react'
import { ReactComponent as KadenaIcon } from '../../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as ArrowDown } from '../../assets/images/shared/arrow-down.svg';
import {PactContext} from '../../contexts/PactContext'
import {reduceBalance, extractDecimal, pairUnit} from '../../utils/reduceBalance';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const TokenPair = (props) => {
  let pact = useContext(PactContext);
  let {name, token0, token1, balance, supply, pooledAmount} = props.pair;

  return (
    balance ?
          <List>
             <Message>
               <List.Item>
                <List.Content>
                  <List.Header>
                  <KadenaIcon/>
                  {` ${token0} / ${token1}`}
                  </List.Header>
                </List.Content>
              </List.Item>
              <br/>
              <List.Item>
                {`Your pool tokens: ${pairUnit(extractDecimal(balance))}`}
                <List.Content>
                Pooled {token0}: {pairUnit(extractDecimal(pooledAmount[0]))}
                </List.Content>
                <List.Content>
                Pooled {token1}: {pairUnit(extractDecimal(pooledAmount[1]))}
                </List.Content>
                <List.Content>
                {`Your pool share: ${reduceBalance(extractDecimal(balance)/extractDecimal(supply)*100)}%`}
                </List.Content>
              </List.Item>
              <br/>
              <Container>
                <Button
                  onClick={() => props.selectAddLiquidity()}>
                  Add
                </Button>
                <Button
                  onClick={() => {
                    props.selectRemoveLiquidity()
                    props.setTokenPair(props.pair)
                  }}>
                  Remove
                </Button>
              </Container>
            </Message>
        </List>
        : ""
  );
};

export default TokenPair;
