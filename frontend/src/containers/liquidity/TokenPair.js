import React, { useState, useContext, useEffect } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import FormContainer from '../../components/shared/FormContainer';
import Search from '../../components/shared/Search';
import Backdrop from '../../components/shared/Backdrop';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import { List, Message, Button} from 'semantic-ui-react'
import { ReactComponent as KadenaIcon } from '../../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as ArrowDown } from '../../assets/images/shared/arrow-down.svg';
import {PactContext} from '../../contexts/PactContext'
import reduceBalance from '../../utils/reduceBalance';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const TokenPair = (props) => {
  let pact = useContext(PactContext);
  const [pairBalance, setPairBalance] = useState(pact.pairAccountBalance);
  let {name, token0, token1} = props.pair;

  useEffect( async () => {
    pact.setVerifiedAccount(pact.account.account);
    pact.getPairAccountBalance(token0.name, token1.name, pact.account.account);
    pact.getTotalTokenSupply(token0.name, token1.name);
    pact.getPooledAmount(name, token0.name, token1.name, pact.account.account);
  }, []);

  console.log(pact.totalSupply)
  
  return (
          pact.pairAccountBalance!==null ?
          <List>
             <Message
             key={token0.code +token1.code}
             >
               <List.Item>
                <List.Content>
                  <List.Header>
                  <KadenaIcon/>
                  {` ${token0.code} / ${token1.code}`}
                  </List.Header>
                </List.Content>
              </List.Item>
              <br/>
              <List.Item>
                {`Your pool tokens: ${reduceBalance(pact.pairAccountBalance)}`}
                <List.Content>
                Pooled {token0.code}: {reduceBalance(pact.poolBalance[0])}
                </List.Content>
                <List.Content>
                Pooled {token1.code}: {reduceBalance(pact.poolBalance[1])}
                </List.Content>
                <List.Content>
                {`Your pool share: ${reduceBalance(pact.pairAccountBalance/pact.totalSupply*100)}%`}
                </List.Content>
              </List.Item>
              <br/>
              <Container>
                <Button
                  onClick={() => props.selectAddLiquidity()}>
                  Add
                </Button>
                <Button
                  onClick={() => props.selectRemoveLiquidity()}>
                  Remove
                </Button>
              </Container>
            </Message>

        </List>
        :<Message>Connect an account to view your liquidity</Message>

  );
};

export default TokenPair;
