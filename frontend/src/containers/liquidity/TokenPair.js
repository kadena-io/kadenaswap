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
  let pair = {to: "ABC", from: "KDA"}

  useEffect( async () => {
     pact.getPairAccountBalance("coin", "free.abc", pact.account.account);
     pact.getTotalTokenSupply("coin", "free.abc");
  }, []);

  return (
          pact.pairAccountBalance!==null ?
          <List>
             <Message
             key={pair.from +pair.to}
             >
               <List.Item>
                <List.Content>
                  <List.Header>
                  <KadenaIcon/>
                  {`${pair.from} / ${pair.to}`}
                  </List.Header>
                </List.Content>
              </List.Item>
              <br/>
              <List.Item>
                {`Your pool tokens: ${reduceBalance(pact.pairAccountBalance)}`}
                <List.Content>
                Pooled {pair.from}: {reduceBalance(pact.poolBalance[0])}
                </List.Content>
                <List.Content>
                Pooled {pair.to}: {reduceBalance(pact.poolBalance[1])}
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
        :<Message>No Liquidity Found</Message>

  );
};

export default TokenPair;
