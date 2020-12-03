import React, { useState, useContext, useEffect } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import FormContainer from '../../components/shared/FormContainer';
import Search from '../../components/shared/Search';
import Backdrop from '../../components/shared/Backdrop';
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import { List, Message } from 'semantic-ui-react'
import { ReactComponent as KadenaIcon } from '../../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as ArrowDown } from '../../assets/images/shared/arrow-down.svg';
import {PactContext} from '../../contexts/PactContext'

const TokenPair = (props) => {
  let pact = useContext(PactContext);
  const [pairBalance, setPairBalance] = useState({ balance: null });
  let pair = {to: "KDA", from: "SIL"}
  useEffect(() => {
    pact.getPairAccountBalance("coin", "abc", pact.account.account);
  });

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
                  {pact.pairAccountBalance}
                </List.Content>
              </List.Item>
            </Message>
        </List>
        :<Message>No Liquidity Found</Message>

  );
};

export default TokenPair;
