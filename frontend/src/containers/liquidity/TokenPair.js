import React, { useState, useContext } from 'react';
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
  pact.getPairAccountBalance("coin", "abc", pact.account.account)


  return (
          pact.supplied?
          <List>
            {  props.pairList.map(pair => {
             return (
               <Message
               key={pair.from +pair.to}
               >
                 <List.Item>
                  <List.Content>
                    <List.Header>

                      <KadenaIcon/>
                      {`        ${pair.from} / ${pair.to} `}
                      <ArrowDown width="200px" />
                    </List.Header>

                  </List.Content>

                </List.Item>
              </Message>
            )
            })
          }
        </List>
        :<Message>No Liquidity Found</Message>

  );
};

export default TokenPair;
