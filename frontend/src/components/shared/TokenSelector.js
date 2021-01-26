import React, { useState, useContext } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import FormContainer from './FormContainer';
import Search from './Search';
import Backdrop from './Backdrop';
import { PactContext } from '../../contexts/PactContext'

const Container = styled.div`
  position: absolute;
  display: flex;
  justify-content: center;
  align-items: center;
  max-width: 385px;
  width: 100%;
  z-index: 5;
`;

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
`;

const Divider = styled.div`
  border: ${({ theme: { colors } }) => `1px solid ${colors.border}`};
  margin: 16px 0px;
  width: 100%;
`;

const TokensContainer = styled.div`
  display: flex;
  flex-flow: column;
  overflow: auto;

  & > div:not(:last-child) {
    margin-bottom: 16px;
  }
`;

const TokenItem = styled.div`
  cursor: pointer;
  display: flex;
  align-items: center;
  font-size: 16px;
  opacity: ${({ active }) => (active ? 0.3 : 1)};
  color: ${({ selected }) => (selected ? 'green' : '')};
  svg {
    margin-right: 8px;
    width: 24px;
    height: 24px;
  }
`;

const TokenSelector = ({ show, selectedToken, onTokenClick, onClose, fromToken, toToken }) => {
  const [searchValue, setSearchValue] = useState('');
  const pact = useContext(PactContext);

  return (
    <Transition items={show} from={{ opacity: 0 }} enter={{ opacity: 1 }} leave={{ opacity: 0 }}>
      {(show) =>
        show &&
        ((props) => (
          <Container style={props}>
            <Backdrop onClose={() => {
              setSearchValue('')
              onClose()
            }} />
            <FormContainer
              title="select a token"
              containerStyle={{ height: '100%', maxHeight: '80vh', maxWidth: '90vw' }}
              onClose={() => {
                setSearchValue('')
                onClose()
              }}
            >
              <Label style={{ marginBottom: 4 }}>search token</Label>
              <Search
                fluid
                containerStyle={{ marginBottom: 15 }}
                placeholder="Search"
                value={searchValue}
                onChange={(e, { value }) => setSearchValue(value)}
              />
              <Label>token</Label>
              <Divider />
              <TokensContainer>
                {Object.values(pact.tokenData)
                  .filter((c) => {
                    const code = (c.code !== 'coin' ? c.code.split('.')[1] : c.code)
                    return code.toLocaleLowerCase().includes(searchValue) || c.name.toLowerCase().includes(searchValue)
                  })
                  .map((crypto) => {
                    return(
                    <TokenItem
                      key={crypto.name}
                      active={selectedToken === crypto.name || fromToken === crypto.name || toToken === crypto.name}
                      // active={selectedToken === crypto.name}
                      selected={selectedToken === crypto.name}
                      style={{ cursor: selectedToken === crypto.name ? 'default' : 'pointer' }}
                      onClick={() => {
                        if (fromToken === crypto.name || toToken === crypto.name) return
                        if (selectedToken !== crypto.name) {
                          onTokenClick({ crypto });
                          setSearchValue('')
                          onClose();
                        }
                      }}
                    >
                      {crypto.icon}
                      {crypto.name}
                      {selectedToken === crypto.name
                        ?
                          <Label style={{ marginLeft: 5 }}>(SELECTED)</Label>
                        :
                          <></>
                      }
                      <span style={{ marginLeft: "auto", marginRight: 1, fontSize: 13 }}>{crypto.balance? `${crypto.balance} ${crypto.name}` : ""} </span>
                    </TokenItem>
                  )})}
              </TokensContainer>
            </FormContainer>
          </Container>
        ))
      }
    </Transition>
  );
};

export default TokenSelector;
