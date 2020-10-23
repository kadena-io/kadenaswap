import React, { useState } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import FormContainer from './FormContainer';
import Search from './Search';
import Backdrop from './Backdrop';
import cryptoCurrencies from '../../constants/cryptoCurrencies';

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

  svg {
    margin-right: 8px;
    width: 24px;
    height: 24px;
  }
`;

const TokenSelector = ({ show, selectedToken, onTokenClick, onClose }) => {
  const [searchValue, setSearchValue] = useState('');

  return (
    <Transition items={show} from={{ opacity: 0 }} enter={{ opacity: 1 }} leave={{ opacity: 0 }}>
      {(show) =>
        show &&
        ((props) => (
          <Container style={props}>
            <Backdrop onClose={onClose} />
            <FormContainer title="select a token" containerStyle={{ height: '100%', maxHeight: '80vh', maxWidth: '90vw' }} onClose={onClose}>
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
                {Object.values(cryptoCurrencies)
                  .filter((c) => c.name.toLocaleLowerCase().includes(searchValue) || c.code.toLowerCase().includes(searchValue))
                  .map((crypto) => (
                    <TokenItem
                      key={crypto.code}
                      active={selectedToken === crypto.code}
                      style={{ cursor: selectedToken === crypto.code ? 'default' : 'pointer' }}
                      onClick={() => {
                        if (selectedToken !== crypto.code) {
                          onTokenClick({ crypto });
                          onClose();
                        }
                      }}
                    >
                      {crypto.icon}
                      {crypto.code}
                    </TokenItem>
                  ))}
              </TokensContainer>
            </FormContainer>
          </Container>
        ))
      }
    </Transition>
  );
};

export default TokenSelector;
