import React, { useState } from 'react';
import styled from 'styled-components/macro';
import { Modal, Header, Icon } from 'semantic-ui-react'
import Button from '../components/shared/Button';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import InputToken from '../components/shared/InputToken';
import MenuTabs from '../components/shared/MenuTabs';
import TokenSelector from '../components/shared/TokenSelector';
import cryptoCurrencies from '../constants/cryptoCurrencies';

import { ROUTE_INDEX, ROUTE_POOL, ROUTE_SWAP, ROUTE_WRAP } from '../router/routes';
import { NavLink, useHistory } from 'react-router-dom';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const WrapContainer = (props) => {
  const [activeItem, setActiveItem] = useState(0);
  const [showTokenSelector, setShowTokenSelector] = useState(false);
  const [selectedToken, setSelectedToken] = useState(null);
  const [amount, setAmount] = useState('');
  const [address, setAddress] = useState('');
  const [amountReceived, setAmountReceived] = useState('');

  const onTokenClick = ({ crypto }) => {
    setSelectedToken(crypto.code);
  };

  const [open, setOpen] = React.useState(true)

  return (
    <Container>
      <Modal
        basic
        onClose={() => {
          props.history.goBack()
          setOpen(false)
        }}
        onOpen={() => setOpen(true)}
        open={open}
        size='small'
      >
        <Header icon>
          <Icon name='time' />
          Coming soon!
        </Header>
        <Modal.Content>
            The bridge feature is coming to KadenaSwap soon! Be ready to wrap and unwrap your tokens
        </Modal.Content>
        <Modal.Actions>
          <Button>
            <NavLink to={ROUTE_SWAP} style={{color:'white'}}>
              <Icon name='checkmark' /> Got it
            </NavLink>
          </Button>
        </Modal.Actions>
      </Modal>


      <TokenSelector show={showTokenSelector} selectedToken={selectedToken} onTokenClick={onTokenClick} onClose={() => setShowTokenSelector(false)} />
      <FormContainer>
        <MenuTabs activeItem={activeItem} items={['wrap', 'unwrap']} onItemClick={(index) => setActiveItem(index)} />
        <Input
          leftLabel="input"
          placeholder="enter amount"
          inputRightComponent={
            selectedToken ? (
              <InputToken
                icon={cryptoCurrencies[selectedToken].icon}
                code={cryptoCurrencies[selectedToken].code}
                onClick={() => setShowTokenSelector(true)}
              />
            ) : null
          }
          containerStyle={{ marginBottom: 16 }}
          withSelectButton
          numberOnly
          value={amount}
          onSelectButtonClick={() => setShowTokenSelector(true)}
          onChange={(e, { value }) => setAmount(value)}
        />
        <Input
          leftLabel="destination"
          placeholder="enter address"
          containerStyle={{ marginBottom: 16 }}
          value={address}
          onChange={(e, { value }) => setAddress(value)}
        />
        <Input
          leftLabel="you will receive"
          placeholder="amount"
          value={amountReceived}
          disabled
          onChange={(e, { value }) => setAmountReceived(value)}
        />
        <Button buttonStyle={{ marginTop: 24 }} onClick={() => console.log('next')}>
          Next
        </Button>
      </FormContainer>
    </Container>
  );
};

export default WrapContainer;
