import React, { useContext } from 'react';
import styled from 'styled-components/macro';
import { Transition } from 'react-spring/renderprops';
import { Message } from 'semantic-ui-react';
import FormContainer from './FormContainer';
import Search from './Search';
import Backdrop from './Backdrop';
import Button from './Button'
import cryptoCurrencies from '../../constants/cryptoCurrencies';
import { PactContext } from '../../contexts/PactContext';

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

const RowContainer = styled.div`
  display: flex;
  justify-content: space-between;
  margin: 15px 0px;
`;

const Divider = styled.div`
  border: ${({ theme: { colors } }) => `1px solid ${colors.border}`};
  margin: 16px 0px;
  width: 100%;
`;


const TxView = ({ show, selectedToken, onTokenClick, onClose }) => {

  const pact = useContext(PactContext);
  const successView = () => {
    return (
      <>
        <Message color='green'>
          <Label style={{ marginBottom: 4, color: 'purple'}}>Preview Successful!</Label>
          <RowContainer>
            <Label style={{ marginBottom: 4 }}>Send</Label>
            <span>{`${pact.localRes.result.data[0].amount} ${pact.localRes.result.data[0].token.toUpperCase()}`}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4 }}>Receive</Label>
            <span>{`${pact.localRes.result.data[1].amount} ${pact.localRes.result.data[1].token.toUpperCase()}`}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'black' }}>Gas Cost</Label>
            <span style={{ color: 'black' }}>0 KDA</span>
          </RowContainer>
        </Message>
        <Button
          onClick={async () => {
            pact.swapSend();
            onClose()
          }}
        >
          Send Transaction
        </Button>
      </>
    )
  }

  const failView = () => {
    return (
      <>
        <Message color='red'>
          <Label style={{ marginBottom: 4, color: 'purple' }}>Preview Failed!</Label>
          <RowContainer>
            <span>{pact.localRes.result.error.message}</span>
          </RowContainer>
        </Message>
        <Button
          onClick={() => {
            onClose()
          }}
        >
          Retry
        </Button>
      </>
    )
  }
  return (
    <Transition items={show} from={{ opacity: 0 }} enter={{ opacity: 1 }} leave={{ opacity: 0 }}>
      {(show) =>
        show &&
        ((props) => (
          <Container style={props}>
            <Backdrop onClose={onClose} />
            <FormContainer title="transaction details" containerStyle={{ height: '100%', maxHeight: '80vh', maxWidth: '90vw' }} onClose={onClose}>
              {(pact.localRes.result.status === 'success'
                ?
                  successView()
                :
                  failView()
                )}
              <Divider />
            </FormContainer>
          </Container>
        ))
      }
    </Transition>
  );
};

export default TxView;
