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


const TxView = ({ show, view, selectedToken, onTokenClick, onClose, token0, token1}) => {

  const pact = useContext(PactContext);

  const showTicker = (ticker) => {
    if (ticker === 'coin') return 'KDA'
    else return ticker.toUpperCase()
  }

  const successView = () => {
    return (
      <>
        <Message color='green'>
          <Label style={{ marginBottom: 4, color: 'purple'}}>Preview Successful!</Label>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'red' }}>Send</Label>
            <span style={{ color: 'red' }}>{`${pact.localRes.result.data[0].amount} ${showTicker(pact.localRes.result.data[0].token)}`}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4 }}>Receive</Label>
            <span>{`${pact.localRes.result.data[1].amount} ${showTicker(pact.localRes.result.data[1].token)}`}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'black' }}>Gas Cost</Label>
            <span style={{ color: 'black' }}>{`${pact.localRes.gas*0.00000000001} KDA`}</span>
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

  const successRemoveView = () => {
    return (
      <>
        <Message color='green'>
          <Label style={{ marginBottom: 4, color: 'purple'}}>Preview Successful!</Label>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'red' }}>Remove</Label>
            <span style={{ color: 'red' }}>{`${
              pact.localRes.result.data.amount0.decimal
                ? pact.localRes.result.data.amount0.decimal
                : pact.localRes.result.data.amount0
            }`} {showTicker(token0)}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'red' }}>Remove</Label>
            <span style={{ color: 'red' }}>{`${
              pact.localRes.result.data.amount1.decimal
                ? pact.localRes.result.data.amount1.decimal
                : pact.localRes.result.data.amount1
            }`} {showTicker(token1)}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'black' }}>Gas Cost</Label>
            <span style={{ color: 'black' }}>{`${pact.localRes.gas*0.00000000001} KDA`}</span>
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

  const successAddView = () => {
    return (
      <>
        <Message color='green'>
          <Label style={{ marginBottom: 4, color: 'purple'}}>Preview Successful!</Label>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'red' }}>Liquidity Balance</Label>
            <span style={{ color: 'red' }}>{`${  pact.localRes.result.data[0].amount.decimal
                ? pact.localRes.result.data.liquidity.amount.decimal
                :pact.localRes.result.data.liquidity.amount}`}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'red' }}>Add</Label>
            <span style={{ color: 'red' }}>{`${
              pact.localRes.result.data.amount0.decimal
                ? pact.localRes.result.data.amount0.decimal
                : pact.localRes.result.data.amount0
            }`} {showTicker(token0)}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'red' }}>Add</Label>
            <span style={{ color: 'red' }}>{
              pact.localRes.result.data.amount1.decimal
                ? pact.localRes.result.data.amount1.decimal
                : pact.localRes.result.data.amount1
            }} {showTicker(token1)}</span>
          </RowContainer>
          <RowContainer>
            <Label style={{ marginBottom: 4, color: 'black' }}>Gas Cost</Label>
            <span style={{ color: 'black' }}>{`${pact.localRes.gas*0.00000000001} KDA`}</span>
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
        <Message color='red' style={{wordBreak: "break-all"}}>
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

  const localError = () => {
    return (
      <>
        <Message color='red' style={{wordBreak: "break-all"}}>
          <Label style={{ marginBottom: 4, color: 'purple' }}>Local Failed!</Label>
          <RowContainer>
            <span style={{wordBreak: "break-all"}}>
              {pact.localRes}
            </span>
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
              {(typeof pact.localRes === 'string'
                ?
                  localError()
                :
                  (pact.localRes.result.status === 'success'
                  ?
                    !view
                    ? successView()
                    : view==="addLiquidity"
                      ? successAddView()
                      : successRemoveView()
                  :
                    failView()
                  )
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
