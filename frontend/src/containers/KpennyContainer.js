import React, { useEffect, useState, useContext } from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import Input from '../components/shared/Input';
import Button from '../components/shared/Button';
import KPTxView from '../components/shared/KPTxView';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { PactContext } from '../contexts/PactContext';
import {reduceBalance, extractDecimal} from '../utils/reduceBalance';
import { ReactComponent as CloseIcon } from '../assets/images/shared/cross.svg';
import pwError from '../components/alerts/pwError'

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const RowContainer = styled.div`
  display: flex;
  justify-content: space-between;
  margin-top: 15px;
`;

const Label = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
`;

const KpennyContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  const [amount, setAmount] = useState("");
  const [loading, setLoading] = useState(false);
  const [showTxModal, setShowTxModal] = useState(false);

  const getButtonLabel = () => {
    if (!pact.account.account) return 'Connect your KDA wallet';
    if (!pact.hasWallet()) return 'Set signing method in wallet';
    if (!amount) return 'Enter an amount';
    if (amount > pact.account.balance) return `Insufficient KDA balance`
    return 'RESERVE';
  };

  return (
    <Container>
      <KPTxView
        show={showTxModal}
        amtKda={amount}
        onClose={() => setShowTxModal(false)}
      />
      <FormContainer title="Reserve your Kpenny"  containerStyle={{ maxWidth: 500 }}>
        <Input
          // error={isNaN(fromValues.amount)}
          // leftLabel={`from ${fromNote}`}
          // rightLabel={`balance: ${reduceBalance(fromValues.balance) ?? '-'}`}
          placeholder="enter amount of KDA"
          numberOnly
          value={amount}
          onChange={async (e, { value }) => {
            setAmount(value)
          }}
        />
        <>
         <RowContainer>
           <Label>Rate</Label>
           <span>1000000 KPY per KDA</span>
         </RowContainer>
       </>
        {(getButtonLabel() === "RESERVE"
          ?
            <>
             <RowContainer>
               <Label>send</Label>
               <span>{amount} KDA</span>
              </RowContainer>
              <RowContainer>
               <Label>recieve</Label>
               <span>{amount * 1000000} KPY</span>
             </RowContainer>
           </>
          :
            <></>
        )}
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={getButtonLabel() !== "RESERVE" || isNaN(amount)}
          loading={loading}
          onClick={async () => {
            setLoading(true)
            if (pact.signing.method !== 'sign') {
              const res = await pact.kpennyReserveLocal(amount)
              if (res === -1) {
                setLoading(false)
                //error alert
                if (pact.localRes) pwError();
                return
              } else {
                setShowTxModal(true)
                setLoading(false)
              }
            } else {
              pact.kpennyReserveWallet(amount)
              setLoading(false)
            }
            setAmount("");
          }}
        >
          {getButtonLabel()}
        </Button>
      </FormContainer>
    </Container>
  );
};

export default KpennyContainer;
