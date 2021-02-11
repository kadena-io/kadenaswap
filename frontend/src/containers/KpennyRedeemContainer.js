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

const KpennyRedeemContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  const [balance, setBalance] = useState(0);
  const [amount, setAmount] = useState("");
  const [loading, setLoading] = useState(false);
  const [showTxModal, setShowTxModal] = useState(false);

  useEffect(() => {
    if (pact.tokenData["KPY"].balance) setBalance(pact.tokenData["KPY"].balance);
  }, [pact])

  const getButtonLabel = () => {
    if (!pact.account.account) return 'Connect your KDA wallet';
    if (!pact.hasWallet()) return 'Set signing method in wallet';
    if (balance === 0) return `You Don't own KPY`
    return 'REDEEM';
  };

  return (
    <Container>
      <KPTxView
        show={showTxModal}
        amtKda={(balance ? balance / 1000000 : 0)}
        onClose={() => setShowTxModal(false)}
        isRedeem={true}
      />
      <FormContainer title="Redeem from Kpenny"  containerStyle={{ maxWidth: 500 }}>
        <>
          <RowContainer>
            <Label>Balance</Label>
            <span>{balance} KPY</span>
          </RowContainer>
         <RowContainer>
           <Label>Rate</Label>
           <span>0.0000001 KDA per KPY</span>
         </RowContainer>
         <RowContainer>
           <Label>Recieve</Label>
           <span>{balance / 1000000} KDA</span>
         </RowContainer>
       </>
        <Button
          buttonStyle={{ marginTop: 24, marginRight: 0 }}
          disabled={getButtonLabel() !== "REDEEM"}
          loading={loading}
          onClick={async () => {
            setLoading(true)
            if (pact.signing.method !== 'sign') {
              const res = await pact.kpennyRedeemLocal()
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
              pact.kpennyRedeemWallet()
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

export default KpennyRedeemContainer;
