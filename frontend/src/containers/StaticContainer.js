import React from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { PactContext } from '../contexts/PactContext';
import {reduceBalance, extractDecimal} from '../utils/reduceBalance';
import { ReactComponent as CloseIcon } from '../assets/images/shared/cross.svg';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const StaticContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  React.useEffect(async () => {
    await pact.getPairList()
  }, [])
  return (
    <Container>
      <FormContainer title="Get Started with Bountyswap"  containerStyle={{ maxWidth: 500 }}>
        <span style={{ marginLeft: 30, marginBottom: 20 }}>
          1. Get familiar with Bountyswap
          <a
            style={{ marginLeft: 3}}
            href="https://medium.com/kadena-io/introducing-kadenaswap-bountyswap-live-beta-ecde4cf014dc"
            target="_blank"
          >
            here
          </a>
        </span>
        <span style={{ marginLeft: 30, marginBottom: 20 }}>
          2. Download
          <a
            style={{ marginLeft: 3}}
            href="https://github.com/ZelCore-io/ZelCore/releases/tag/v4.9.0-beta-build-2615"
            target="_blank"
          >
            Zelcore Wallet
          </a>
        </span>
        <span style={{ marginLeft: 30, marginBottom: 20}}>
          3. Reserve
          <a
            style={{ marginLeft: 3}}
            href="https://kadenaswap.chainweb.com/kpenny-reserve"
            target="_blank"
          >
            KPenny
          </a>
        </span>
        <span style={{ marginLeft: 30, marginBottom: 20}}>
          4. Trade on
          <a
            style={{ marginLeft: 3}}
            href="https://kadenaswap.chainweb.com/"
            target="_blank"
          >
            Bountyswap
          </a>
        </span>
      </FormContainer>
    </Container>
  );
};

export default StaticContainer;
