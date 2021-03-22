import React from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { Dimmer, Loader } from 'semantic-ui-react'
import { PactContext } from '../contexts/PactContext';
import {reduceBalance, extractDecimal} from '../utils/reduceBalance';
import { ReactComponent as CloseIcon } from '../assets/images/shared/cross.svg';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
`;

const TitlesContainer = styled.div`
  display: flex;
  justify-content: space-between;
  font-family: neue-bold;
  font-size: 14px;
  margin-bottom: 14px;
`;

const Row = styled.div`
  display: flex;
  align-items: center;
  border-top: 1px solid #ecebec;
  min-height: 44px;
`;

const IconsContainer = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-right: 21px;

  svg:first-child {
    z-index: 2;
  }

  svg:last-child {
    margin-left: 1px;
  }
`;

const StatsContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  React.useEffect(async () => {
    await pact.getPairList()
  }, [])

  return (
    <Container>
      <FormContainer title="pool stats"  containerStyle={{ maxWidth: 500 }}>
        <TitlesContainer>
          <span style={{ width: 120 }}>Name</span>
          <span style={{ width: 150 }}>Total Reserve - token0 </span>
          <span style={{ width: 150 }}>Total Reserve - token1</span>
          <span style={{ width: 80 }}>Rate</span>
        </TitlesContainer>
        {pact.pairList[0]
        ?
        Object.values(pact.pairList).map(pair => (
          pair&&pair.reserves ?
          <Row key={pair.name}>
            <div style={{ marginLeft: 0,  flex: 0.2 }}>
              <IconsContainer style={{ width:30 }}>
                {pact.tokenData[pair.token0].icon}
                {pact.tokenData[pair.token1].icon}
              </IconsContainer>
            </div>
            <div style={{ marginLeft: -10, width:30 }}>{`${pair.token0}/${pair.token1}`}</div>
            <div style={{marginLeft: 45, width:150 }}>{reduceBalance(pair.reserves[0])}</div>
            <div style={{marginLeft: 10, width:150 }}>{reduceBalance(pair.reserves[1])}</div>
            <div style={{marginLeft: 5}}>{`${reduceBalance(extractDecimal(pair.reserves[0])/extractDecimal(pair.reserves[1]))} ${pair.token0}/${pair.token1}`}</div>
          </Row>
          :""
        ))
        :
        <Dimmer active inverted>
          <Loader>Loading</Loader>
        </Dimmer>
      }
      </FormContainer>
    </Container>
  );
};

export default StatsContainer;
