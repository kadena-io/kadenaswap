import React from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { PactContext } from '../contexts/PactContext';
import pairTokens from '../constants/pairTokens'
import reduceBalance from '../utils/reduceBalance';

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
      <FormContainer title="pool stats">
        <TitlesContainer>
          <span style={{ width: 100 }}>Name</span>
          <span style={{ width: 80 }}>Total Reserve - token0 </span>
          <span style={{ width: 80 }}>Total Reserve - token1</span>
          <span style={{ width: 80 }}>Rate</span>
        </TitlesContainer>
        {Object.values(pact.pairList).map(pair => (
          pair&&pair.reserves ?
          <Row key={pair.name}>
            <div style={{ marginLeft: 0,  flex: 0.2 }}>
              <IconsContainer>
                {pair.token0.icon}
                {pair.token1.icon}
              </IconsContainer>
            </div>
            <div  style={{ marginLeft: -10,width: 80 }}>{`${pair.token0.code}/${pair.token1.code}`}</div>
            <div style={{ marginLeft: -8, width: 80 }}>{reduceBalance(pair.reserves[0])}</div>
            <div style={{ marginLeft: 5, width: 20 }}>{reduceBalance(pair.reserves[1])}</div>
            <div style={{ marginLeft: 50, width: 50}}>{`${reduceBalance(reduceBalance(pair.reserves[1])/reduceBalance(pair.reserves[0]))} ${pair.token0.code}/${pair.token1.code}`}</div>
          </Row>
          :""
        ))}
      </FormContainer>
    </Container>
  );
};

export default StatsContainer;
