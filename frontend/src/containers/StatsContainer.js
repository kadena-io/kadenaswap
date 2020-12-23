import React from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as EthereumLogo } from '../assets/images/crypto/ethereum.svg';
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
console.log(pairTokens[0].token0)
const FAKE_DATA = [
  {
    rank: 1,
    crypto1: <KadenaLogo></KadenaLogo>,
    crypto2: <KadenaLogo></KadenaLogo>,
    name: `${pairTokens[0].token0.code}-${pairTokens[0].token1.code}`,
    liquidity: 'US$149,079,004',
    totalSupply: 0
  },
  // {
  //   rank: 2,
  //   crypto1: <KadenaLogo></KadenaLogo>,
  //   crypto2: <KadenaLogo></KadenaLogo>,
  //   name: 'XYZ-KDA',
  //   liquidity: 'US$141,179,104',
  //   totalSupply: ''
  // }
];

const StatsContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  React.useEffect(async () => {
    await pact.getTotalTokenSupply(pairTokens[0].token0.name, pairTokens[0].token1.name);
    await pact.getReserves(pairTokens[0].token0.name, pairTokens[0].token1.name);
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
        {FAKE_DATA.map((d, i) => (
          <Row key={i}>
            <div style={{ marginLeft: 0,  flex: 0.2 }}>
              <IconsContainer>
                {d.crypto1}
                {d.crypto2}
              </IconsContainer>
            </div>
            <div  style={{ marginLeft: -10,width: 80 }}>{d.name}</div>
            <div style={{ marginLeft: -8, width: 80 }}>{reduceBalance(pact.pairReserve["token0"])}</div>
            <div style={{ marginLeft: 5, width: 20 }}>{reduceBalance(pact.pairReserve["token1"])}</div>
            <div style={{ marginLeft: 50, width: 50}}>{`${reduceBalance(pact.ratio)} ${pairTokens[0].token0.code}/${pairTokens[0].token1.code}`}</div>
          </Row>
        ))}
      </FormContainer>
    </Container>
  );
};

export default StatsContainer;
