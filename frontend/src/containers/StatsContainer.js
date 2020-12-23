import React from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { ReactComponent as EthereumLogo } from '../assets/images/crypto/ethereum.svg';
import { PactContext } from '../contexts/PactContext';

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

const FAKE_DATA = [
  {
    rank: 1,
    crypto1: <KadenaLogo></KadenaLogo>,
    crypto2: <KadenaLogo></KadenaLogo>,
    name: 'ABC-KDA',
    liquidity: 'US$149,079,004'
  },
  {
    rank: 2,
    crypto1: <KadenaLogo></KadenaLogo>,
    crypto2: <KadenaLogo></KadenaLogo>,
    name: 'XYZ-KDA',
    liquidity: 'US$141,179,104'
  }
];

const StatsContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  React.useEffect(() => {

  }, [])

  return (
    <Container>
      <FormContainer title="pool stats">
        <TitlesContainer>
          <span style={{ marginLeft: 40 }}>Name</span>
          <span>Liquidity</span>
        </TitlesContainer>
        {FAKE_DATA.map((d, i) => (
          <Row key={i}>
            <div style={{ marginLeft: 15, flex: 0.2 }}>
              <span>{d.rank}</span>
            </div>
            <div style={{ flex: 0.2 }}>
              <IconsContainer>
                {d.crypto1}
                {d.crypto2}
              </IconsContainer>
            </div>
            <div>{d.name}</div>
            <div style={{ marginLeft: 'auto' }}>{d.liquidity}</div>
          </Row>
        ))}
      </FormContainer>
    </Container>
  );
};

export default StatsContainer;
