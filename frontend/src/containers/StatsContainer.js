import React from 'react';
import styled from 'styled-components/macro';
import FormContainer from '../components/shared/FormContainer';
import { ReactComponent as KadenaLogo } from '../assets/images/crypto/kadena-logo.svg';
import { Dimmer, Loader, Table, Menu, Label } from 'semantic-ui-react'
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
  width: 80px;
  svg:first-child {
    z-index: 2;
  }
  div:last-child {
    margin-left: 5px;
  }
`;

const StatsContainer = ({ data }) => {

  const pact = React.useContext(PactContext);

  React.useEffect(async () => {
    await pact.getPairList()
  }, [])

  return (
    <Container>
      <FormContainer title="pool stats"  containerStyle={{ maxWidth: 650 }}>

        <Table celled basic='very'>
         <Table.Header>
           <Table.Row style={{fontFamily: "neue-bold"}}>
             <Table.HeaderCell textAlign='center'>Name</Table.HeaderCell>
             <Table.HeaderCell textAlign='center'>Total Reserve - <br/>  token0</Table.HeaderCell>
             <Table.HeaderCell textAlign='center'>Total Reserve - <br/>  token1</Table.HeaderCell>
             <Table.HeaderCell textAlign='center'>Rate</Table.HeaderCell>
           </Table.Row>
         </Table.Header>
         {
         pact.pairList[0]
         ?
         Object.values(pact.pairList).map(pair => (
           pair&&pair.reserves
           ?
           <Table.Body>
             <Table.Row key={pair.name}>
               <Table.Cell>
                 <IconsContainer>
                   {pact.tokenData[pair.token0].icon}
                   {pact.tokenData[pair.token1].icon}
                   <div>{`${pair.token0}/${pair.token1}`}</div>
                 </IconsContainer>
               </Table.Cell>
               <Table.Cell>{reduceBalance(pair.reserves[0])}</Table.Cell>
               <Table.Cell>{reduceBalance(pair.reserves[1])}</Table.Cell>
               <Table.Cell>{`${reduceBalance(extractDecimal(pair.reserves[0])/extractDecimal(pair.reserves[1]))} ${pair.token0}/${pair.token1}`}</Table.Cell>
             </Table.Row>
           </Table.Body>
           :""
         ))
         :
         <Dimmer active inverted>
           <Loader>Loading</Loader>
         </Dimmer>
         }
       </Table>
     </FormContainer>
    </Container>
  );
};

export default StatsContainer;
