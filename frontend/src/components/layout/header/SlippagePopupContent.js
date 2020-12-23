import React, { useContext, useState } from 'react';
import styled from 'styled-components';
import { PactContext } from '../../../contexts/PactContext';
import theme from '../../../styles/theme';
import Input from '../../shared/Input';

const Container = styled.div`
  display: flex;
  flex-direction: column;
`;

const BoldLabel = styled.span`
  font-size: 13px;
  font-family: neue-bold;
  text-transform: capitalize;
`;

const RegularLabel = styled.span`
  font-size: 13px;
  font-family: neue-regular;
  text-transform: capitalize;
  color: #b5b5b5;
`;

const SlippageTolleranceValue = styled.div`
  border-radius: 16px;
  border: ${({ theme: { colors } }) => `1px solid ${colors.border}`};
  color: ${({ isSelected, theme: { colors } }) => (isSelected ? '#ffffff' : colors.primary)};
  font-family: neue-regular;
  font-size: 14px;
  padding: 6.5px 8.5px;
  min-width: 48px;
  display: flex;
  justify-content: center;
  background-image: ${({ isSelected }) => (isSelected ? 'linear-gradient(to top right, #ed098f 0%,  #7a0196 100%)' : '#ffffff')};
  cursor: pointer;
`;

const ContainerInputTypeNumber = styled.div`
  display: flex;
  align-items: center;
  padding: 6.5px 8.5px;
  border-radius: 16px;
  border: ${({ theme: { colors } }) => `1px solid ${colors.border}`};

  .ui.input > input {
    border: unset;
    padding: 0px;
    text-align: right;
    font-size: 14px;
  }
  .ui.fluid.input > input {
    width: 80px !important;
  }
`;

const Row = styled.div`
  display: flex;
  align-items: center;
`;

const SlippagePopupContent = () => {
  const pact = useContext(PactContext);
  return (
    <Container>
      <BoldLabel>Transactions Settings</BoldLabel>
      <RegularLabel style={{ marginTop: 16 }}>Slippage Tolerance</RegularLabel>

      <Row style={{ marginTop: 8 }}>
        <SlippageTolleranceValue isSelected={Number(pact.slippage) === 0.1/100} onClick={() => pact.storeSlippage(0.1/100)}>
          0.1%
        </SlippageTolleranceValue>
        <SlippageTolleranceValue
          isSelected={Number(pact.slippage) === 0.5/100}
          style={{ marginLeft: 4, marginRight: 4 }}
          onClick={() => pact.storeSlippage(0.5/100)}
        >
          0.5%
        </SlippageTolleranceValue>
        <SlippageTolleranceValue isSelected={Number(pact.slippage) === 1/100} style={{ marginRight: 8 }} onClick={() => pact.storeSlippage(1/100)}>
          1%
        </SlippageTolleranceValue>

        <ContainerInputTypeNumber>
          <Input
            placeholder={pact.slippage*100}
            numberOnly
            value={pact.slippage*100}
            onChange={(e, { value }) => {
              pact.storeSlippage(value/100)
            }}
          />
          %
        </ContainerInputTypeNumber>
      </Row>

      <RegularLabel style={{ marginTop: 16 }}>Transaction deadline</RegularLabel>
      <Row style={{ marginTop: 8 }}>
        <ContainerInputTypeNumber>
          <Input
            placeholder={pact.ttl}
            disabled
            numberOnly
            value={(pact.ttl/60)}
            onChange={(e, { value }) => {
              pact.setTtl(Number(value));
            }}
          />
        </ContainerInputTypeNumber>
        <RegularLabel style={{ color: theme.colors.primary, marginLeft: 8 }}>minutes</RegularLabel>
      </Row>
    </Container>
  );
};

export default SlippagePopupContent;
