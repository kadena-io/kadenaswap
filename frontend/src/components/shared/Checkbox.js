import React from 'react';
import styled from 'styled-components/macro';
import { Checkbox as SUICheckbox } from 'semantic-ui-react';

const Container = styled.div`
  display: flex;
  align-items: center;

  .ui.toggle.checkbox input:checked ~ .box::before,
  .ui.toggle.checkbox input:checked ~ label::before {
    background-color: ${({ theme: { colors } }) => `${colors.primary} !important`};
  }
`;

const Checkbox = ({ toggle, checked, disabled, onChange }) => {
  return (
    <Container>
      <SUICheckbox checked={checked} disabled={disabled} toggle={toggle} onChange={onChange}></SUICheckbox>
    </Container>
  );
};

export default Checkbox;
