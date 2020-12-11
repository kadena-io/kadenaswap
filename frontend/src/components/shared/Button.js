/* eslint-disable react/jsx-props-no-spreading */
import React from 'react';
import styled from 'styled-components/macro';
import { Button as SUIButton } from 'semantic-ui-react';

const StyledButton = styled(SUIButton)`
  font-family: neue-bold !important;
  font-size: 16px !important;
  color: white !important;
  background: ${({ disabled, theme: { buttonBackgroundGradient } }) => (disabled ? '#CDCDCD !important' : buttonBackgroundGradient)};
  opacity: 1 !important;
`;

const Button = ({ props, disabled, buttonStyle, children, onClick, loading }) => {
  return (
    <StyledButton {...props} disabled={disabled} style={buttonStyle} onClick={onClick} loading={loading}>
      {children}
    </StyledButton>
  );
};

export default Button;
