/* eslint-disable react/jsx-props-no-spreading */
import React from 'react';
import styled from 'styled-components/macro';
import { Button as SUIButton } from 'semantic-ui-react';

const StyledButton = styled(SUIButton)`
  font-family: neue-bold !important;
  font-size: ${({ fontSize }) => (fontSize ? fontSize + ' !important' : '16px !important')};
  color: ${({ color }) => (color ? color + ' !important' : 'white !important')};
  background: ${({ disabled, background, theme: { buttonBackgroundGradient } }) => {
    if (background) return background + ' !important';
    if (disabled) return '#CDCDCD !important';
    return buttonBackgroundGradient;
  }};
  opacity: 1 !important;
`;

const Button = ({ props, disabled, buttonStyle, background, color, fontSize, children, onClick, loading }) => {
  return (
    <StyledButton
      {...props}
      disabled={disabled}
      background={background}
      color={color}
      fontSize={fontSize}
      style={buttonStyle}
      onClick={onClick}
      loading={loading}
    >
      {children}
    </StyledButton>
  );
};

export default Button;
