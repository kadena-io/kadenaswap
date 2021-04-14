import React from 'react';
import styled from 'styled-components/macro';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
  flex-wrap: nowrap;
  margin: 16px 0px;
`;

const Button = styled.button`
  cursor: pointer;
  display: flex;
  justify-content: center;
  align-items: center;
  border: none;
  border-radius: 100%;
  width: 32px;
  height: 32px;
  padding: 0;
  margin: 0;
  background: ${({ theme: { buttonBackgroundGradient } }) => buttonBackgroundGradient};
`;

const Track = styled.div`
  width: 86.5px;
  height: 1px;
  background-color: ${({ theme: { colors } }) => colors.border};
`;

const ButtonDivider = ({ icon, containerStyle, buttonStyle, onClick }) => {
  return (
    <Container style={containerStyle}>
      <Track />
      <Button style={buttonStyle} onClick={onClick}>
        {icon}
      </Button>
      <Track />
    </Container>
  );
};

export default ButtonDivider;
