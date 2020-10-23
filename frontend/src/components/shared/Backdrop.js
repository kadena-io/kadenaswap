import React from 'react';
import styled from 'styled-components/macro';

const Container = styled.div`
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: rgba(0, 0, 0, 0.5);
  transition: background-color 0.3s ease-in;
  z-index: ${({ isSideDrawerOpened }) => (isSideDrawerOpened ? 14 : -10)};
`;

const Backdrop = ({ onClose }) => {
  return <Container onClick={onClose} />;
};

export default Backdrop;
