import React from 'react';
import styled from 'styled-components/macro';
import Wrapper from '../shared/Container';
import Header from './header/Header';

const MainContent = styled.div`
  /* margin-top: ${({ theme: { header } }) => `${header.height}px`}; */
  height: 100%;

  & > div:first-child {
    height: ${({ theme: { header } }) => `calc(100% - ${header.height}px)`};
    width: 100%;
  }
`;

const Layout = ({ children }) => {
  return (
    <Wrapper>
      <Header />
      <MainContent>{children}</MainContent>
    </Wrapper>
  );
};

export default Layout;
