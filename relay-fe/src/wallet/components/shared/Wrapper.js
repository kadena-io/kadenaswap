import React from 'react';
import styled from 'styled-components/macro';
import { Container } from 'semantic-ui-react';

const Wrapper = styled(Container)`
  &.ui {
    @media only screen and (max-width: 767px) {
      width: 100% !important;
      margin-left: 0 !important;
      margin-right: 0 !important;
    }
  }
`;

export default ({ id, className, children, style }) => {
  return (
    <Wrapper id={id} className={className} fluid style={style}>
      {children}
    </Wrapper>
  );
};
