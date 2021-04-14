import React from 'react';
import styled from 'styled-components/macro';
import PropTypes from 'prop-types';
import { Container as SUIContainer } from 'semantic-ui-react';

const Container = styled(SUIContainer)`
  display: flex;
  margin: 0 auto;
  width: ${({ theme: { layout } }) => layout.mobileWidth};
  height: ${({ height }) => (height ? `${height}` : 'auto')};
  padding-left: 1em;
  padding-right: 1em;

  @media (min-width: ${({ theme: { mediaQueries } }) => mediaQueries.mobileBreakpoint}) {
    width: ${({ theme: { layout } }) => layout.desktopWidth};
  }
`;

const Wrapper = ({ children, id, className, height, style }) => {
  return (
    <Container id={id} className={className} height={height} style={style}>
      {children}
    </Container>
  );
};

Wrapper.propTypes = {
  height: PropTypes.string
};

Wrapper.defaultProps = {
  height: 'auto'
};

export default Wrapper;
